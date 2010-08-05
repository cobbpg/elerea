{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

This version differs from the simple one in adding associated freeze
control signals (\'clocks\') to stateful entities to be able to pause
entire subnetworks without having to write all the low-level logic
explicitly.  The clocks are fixed to signals upon their creation, and
the 'withClock' function can be used to specify the common clock for
the signals created in a given generator.

A clock signal affects 'delay' elements the following way: if the
clock signal is true, the delay works as usual, otherwise it remembers
its current output and throws away its current input.  If we consider
signals to be functions of time (natural numbers), the behaviour of
delay can be described by the following function:

@
 delay x0 s (t_start,clk) t_sample
   | t_start == t_sample = x0
   | t_start \< t_sample  = if clk t_sample
                             then s (t_sample-1)
                             else delay x0 s (t_start (t_sample-1)
   | otherwise           = error \"stream doesn't exist yet\"
@

A simple example to create counters operating at different rates using
the same generator:

@
 divisibleBy n x = x \`mod\` n == 0
@

@
 counter = stateful 0 (+1)
@

@
 drift = do
   time \<- counter
   c1 \<- withClock (divisibleBy 2 \<$\> time) counter
   c2 \<- withClock (divisibleBy 3 \<$\> time) counter
   return ((,) \<$\> c1 \<*\> c2)
@

Note that if you want to slow down the drift system defined above, the
naive approach might lead to surprising results:

@
 slowDrift = do
   time \<- counter
   withClock (divisibleBy 2 \<$\> time) drift
@

The problem is that the clocks are also slowed down, and their spikes
double in length.  This may or may not be what you want.  To overcome
this problem, we can define a clock oblivious edge detector to be used
within the definition of @drift@:

@
 edge = withClock (pure True) . transfer False (\\b b' -> b && not b')
@

@
 drift = do
   time \<- counter
   t2 \<- edge (divisibleBy 2 \<$\> time)
   t3 \<- edge (divisibleBy 3 \<$\> time)
   c1 \<- withClock t2 counter
   c2 \<- withClock t3 counter
   return ((,) \<$\> c1 \<*\> c2)
@

This works because the 'withClock' function overrides any clock
imposed on the generator from outside.

-}

module FRP.Elerea.Clocked
    ( Signal
    , SignalGen
    , start
    , external
    , externalMulti
    , delay
    , generator
    , memo
    , until
    , withClock
    , stateful
    , transfer
    , noise
    , getRandom
    , debug
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Maybe
import Prelude hiding (until)
import System.Mem.Weak
import System.Random.Mersenne

-- | A signal can be thought of as a function of type @Nat -> a@,
-- where the argument is the sampling time, and the 'Monad' instance
-- agrees with the intuition (bind corresponds to extracting the
-- current sample).
newtype Signal a = S (IO a) deriving (Functor, Applicative, Monad)

-- | A dynamic set of actions to update a network without breaking
-- consistency.
type UpdatePool = [Weak (IO (),IO ())]

-- | A signal generator is the only source of stateful signals.  It
-- can be thought of as a function of type @Nat -> a@, where the
-- result is an arbitrary data structure that can potentially contain
-- new signals, and the argument is the creation time of these new
-- signals.  It exposes the 'MonadFix' interface, which makes it
-- possible to define signals in terms of each other.
newtype SignalGen a = SG { unSG :: IORef UpdatePool -> Signal Bool -> IO a }

-- | The phases every signal goes through during a superstep.
data Phase a = Ready a | Updated a a

instance Functor SignalGen where
  fmap = (<*>).pure

instance Applicative SignalGen where
  pure = return
  (<*>) = ap

instance Monad SignalGen where
  return = SG . const . const . return
  SG g >>= f = SG $ \p c -> g p c >>= \x -> unSG (f x) p c

instance MonadFix SignalGen where
  mfix f = SG $ \p c -> mfix (($c).($p).unSG.f)

-- | Embedding a signal into an 'IO' environment.  Repeated calls to
-- the computation returned cause the whole network to be updated, and
-- the current sample of the top-level signal is produced as a
-- result. This is the only way to extract a signal generator outside
-- the network, and it is equivalent to passing zero to the function
-- representing the generator.
start :: SignalGen (Signal a) -- ^ the generator of the top-level signal
      -> IO (IO a)            -- ^ the computation to sample the signal
start (SG gen) = do
  pool <- newIORef []
  S sample <- gen pool (pure True)
  return $ do
    let deref ptr = (fmap.fmap) ((,) ptr) (deRefWeak ptr)
    res <- sample
    (ptrs,acts) <- unzip.catMaybes <$> (mapM deref =<< readIORef pool)
    writeIORef pool ptrs
    mapM_ fst acts
    mapM_ snd acts
    return res

-- | Auxiliary function used by all the primitives that create a
-- mutable variable.
addSignal :: (a -> IO a)      -- ^ sampling function
          -> (a -> IO ())     -- ^ aging function
          -> IORef (Phase a)  -- ^ the mutable variable behind the signal
          -> IORef UpdatePool -- ^ the pool of update actions
          -> IO (Signal a)    -- ^ the signal created
addSignal sample update ref pool = do
  let  upd = readIORef ref >>= \v -> case v of
               Ready x  -> update x
               _        -> return ()

       fin = readIORef ref >>= \v -> case v of
               Updated x _  -> writeIORef ref $! Ready x
               _            -> error "Signal not updated!"

       sig = S $ readIORef ref >>= \v -> case v of
               Ready x      -> sample x
               Updated _ x  -> return x

  updateActions <- mkWeak sig (upd,fin) Nothing
  modifyIORef pool (updateActions:)
  return sig

-- | The 'delay' transfer function emits the value of a signal from
-- the previous superstep, starting with the filler value given in the
-- first argument.  It can be thought of as the following function
-- (which should also make it clear why the return value is
-- 'SignalGen'):
--
-- @
--  delay x0 s t_start t_sample
--    | t_start == t_sample = x0
--    | t_start < t_sample  = s (t_sample-1)
--    | otherwise           = error \"Premature sample!\"
-- @
--
-- The way signal generators are extracted ensures that the error can
-- never happen.
delay :: a                    -- ^ initial output at creation time
      -> Signal a             -- ^ the signal to delay
      -> SignalGen (Signal a) -- ^ the delayed signal
delay x0 (S s) = SG $ \pool (S clk) -> do
  ref <- newIORef (Ready x0)

  let update x = do  x' <- s
                     c <- clk
                     x' `seq` writeIORef ref (Updated (if c then x' else x) x)

  addSignal return update ref pool

-- | A reactive signal that takes the value to output from a signal
-- generator carried by its input with the sampling time provided as
-- the time of generation.  It is possible to create new signals in
-- the monad.  It can be thought of as the following function:
--
-- @
--  generator g t_start t_sample = g t_sample t_sample
-- @
--
-- It has to live in the 'SignalGen' monad, because it needs to
-- maintain an internal state to be able to cache the current sample
-- for efficiency reasons. However, this state is not carried between
-- samples, therefore starting time doesn't matter and can be ignored.
generator :: Signal (SignalGen a) -- ^ the signal of generators to run
          -> SignalGen (Signal a) -- ^ the signal of generated structures
generator (S s) = SG $ \pool clk -> do
  ref <- newIORef (Ready undefined)

  let sample = do  SG g <- s
                   x <- g pool clk
                   writeIORef ref (Updated undefined x)
                   return x

  addSignal (const sample) (const (sample >> return ())) ref pool

-- | Override the clock used in a generator.  Note that clocks don't
-- interact unless one is used in the definition of the other, i.e. it
-- is possible to provide a fast clock within a generator with a slow
-- associated clock.
withClock :: Signal Bool -> SignalGen a -> SignalGen a
withClock clk (SG g) = SG $ \pool _ -> g pool clk

-- | Memoising combinator.  It can be used to cache results of
-- applicative combinators in case they are used in several places.
-- It is observationally equivalent to 'return' in the 'SignalGen'
-- monad.
memo :: Signal a             -- ^ the signal to cache
     -> SignalGen (Signal a) -- ^ a signal observationally equivalent to the argument
memo (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = s >>= \x -> writeIORef ref (Updated undefined x) >> return x

  addSignal (const sample) (const (sample >> return ())) ref pool

-- | A signal that is true exactly once: the first time the input
-- signal is true.  Afterwards, it is constantly false, and it holds
-- no reference to the input signal.  Note that 'until' always follows
-- the master clock, i.e. the fastest one, therefore it never creates
-- a long spike of @True@.
until :: Signal Bool             -- ^ the boolean input signal
      -> SignalGen (Signal Bool) -- ^ a one-shot signal true only the first time the input is true
until (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  rsmp <- mfix $ \rs -> newIORef $ do
    x <- s
    writeIORef ref (Updated undefined x)
    when x $ writeIORef rs $ do
      writeIORef ref (Updated undefined False)
      return False
    return x

  let sample = join (readIORef rsmp)

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | A signal that can be directly fed through the sink function
-- returned.  This can be used to attach the network to the outer
-- world.
external :: a                         -- ^ initial value
         -> IO (Signal a, a -> IO ()) -- ^ the signal and an IO function to feed it
external x = do
  ref <- newIORef x
  return (S (readIORef ref), writeIORef ref)

-- | An event-like signal that can be fed through the sink function
-- returned.  The signal carries a list of values fed in since the
-- last sampling, i.e. it is constantly [] if the sink is never
-- invoked.  The order of elements is reversed, so the last value
-- passed to the sink is the head of the list.  Note that unlike
-- 'external' this function only returns a generator to be used within
-- the expression constructing the top-level stream, and this
-- generator can only be used once.
externalMulti :: IO (SignalGen (Signal [a]), a -> IO ()) -- ^ a generator for the event signal and the associated sink
externalMulti = do
  var <- newMVar []
  return (SG $ \pool _ -> do
             let sig = S $ readMVar var
             update <- mkWeak sig (return (),takeMVar var >> putMVar var []) Nothing
             modifyIORef pool (update:)
             return sig
         ,\val -> do  vals <- takeMVar var
                      putMVar var (val:vals)
         )

-- | A pure stateful signal.  The initial state is the first output,
-- and every subsequent state is derived from the preceding one by
-- applying a pure transformation.  It is equivalent to the following
-- expression:
--
-- @
--  stateful x0 f = 'mfix' $ \sig -> 'delay' x0 (f '<$>' sig)
-- @
stateful :: a                    -- ^ initial state
         -> (a -> a)             -- ^ state transformation
         -> SignalGen (Signal a)
stateful x0 f = mfix $ \sig -> delay x0 (f <$> sig)

-- | A stateful transfer function.  The current input affects the
-- current output, i.e. the initial state given in the first argument
-- is considered to appear before the first output, and can never be
-- observed, and subsequent states are determined by combining the
-- preceding state with the current output of the input signal using
-- the function supplied.  It is equivalent to the following
-- expression:
--
-- @
--  transfer x0 f s = 'mfix' $ \sig -> 'liftA2' f s '<$>' 'delay' x0 sig
-- @
transfer :: a                    -- ^ initial internal state
         -> (t -> a -> a)        -- ^ state updater function
         -> Signal t             -- ^ input signal
         -> SignalGen (Signal a)
transfer x0 f s = mfix $ \sig -> liftA2 f s <$> delay x0 sig

-- | A random signal.
noise :: MTRandom a => SignalGen (Signal a)
noise = memo (S randomIO)

-- | A random source within the 'SignalGen' monad.
getRandom :: MTRandom a => SignalGen a
getRandom = SG (const (const randomIO))

-- | A printing action within the 'SignalGen' monad.
debug :: String -> SignalGen ()
debug = SG . const . const . putStrLn

-- The Show instance is only defined for the sake of Num...
instance Show (Signal a) where
  showsPrec _ _ s = "<SIGNAL>" ++ s

-- Equality test is impossible.
instance Eq (Signal a) where
  _ == _ = False

-- Error message for unimplemented instance functions.
unimp :: String -> a
unimp = error . ("Signal: "++)

instance Ord t => Ord (Signal t) where
  compare = unimp "compare"
  min = liftA2 min
  max = liftA2 max

instance Enum t => Enum (Signal t) where
  succ = fmap succ
  pred = fmap pred
  toEnum = pure . toEnum
  fromEnum = unimp "fromEnum"
  enumFrom = unimp "enumFrom"
  enumFromThen = unimp "enumFromThen"
  enumFromTo = unimp "enumFromTo"
  enumFromThenTo = unimp "enumFromThenTo"

instance Bounded t => Bounded (Signal t) where
  minBound = pure minBound
  maxBound = pure maxBound

instance Num t => Num (Signal t) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  signum = fmap signum
  abs = fmap abs
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Real t => Real (Signal t) where
  toRational = unimp "toRational"

instance Integral t => Integral (Signal t) where
  quot = liftA2 quot
  rem = liftA2 rem
  div = liftA2 div
  mod = liftA2 mod
  quotRem a b = (fst <$> qrab,snd <$> qrab)
    where qrab = quotRem <$> a <*> b
  divMod a b = (fst <$> dmab,snd <$> dmab)
    where dmab = divMod <$> a <*> b
  toInteger = unimp "toInteger"

instance Fractional t => Fractional (Signal t) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating t => Floating (Signal t) where
  pi = pure pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh
