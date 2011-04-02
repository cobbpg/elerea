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

> delay x0 s (t_start,clk) t_sample
>   | t_start == t_sample = x0
>   | t_start < t_sample  = if clk t_sample
>                             then s (t_sample-1)
>                             else delay x0 s t_start s_clock (t_sample-1)
>   | otherwise           = error "stream doesn't exist yet"

A simple example to create counters operating at different rates using
the same generator:

> divisibleBy n x = x `mod` n == 0
>
> counter = stateful 0 (+1)
>
> drift = do
>   time <- counter
>   c1 <- withClock (divisibleBy 2 <$> time) counter
>   c2 <- withClock (divisibleBy 3 <$> time) counter
>   return ((,) <$> c1 <*> c2)

Note that if you want to slow down the drift system defined above, the
naive approach might lead to surprising results:

> slowDrift = do
>   time <- counter
>   withClock (divisibleBy 2 <$> time) drift

The problem is that the clocks are also slowed down, and their spikes
double in length.  This may or may not be what you want.  To overcome
this problem, we can define a clock oblivious edge detector to be used
within the definition of @drift@:

> edge = withClock (pure True) . transfer False (\b b' -> b && not b')
>
> drift = do
>   time <- counter
>   t2 <- edge (divisibleBy 2 <$> time)
>   t3 <- edge (divisibleBy 3 <$> time)
>   c1 <- withClock t2 counter
>   c2 <- withClock t3 counter
>   return ((,) <$> c1 <*> c2)

This works because the 'withClock' function overrides any clock
imposed on the generator from outside.

-}

module FRP.Elerea.Clocked
    (
    -- * The signal abstraction
      Signal
    , SignalGen
    -- * Embedding into I/O
    , start
    , external
    , externalMulti
    , debug
    -- * Basic building blocks
    , delay
    , generator
    , memo
    , until
    , withClock
    -- * Derived combinators
    , stateful
    --, transfer
    -- * Random sources
    , noise
    , getRandom
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

-- | A signal represents a value changing over time.  It can be
-- thought of as a function of type @Nat -> a@, where the argument is
-- the sampling time, and the 'Monad' instance agrees with the
-- intuition (bind corresponds to extracting the current sample).
-- Signals and the values they carry are denoted the following way in
-- the documentation:
--
-- > s = <<s0 s1 s2 ...>>
--
-- This says that @s@ is a signal that reads @s0@ during the first
-- sampling, @s1@ during the second and so on.  You can also think of
-- @s@ as the following function:
--
-- > s t_sample = [s0,s1,s2,...] !! t_sample
--
-- Signals are constrained to be sampled sequentially, there is no
-- random access.  The only way to observe their output is through
-- 'start'.
newtype Signal a = S (IO a) deriving (Functor, Applicative, Monad)

-- | A dynamic set of actions to update a network without breaking
-- consistency.
type UpdatePool = [Weak (IO (),IO ())]

-- | A signal generator is the only source of stateful signals.  It
-- can be thought of as a function of type @Nat -> a@, where the
-- result is an arbitrary data structure that can potentially contain
-- new signals, and the argument is the creation time of these new
-- signals.  It exposes the 'MonadFix' interface, which makes it
-- possible to define signals in terms of each other.  Unlike the
-- simple variant, the denotation of signal generators differs from
-- that of signals.  We will use the following notation for
-- generators:
--
-- > g = <|g0 g1 g2 ...|>
--
-- Just like signals, generators behave as functions of time, but they
-- can also refer to the clock signal:
--
-- > g t_start s_clock = [g0,g1,g2,...] !! t_start
--
-- The conceptual difference between the two notions is that signals
-- are passed a sampling time, while generators expect a start time
-- that will be the creation time of all the freshly generated
-- signals in the resulting structure.
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
-- representing the generator.  The clock associated with the
-- top-level signal ticks at every sampling point.  In general:
--
-- > replicateM n =<< start <|<<x0 x1 x2 x3 ...>> ...|> == take n [x0,x1,x2,x3,...]
--
-- Example:
--
-- > do
-- >     smp <- start (stateful 3 (+2))
-- >     res <- replicateM 5 smp
-- >     print res
--
-- Output:
--
-- > [3,5,7,9,11]
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

-- | The 'delay' combinator is the elementary building block for
-- adding state to the signal network by constructing delayed versions
-- of a signal that emit a given value at creation time and the
-- previous output of the signal afterwards.
--
-- The clock signal associated to the generator affects 'delay'
-- elements the following way: if the clock signal is true, the delay
-- works as usual, otherwise it remembers its current output and
-- throws away its current input.  If we consider signals to be
-- functions of time (natural numbers), the behaviour of delay can be
-- described by the following function:
--
-- > delay x0 s t_start s_clock t_sample
-- >   | t_start == t_sample = x0
-- >   | t_start < t_sample  = if s_clock t_sample
-- >                             then s (t_sample-1)
-- >                             else delay x0 s t_start s_clock (t_sample-1)
-- >   | otherwise           = error "stream doesn't exist yet"
--
-- The way signal generators are extracted by 'generator' ensures that
-- the error can never happen.
--
-- Example (requires the @DoRec@ extension):
--
-- > do
-- >     smp <- start $ do
-- >         rec let fib'' = liftA2 (+) fib' fib
-- >             fib' <- delay 1 fib''
-- >             fib <- delay 1 fib'
-- >         return fib
-- >     res <- replicateM 7 smp
-- >     print res
--
-- Output:
--
-- > [1,1,2,3,5,8,13]
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
-- the start time for the generated structure.  It is possible to
-- create new signals in the monad, which is the key to defining
-- dynamic data-flow networks.
--
-- > generator << <|x00 x01 x02 ...|>
-- >              <|x10 x11 x12 ...|>
-- >              <|x20 x21 x22 ...|>
-- >              ...
-- >           >> = <| <<x00 x11 x22 ...>>
-- >                   <<x00 x11 x22 ...>>
-- >                   <<x00 x11 x22 ...>>
-- >                   ...
-- >                |>
--
-- It can be thought of as the following function:
--
-- > generator g t_start s_clock t_sample = g t_sample t_sample s_clock
--
-- It has to live in the 'SignalGen' monad, because it needs to
-- maintain an internal state to be able to cache the current sample
-- for efficiency reasons. However, this state is not carried between
-- samples, therefore start time doesn't matter and can be ignored.
-- Also, even though it does not make use of the clock itself, part of
-- its job is to distribute it among the newly generated signals.
--
-- Refer to the longer example at the bottom of "FRP.Elerea.Simple" to
-- see how it can be used.
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
-- associated clock.  It is equivalent to the following function:
--
-- > withClock s g t_start s_clock = g t_start s
--
-- For instance, the following equivalence holds:
--
-- > withClock (pure False) (stateful x f) == pure x
withClock :: Signal Bool -> SignalGen a -> SignalGen a
withClock clk (SG g) = SG $ \pool _ -> g pool clk

-- | Memoising combinator.  It can be used to cache results of
-- applicative combinators in case they are used in several places.
-- It is observationally equivalent to 'return' in the 'SignalGen'
-- monad.
--
-- > memo s = <|s s s s ...|>
--
-- For instance, if @s = f \<$\> s'@, then @f@ will be recalculated
-- once for each sampling of @s@.  This can be avoided by writing @s
-- \<- memo (f \<$\> s')@ instead.  However, 'memo' incurs a small
-- overhead, therefore it should not be used blindly.
--
-- All the functions defined in this module return memoised signals.
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
-- a long spike of @True@.  For instance (assuming the rest of the
-- input is constantly @False@):
--
-- > until <<False False True True False True ...>> =
-- >     <| <<False False True  False False False False False False False ...>>
-- >        << ---  False True  False False False False False False False ...>>
-- >        << ---   ---  True  False False False False False False False ...>>
-- >        << ---   ---   ---  True  False False False False False False ...>>
-- >        << ---   ---   ---   ---  False True  False False False False ...>>
-- >        << ---   ---   ---   ---   ---  True  False False False False ...>>
-- >        << ---   ---   ---   ---   ---   ---  False False False False ...>>
-- >        ...
-- >     |>
--
-- It is observationally equivalent to the following expression (which
-- would hold onto @s@ forever):
--
-- > until s = withClock (pure True) $ do
-- >     step <- transfer False (||) s
-- >     dstep <- delay False step
-- >     memo (liftA2 (/=) step dstep)
--
-- Example:
--
-- > do
-- >     smp <- start $ do
-- >         cnt <- stateful 0 (+1)
-- >         tick <- until ((>=3) <$> cnt)
-- >         return $ liftA2 (,) cnt tick
-- >     res <- replicateM 6 smp
-- >     print res
--
-- Output:
--
-- > [(0,False),(1,False),(2,False),(3,True),(4,False),(5,False)]
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
-- world.  The signal always yields the value last written to the
-- sink.  In other words, if the sink is written less frequently than
-- the network sampled, the output remains the same during several
-- samples.  If values are pushed in the sink more frequently, only
-- the last one before sampling is visible on the output.
--
-- Example:
--
-- > do
-- >     (sig,snk) <- external 4
-- >     smp <- start (return sig)
-- >     r1 <- smp
-- >     r2 <- smp
-- >     snk 7
-- >     r3 <- smp
-- >     snk 9
-- >     snk 2
-- >     r4 <- smp
-- >     print [r1,r2,r3,r4]
--
-- Output:
--
-- > [4,4,7,2]
external :: a                         -- ^ initial value
         -> IO (Signal a, a -> IO ()) -- ^ the signal and an IO function to feed it
external x = do
  ref <- newIORef x
  return (S (readIORef ref), writeIORef ref)

-- | An event-like signal that can be fed through the sink function
-- returned.  The signal carries a list of values fed in since the
-- last sampling, i.e. it is constantly @[]@ if the sink is never
-- invoked.  The order of elements is reversed, so the last value
-- passed to the sink is the head of the list.  Note that unlike
-- 'external' this function only returns a generator to be used within
-- the expression constructing the top-level stream, and this
-- generator can only be used once.
--
-- Example:
--
-- > do
-- >     (gen,snk) <- externalMulti
-- >     smp <- start gen
-- >     r1 <- smp
-- >     snk 7
-- >     r2 <- smp
-- >     r3 <- smp
-- >     snk 9
-- >     snk 2
-- >     r4 <- smp
-- >     print [r1,r2,r3,r4]
--
-- Output:
--
-- > [[],[7],[],[2,9]]
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
-- applying a pure transformation.  It is affected by the associated
-- clock like 'delay': no transformation is performed in the absence
-- of a tick; see the example at the top.
--
-- Example:
--
-- > do
-- >     smp <- start (stateful "x" ('x':))
-- >     res <- replicateM 5 smp
-- >     print res
--
-- Output:
--
-- > ["x","xx","xxx","xxxx","xxxxx"]
stateful :: a                    -- ^ initial state
         -> (a -> a)             -- ^ state transformation
         -> SignalGen (Signal a)
stateful x0 f = mfix $ \sig -> delay x0 (f <$> sig)

{-

transfer :: a                    -- ^ initial internal state
         -> (t -> a -> a)        -- ^ state updater function
         -> Signal t             -- ^ input signal
         -> SignalGen (Signal a)
transfer x0 f s = mfix $ \sig -> do
    sig' <- delay x0 sig
    -- TODO: we shouldn't apply the function when there is no tick
    memo (liftA2 f s sig')

-}

-- | A random signal.
--
-- Example:
--
-- > do
-- >     smp <- start noise :: IO (IO Double)
-- >     res <- replicateM 5 smp
-- >     print res
--
-- Output:
--
-- > [0.12067753390401374,0.8658877349182655,0.7159264443196786,0.1756941896012891,0.9513646060896676]
noise :: MTRandom a => SignalGen (Signal a)
noise = memo (S randomIO)

-- | A random source within the 'SignalGen' monad.
getRandom :: MTRandom a => SignalGen a
getRandom = SG (const (const randomIO))

-- | A printing action within the 'SignalGen' monad.
debug :: String -> SignalGen ()
debug = SG . const . const . putStrLn

-- | The Show instance is only defined for the sake of Num...
instance Show (Signal a) where
  showsPrec _ _ s = "<SIGNAL>" ++ s

-- | Equality test is impossible.
instance Eq (Signal a) where
  _ == _ = False

-- | Error message for unimplemented instance functions.
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
