{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

This version differs from the simple one in providing an extra
argument to the sampling action that will be globally distributed to
every node and can be used to update the state.  For instance, it can
hold the time step between the two samplings, but it could also encode
all the external input to the system.

The interface of this module differs from the old Elerea in the
following ways:

* the delta time argument is generalised to an arbitrary type, so it
  is possible to do without 'external' altogether in case someone
  wants to do so;

* there is no 'sampler' any more, it is substituted by 'join', as
  signals are monads;

* 'generator' has been conceptually simplified, so it's a more basic
  primitive now;

* there is no automatic delay in order to preserve semantic soundness
  (e.g. the monad laws for signals);

* all signals are aged regardless of whether they are sampled
  (i.e. their behaviour doesn't depend on the context any more);

* the user needs to cache the results of applicative operations to be
  reused in multiple places explicitly using the 'memo' combinator;

* the input can be retrieved as an explicit signal within the
  SignalGen monad, and also overridden for parts of the network.

-}

module FRP.Elerea.Param
    ( Signal
    , SignalGen
    , start
    , external
    , externalMulti
    , delay
    , generator
    , memo
    , until
    , input
    , embed
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

-- | A signal can be thought of as a function of type @Nat -> a@, and
-- its 'Monad' instance agrees with that intuition.  Internally, is
-- represented by a sampling computation.
newtype Signal a = S (IO a) deriving (Functor, Applicative, Monad)

-- | A dynamic set of actions to update a network without breaking
-- consistency.
type UpdatePool = [Weak (IO (), IO ())]

-- | A signal generator is the only source of stateful signals.
-- Internally, computes a signal structure and adds the new variables
-- to an existing update pool.
newtype SignalGen p a = SG { unSG :: IORef UpdatePool -> Signal p -> IO a }

-- | The phases every signal goes through during a superstep: before
-- or after sampling.
data Phase s a = Ready s | Aged s a

instance Functor (SignalGen p) where
  fmap = liftM

instance Applicative (SignalGen p) where
  pure = return
  (<*>) = ap

instance Monad (SignalGen p) where
  return = SG . const . const . return
  SG g >>= f = SG $ \p i -> g p i >>= \x -> unSG (f x) p i

instance MonadFix (SignalGen p) where
  mfix f = SG $ \p i -> mfix (($i).($p).unSG.f)

-- | Embedding a signal into an 'IO' environment.  Repeated calls to
-- the computation returned cause the whole network to be updated, and
-- the current sample of the top-level signal is produced as a
-- result. The computation accepts a global parameter that will be
-- distributed to all signals.  For instance, this can be the time
-- step, if we want to model continuous-time signals.
start :: SignalGen p (Signal a) -- ^ the generator of the top-level signal
      -> IO (p -> IO a)         -- ^ the computation to sample the signal
start (SG gen) = do
  pool <- newIORef []
  (inp,sink) <- external undefined
  S sample <- gen pool inp

  ptrs0 <- readIORef pool
  writeIORef pool []
  (as0,cs0) <- unzip . map fromJust <$> mapM deRefWeak ptrs0
  let ageStatic = sequence_ as0
      commitStatic = sequence_ cs0

  return $ \param -> do
    let update [] ptrs age commit = do
          writeIORef pool ptrs
          ageStatic >> age
          commitStatic >> commit
        update (p:ps) ptrs age commit = do
          r <- deRefWeak p
          case r of
            Nothing -> update ps ptrs age commit
            Just (a,c) -> update ps (p:ptrs) (age >> a) (commit >> c)

    sink param
    res <- sample
    ptrs <- readIORef pool
    update ptrs [] (return ()) (return ())
    return res

-- | Auxiliary function used by all the primitives that create a
-- mutable variable.
addSignal :: (Phase s a -> IO a)  -- ^ sampling function
          -> (Phase s a -> IO ()) -- ^ aging function
          -> IORef (Phase s a)    -- ^ the mutable variable behind the signal
          -> IORef UpdatePool     -- ^ the pool of update actions
          -> IO (Signal a)
addSignal sample age ref pool = do
  let  commit (Aged s _)  = Ready s
       commit _           = error "commit error: signal not aged"

       sig = S $ readIORef ref >>= sample

  update <- mkWeak sig (readIORef ref >>= age, modifyIORef ref commit) Nothing
  modifyIORef pool (update:)
  return sig

-- | The 'delay' transfer function emits the value of a signal from
-- the previous superstep, starting with the filler value given in the
-- first argument.
delay :: a                        -- ^ initial output
      -> Signal a                 -- ^ the signal to delay
      -> SignalGen p (Signal a)
delay x0 (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready x0)

  let  sample (Ready x)   = return x
       sample (Aged _ x)  = return x

       age (Ready x)  = s >>= \x' -> x' `seq` writeIORef ref (Aged x' x)
       age _          = return ()

  addSignal sample age ref pool

-- | Memoising combinator.  It can be used to cache results of
-- applicative combinators in case they are used in several
-- places. Other than that, it is equivalent to 'return'.
memo :: Signal a               -- ^ signal to memoise
     -> SignalGen p (Signal a)
memo (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let  sample (Ready _)      = s >>= \x -> writeIORef ref (Aged undefined x) >> return x
       sample (Aged _ x)     = return x

       age (Ready _)      = s >>= \x -> writeIORef ref (Aged undefined x)
       age _              = return ()

  addSignal sample age ref pool

-- | A reactive signal that takes the value to output from a monad
-- carried by its input.  It is possible to create new signals in the
-- monad.
generator :: Signal (SignalGen p a) -- ^ a stream of generators to potentially run
          -> SignalGen p (Signal a)
generator (S gen) = SG $ \pool inp -> do
  ref <- newIORef (Ready undefined)

  let  next = ($inp).($pool).unSG =<< gen

       sample (Ready _)  = next >>= \x' -> writeIORef ref (Aged x' x') >> return x'
       sample (Aged _ x) = return x

       age (Ready _) = next >>= \x' -> writeIORef ref (Aged x' x')
       age _         = return ()

  addSignal sample age ref pool

-- | A signal that is true exactly once: the first time the input
-- signal is true.  Afterwards, it is constantly false, and it holds
-- no reference to the input signal.
until :: Signal Bool               -- ^ the boolean input signal
      -> SignalGen p (Signal Bool) -- ^ a one-shot signal true only the first time the input is true
until (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  rsmp <- mfix $ \rs -> newIORef $ do
    x <- s
    writeIORef ref (Aged undefined x)
    when x $ writeIORef rs $ do
      writeIORef ref (Aged undefined False)
      return False
    return x

  let sample = join (readIORef rsmp)

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | The common input signal that is fed through the function returned
-- by 'start', unless we are in an 'embed'ded generator.
input :: SignalGen p (Signal p)
input = SG $ const return

-- | Embed a generator with an overridden input signal.
embed :: Signal p' -> SignalGen p' a -> SignalGen p a
embed s (SG g) = SG $ \pool _ -> g pool s

-- | A signal that can be directly fed through the sink function
-- returned.  This can be used to attach the network to the outer
-- world.  Note that this is optional, as all the input of the network
-- can be fed in through the global parameter, although that is not
-- really convenient for many signals.
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
externalMulti :: IO (SignalGen p (Signal [a]), a -> IO ()) -- ^ a generator for the event signal and the associated sink
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
-- and every following output is calculated from the previous one and
-- the value of the global parameter (which might have been overridden
-- by 'embed').  It is equivalent to the following expression:
--
-- @
--  stateful x0 f = 'mfix' $ \sig -> 'input' >>= \i -> 'delay' x0 (f '<$>' i '<*>' sig)
-- @
stateful :: a                    -- ^ initial state
         -> (p -> a -> a)        -- ^ state transformation
         -> SignalGen p (Signal a)
stateful x0 f = mfix $ \sig -> input >>= \i -> delay x0 (f <$> i <*> sig)

-- | A stateful transfer function.  The current input affects the
-- current output, i.e. the initial state given in the first argument
-- is considered to appear before the first output, and can never be
-- observed.  Every output is derived from the current value of the
-- input signal, the global parameter (which might have been
-- overridden by 'embed') and the previous output.  It is equivalent
-- to the following expression:
--
-- @
--  transfer x0 f s = 'mfix' $ \sig -> 'input' >>= \i -> 'liftA3' f i s '<$>' 'delay' x0 sig
-- @
transfer :: a                    -- ^ initial internal state
         -> (p -> t -> a -> a)   -- ^ state updater function
         -> Signal t             -- ^ input signal
         -> SignalGen p (Signal a)
transfer x0 f s = mfix $ \sig -> input >>= \i -> liftA3 f i s <$> delay x0 sig

-- | A random signal.
noise :: MTRandom a => SignalGen p (Signal a)
noise = memo (S randomIO)

-- | A random source within the 'SignalGen' monad.
getRandom :: MTRandom a => SignalGen p a
getRandom = SG (const (const randomIO))

-- | A printing action within the 'SignalGen' monad.
debug :: String -> SignalGen p ()
debug = SG . const . const . putStrLn

-- | The @Show@ instance is only defined for the sake of 'Num'...
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
