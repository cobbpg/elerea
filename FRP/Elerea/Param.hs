{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

This module provides leak-free and referentially transparent
higher-order discrete signals.  Unlike in "FRP.Elerea.Simple", the
sampling action has an extra argument that will be globally
distributed to every node and can be used to update the state.  For
instance, it can hold the time step between the two samplings, but it
could also encode all the external input to the system.

-}

module FRP.Elerea.Param
    (
    -- * The signal abstraction
      Signal
    , SignalGen
    -- * Embedding into I/O
    , start
    , external
    , externalMulti
    -- * Basic building blocks
    , delay
    , snapshot
    , generator
    , memo
    , until
    , input
    , embed
    -- * Derived combinators
    , stateful
    , transfer
    , transfer2
    , transfer3
    , transfer4
    -- * Signals with side effects
    -- $effectful
    , execute
    , effectful
    , effectful1
    , effectful2
    , effectful3
    , effectful4
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Prelude hiding (until)
import System.Mem.Weak

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
type UpdatePool = [Weak (IO (), IO ())]

-- | A signal generator is the only source of stateful signals.  It
-- can be thought of as a function of type @Nat -> Signal p -> a@,
-- where the result is an arbitrary data structure that can
-- potentially contain new signals, the first argument is the creation
-- time of these new signals, and the second is a globally accessible
-- input signal.  It exposes the 'MonadFix' interface, which makes it
-- possible to define signals in terms of each other.  Unlike the
-- simple variant, the denotation of signal generators differs from
-- that of signals.  We will use the following notation for
-- generators:
--
-- > g = <|g0 g1 g2 ...|>
--
-- Just like signals, generators behave as functions of time, but they
-- can also refer to the input signal:
--
-- > g t_start s_input = [g0,g1,g2,...] !! t_start
--
-- The conceptual difference between the two notions is that signals
-- are passed a sampling time, while generators expect a start time
-- that will be the creation time of all the freshly generated
-- signals in the resulting structure.
newtype SignalGen p a = SG { unSG :: IORef UpdatePool -> Signal p -> IO a }

-- | The phases every signal goes through during a superstep.
data Phase a = Ready a | Updated a a

instance Functor (SignalGen p) where
  fmap = liftM

instance Applicative (SignalGen p) where
  pure = return
  (<*>) = ap

instance Monad (SignalGen p) where
  return x = SG $ \_ _ -> return x
  SG g >>= f = SG $ \p i -> g p i >>= \x -> unSG (f x) p i

instance MonadFix (SignalGen p) where
  mfix f = SG $ \p i -> mfix $ \x -> unSG (f x) p i

instance MonadIO (SignalGen p) where
  liftIO = execute

instance MonadBase (SignalGen p) (SignalGen p) where
  liftBase = id

-- | Embedding a signal into an 'IO' environment.  Repeated calls to
-- the computation returned cause the whole network to be updated, and
-- the current sample of the top-level signal is produced as a result.
-- The computation accepts a global parameter that will be distributed
-- to all signals.  For instance, this can be the time step, if we
-- want to model continuous-time signals.  This is the only way to
-- extract a signal generator outside the network, and it is
-- equivalent to passing zero to the function representing the
-- generator.
--
-- Example:
--
-- > do
-- >     smp <- start (stateful 10 (+))
-- >     res <- forM [5,3,2,9,4] smp
-- >     print res
--
-- Output:
--
-- > [10,15,18,20,29]
start :: SignalGen p (Signal a) -- ^ the generator of the top-level signal
      -> IO (p -> IO a)         -- ^ the computation to sample the signal
start (SG gen) = do
  pool <- newIORef []
  (inp,sink) <- external undefined
  S sample <- gen pool inp
  return $ \param -> do
    sink param
    res <- sample
    superstep pool
    return res

-- | Performing the two-phase superstep.
superstep :: IORef UpdatePool -> IO ()
superstep pool = loop id []
  where
    deref ptr = (fmap.fmap) ((,) ptr) (deRefWeak ptr)
    loop getPtrs final = do
      (ptrs,acts) <- unzip.catMaybes <$> (mapM deref =<< readIORef pool)
      case acts of
          [] -> do
              sequence_ final
              writeIORef pool (getPtrs [])
          _  -> do
              writeIORef pool []
              mapM_ fst acts
              loop ((ptrs++) . getPtrs) (mapM_ snd acts : final)

-- | Auxiliary function used by all the primitives that create a
-- mutable variable.
addSignal :: (a -> IO a)      -- ^ sampling function
          -> (a -> IO ())     -- ^ aging function
          -> IORef (Phase a)  -- ^ the mutable variable behind the signal
          -> IORef UpdatePool -- ^ the pool of update actions
          -> IO (Signal a)
addSignal sample update ref pool = do
  let upd = readIORef ref >>= \v -> case v of
              Ready x -> update x
              _       -> return ()

      fin = readIORef ref >>= \v -> case v of
              Updated x _ -> writeIORef ref $! Ready x
              _           -> error "Signal not updated!"

      sig = S $ readIORef ref >>= \v -> case v of
              Ready x     -> sample x
              Updated _ x -> return x
      {-# NOINLINE sig #-}
      -- NOINLINE to prevent sig from getting inlined into the
      -- argument position of mkWeak.

  updateActions <- mkWeak sig (upd,fin) Nothing
  modifyIORef pool (updateActions:)
  return sig

-- | The 'delay' combinator is the elementary building block for
-- adding state to the signal network by constructing delayed versions
-- of a signal that emit a given value at creation time and the
-- previous output of the signal afterwards (@--@ is undefined):
--
-- > delay x0 s = <| <<x0 s0 s1 s2 s3 ...>>
-- >                 <<-- x0 s1 s2 s3 ...>>
-- >                 <<-- -- x0 s2 s3 ...>>
-- >                 <<-- -- -- x0 s3 ...>>
-- >                 ...
-- >              |>
--
-- It can be thought of as the following function (which should also
-- make it clear why the return value is 'SignalGen'):
--
-- > delay x0 s t_start s_input t_sample
-- >   | t_start == t_sample = x0
-- >   | t_start < t_sample  = s (t_sample-1)
-- >   | otherwise           = error \"Premature sample!\"
--
-- The way signal generators are extracted by 'generator' ensures that
-- the error can never happen.  It is also clear that the behaviour of
-- 'delay' is not affected in any way by the global input.
--
-- Example (requires the @DoRec@ extension):
--
-- > do
-- >     smp <- start $ do
-- >         rec let fib'' = liftA2 (+) fib' fib
-- >             fib' <- delay 1 fib''
-- >             fib <- delay 1 fib'
-- >         return fib
-- >     res <- replicateM 7 (smp undefined)
-- >     print res
--
-- Output:
--
-- > [1,1,2,3,5,8,13]
delay :: a                        -- ^ initial output
      -> Signal a                 -- ^ the signal to delay
      -> SignalGen p (Signal a)
delay x0 (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready x0)

  let update x = s >>= \x' -> x' `seq` writeIORef ref (Updated x' x)

  addSignal return update ref pool

-- | A formal conversion from signals to signal generators, which
-- effectively allows for retrieving the current value of a previously
-- created signal within a generator.  This includes both signals
-- defined in an external scope as well as those created earlier in
-- the same generator.  It can be modelled by the following function:
--
-- > snapshot s t_start s_input = s t_start
snapshot :: Signal a -> SignalGen p a
snapshot (S s) = SG $ \_ _ -> s

-- | Auxiliary function.
memoise :: IORef (Phase a) -> a -> IO a
memoise ref x = writeIORef ref (Updated undefined x) >> return x

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
-- > generator g t_start s_input t_sample = g t_sample t_sample s_input
--
-- It has to live in the 'SignalGen' monad, because it needs to
-- maintain an internal state to be able to cache the current sample
-- for efficiency reasons. However, this state is not carried between
-- samples, therefore start time doesn't matter and can be ignored.
-- Also, even though it does not make use of the global input itself,
-- part of its job is to distribute it among the newly generated
-- signals.
--
-- Refer to the longer example at the bottom of "FRP.Elerea.Simple" to
-- see how it can be used.
generator :: Signal (SignalGen p a) -- ^ the signal of generators to run
          -> SignalGen p (Signal a) -- ^ the signal of generated structures
generator (S s) = SG $ \pool inp -> do
  ref <- newIORef (Ready undefined)

  let sample = (s >>= \(SG g) -> g pool inp) >>= memoise ref
  
  addSignal (const sample) (const (() <$ sample)) ref pool

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
-- Just like 'delay', it is independent of the global input.
memo :: Signal a               -- ^ the signal to cache
     -> SignalGen p (Signal a) -- ^ a signal observationally equivalent to the argument
memo (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = s >>= memoise ref

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | A signal that is true exactly once: the first time the input
-- signal is true.  Afterwards, it is constantly false, and it holds
-- no reference to the input signal.  For instance (assuming the rest
-- of the input is constantly @False@):
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
-- > until s = do
-- >     step <- transfer False (const (||)) s
-- >     dstep <- delay False step
-- >     memo (liftA2 (/=) step dstep)
--
-- Example:
--
-- > do
-- >     smp <- start $ do
-- >         accum <- stateful 0 (+)
-- >         tick <- until ((>=10) <$> accum)
-- >         return $ liftA2 (,) accum tick
-- >     res <- forM [4,1,3,5,2,8,6] smp
-- >     print res
--
-- Output:
--
-- > [(0,False),(4,False),(5,False),(8,False),(13,True),(15,False),(23,False)]
until :: Signal Bool               -- ^ the boolean input signal
      -> SignalGen p (Signal Bool) -- ^ a one-shot signal true only the first time the input is true
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

-- | The common input signal that is fed through the function returned
-- by 'start', unless we are in an 'embed'ded generator.  It is
-- equivalent to the following function:
--
-- > input t_start s_input = s_input
--
-- Example:
--
-- > do
-- >     smp <- start $ do
-- >         sig <- input
-- >         return (sig*2)
-- >     res <- forM [4,1,3,5,2,8,6] smp
-- >     print res
--
-- Output:
--
-- > [8,2,6,10,4,16,12]
input :: SignalGen p (Signal p)
input = SG $ const return

-- | Embed a generator with an overridden input signal.  It is
-- equivalent to the following function:
--
-- > embed s g t_start s_input = g t_start s
--
-- Example:
--
-- > do
-- >     smp <- start $ do
-- >         sig <- input
-- >         embed (sig*2) $ do
-- >             sig <- input
-- >             return (sig+1)
-- >     res <- forM [4,1,3,5,2,8,6] smp
-- >     print res
--
-- Output:
--
-- > [9,3,7,11,5,17,13]
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
             ref <- newIORef (Ready undefined)
             let sample = modifyMVar var $ \list -> memoise ref list >> return ([], list)
             addSignal (const sample) (const (() <$ sample)) ref pool
         ,\val -> do vals <- takeMVar var
                     putMVar var (val:vals)
         )

-- | A direct stateful transformation of the input.  The initial state
-- is the first output, and every following output is calculated from
-- the previous one and the value of the global parameter (which might
-- have been overridden by 'embed').
--
-- Example:
--
-- > do
-- >     smp <- start (stateful "" (:))
-- >     res <- forM "olleh~" smp
-- >     print res
--
-- Output:
--
-- > ["","o","lo","llo","ello","hello"]
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
-- Example (assuming a delta time is passed to the sampling function
-- in each step):
--
-- > integral x0 s = transfer x0 (\dt v x -> x+dt*v)
--
-- Example for using the above:
--
-- > do
-- >     smp <- start (integral 3 (pure 2))
-- >     res <- replicateM 7 (smp 0.1)
-- >     print res
--
-- Output:
--
-- > [3.2,3.4,3.6,3.8,4.0,4.2,4.4]
transfer :: a                   -- ^ initial internal state
         -> (p -> t -> a -> a)  -- ^ state updater function
         -> Signal t            -- ^ input signal
         -> SignalGen p (Signal a)
transfer x0 f s = mfix $ \sig -> do
    inp <- input
    sig' <- delay x0 sig
    memo (liftA3 f inp s sig')

-- | A variation of 'transfer' with two input signals.
transfer2 :: a                          -- ^ initial internal state
          -> (p -> t1 -> t2 -> a -> a)  -- ^ state updater function
          -> Signal t1                  -- ^ input signal 1
          -> Signal t2                  -- ^ input signal 2
          -> SignalGen p (Signal a)
transfer2 x0 f s1 s2 = mfix $ \sig -> do
    inp <- input
    sig' <- delay x0 sig
    memo (liftM4 f inp s1 s2 sig')

-- | A variation of 'transfer' with three input signals.
transfer3 :: a                                -- ^ initial internal state
          -> (p -> t1 -> t2 -> t3 -> a -> a)  -- ^ state updater function
          -> Signal t1                        -- ^ input signal 1
          -> Signal t2                        -- ^ input signal 2
          -> Signal t3                        -- ^ input signal 3
          -> SignalGen p (Signal a)
transfer3 x0 f s1 s2 s3 = mfix $ \sig -> do
    inp <- input
    sig' <- delay x0 sig
    memo (liftM5 f inp s1 s2 s3 sig')

-- | A variation of 'transfer' with four input signals.
transfer4 :: a                                      -- ^ initial internal state
          -> (p -> t1 -> t2 -> t3 -> t4 -> a -> a)  -- ^ state updater function
          -> Signal t1                              -- ^ input signal 1
          -> Signal t2                              -- ^ input signal 2
          -> Signal t3                              -- ^ input signal 3
          -> Signal t4                              -- ^ input signal 4
          -> SignalGen p (Signal a)
transfer4 x0 f s1 s2 s3 s4 = mfix $ \sig -> do
    inp <- input
    sig' <- delay x0 sig
    memo (liftM5 f inp s1 s2 s3 s4 `ap` sig')

{- $effectful

The following combinators are primarily aimed at library implementors
who wish build abstractions to effectful libraries on top of Elerea.

-}

-- | An IO action executed in the 'SignalGen' monad. Can be used as
-- `liftIO`.
execute :: IO a -> SignalGen p a
execute act = SG $ \_ _ -> act

-- | A signal that executes a given IO action once at every sampling.
--
-- In essence, this combinator provides cooperative multitasking
-- capabilities, and its primary purpose is to assist library writers
-- in wrapping effectful APIs as conceptually pure signals.  If there
-- are several effectful signals in the system, their order of
-- execution is undefined and should not be relied on.
--
-- Example:
--
-- > do
-- >     act <- start $ do
-- >         ref <- execute $ newIORef 0
-- >         let accum n = do
-- >                 x <- readIORef ref
-- >                 putStrLn $ "Accumulator: " ++ show x
-- >                 writeIORef ref $! x+n
-- >                 return ()
-- >         effectful1 accum =<< input
-- >     forM_ [4,9,2,1,5] act
--
-- Output:
--
-- > Accumulator: 0
-- > Accumulator: 4
-- > Accumulator: 13
-- > Accumulator: 15
-- > Accumulator: 16
--
-- Another example (requires mersenne-random):
--
-- > do
-- >     smp <- start $ effectful randomIO :: IO (IO Double)
-- >     res <- replicateM 5 smp
-- >     print res
--
-- Output:
--
-- > [0.12067753390401374,0.8658877349182655,0.7159264443196786,0.1756941896012891,0.9513646060896676]
effectful :: IO a                 -- ^ the action to be executed repeatedly
          -> SignalGen p (Signal a)
effectful act = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = act >>= memoise ref

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | A signal that executes a parametric IO action once at every
-- sampling.  The parameter is supplied by another signal at every
-- sampling step.
effectful1 :: (t -> IO a)          -- ^ the action to be executed repeatedly
           -> Signal t             -- ^ parameter signal
           -> SignalGen p (Signal a)
effectful1 act (S s) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = s >>= act >>= memoise ref

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | Like 'effectful1', but with two parameter signals.
effectful2 :: (t1 -> t2 -> IO a)   -- ^ the action to be executed repeatedly
           -> Signal t1            -- ^ parameter signal 1
           -> Signal t2            -- ^ parameter signal 2
           -> SignalGen p (Signal a)
effectful2 act (S s1) (S s2) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = join (liftM2 act s1 s2) >>= memoise ref

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | Like 'effectful1', but with three parameter signals.
effectful3 :: (t1 -> t2 -> t3 -> IO a) -- ^ the action to be executed repeatedly
           -> Signal t1                -- ^ parameter signal 1
           -> Signal t2                -- ^ parameter signal 2
           -> Signal t3                -- ^ parameter signal 3
           -> SignalGen p (Signal a)
effectful3 act (S s1) (S s2) (S s3) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = join (liftM3 act s1 s2 s3) >>= memoise ref

  addSignal (const sample) (const (() <$ sample)) ref pool

-- | Like 'effectful1', but with four parameter signals.
effectful4 :: (t1 -> t2 -> t3 -> t4 -> IO a) -- ^ the action to be executed repeatedly
           -> Signal t1                      -- ^ parameter signal 1
           -> Signal t2                      -- ^ parameter signal 2
           -> Signal t3                      -- ^ parameter signal 3
           -> Signal t4                      -- ^ parameter signal 4
           -> SignalGen p (Signal a)
effectful4 act (S s1) (S s2) (S s3) (S s4) = SG $ \pool _ -> do
  ref <- newIORef (Ready undefined)

  let sample = join (liftM4 act s1 s2 s3 s4) >>= memoise ref

  addSignal (const sample) (const (() <$ sample)) ref pool

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
