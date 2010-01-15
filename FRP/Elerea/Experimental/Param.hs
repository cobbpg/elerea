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
  reused in multiple places explicitly using the 'memo' combinator.

-}

module FRP.Elerea.Experimental.Param
    ( Signal
    , SignalGen
    , start
    , external
    , delay
    , stateful
    , transfer
    , memo
    , generator
    , noise
    , getRandom
    , debug
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Maybe
import System.Mem.Weak
import System.Random.Mersenne

-- | A signal can be thought of as a function of type @Nat -> a@, and
-- its 'Monad' instance agrees with that intuition.  Internally, is
-- represented by a sampling computation.
newtype Signal p a = S { unS :: p -> IO a }

-- | A dynamic set of actions to update a network without breaking
-- consistency.
type UpdatePool p = [Weak (p -> IO (), IO ())]

-- | A signal generator is the only source of stateful signals.
-- Internally, computes a signal structure and adds the new variables
-- to an existing update pool.
newtype SignalGen p a = SG { unSG :: IORef (UpdatePool p) -> IO a }

-- | The phases every signal goes through during a superstep: before
-- or after sampling.
data Phase s a = Ready s | Aged s a

instance Functor (Signal p) where
  fmap = liftM

instance Applicative (Signal p) where
  pure = return
  (<*>) = ap

instance Monad (Signal p) where
  return = S . const . return
  S g >>= f = S $ \p -> g p >>= \x -> unS (f x) p

instance Functor (SignalGen p) where
  fmap = liftM

instance Applicative (SignalGen p) where
  pure = return
  (<*>) = ap

instance Monad (SignalGen p) where
  return = SG . const . return
  SG g >>= f = SG $ \p -> g p >>= \x -> unSG (f x) p

instance MonadFix (SignalGen p) where
  mfix f = SG $ \p -> mfix (($p).unSG.f)

-- | Embedding a signal into an 'IO' environment.  Repeated calls to
-- the computation returned cause the whole network to be updated, and
-- the current sample of the top-level signal is produced as a
-- result. The computation accepts a global parameter that will be
-- distributed to all signals.  For instance, this can be the time
-- step, if we want to model continuous-time signals.
start :: SignalGen p (Signal p a) -- ^ the generator of the top-level signal
      -> IO (p -> IO a)           -- ^ the computation to sample the signal
start (SG gen) = do
  pool <- newIORef []
  (S sample) <- gen pool

  ptrs0 <- readIORef pool
  writeIORef pool []
  (as0,cs0) <- unzip . map fromJust <$> mapM deRefWeak ptrs0
  let ageStatic param = mapM_ ($param) as0
      commitStatic = sequence_ cs0

  return $ \param -> do
    let update [] ptrs age commit = do
          writeIORef pool ptrs
          ageStatic param >> age
          commitStatic >> commit
        update (p:ps) ptrs age commit = do
          r <- deRefWeak p
          case r of
            Nothing -> update ps ptrs age commit
            Just (a,c) -> update ps (p:ptrs) (age >> a param) (commit >> c)

    res <- sample param
    ptrs <- readIORef pool
    update ptrs [] (return ()) (return ())
    return res

-- | Auxiliary function used by all the primitives that create a
-- mutable variable.
addSignal :: (p -> Phase s a -> IO a)  -- ^ sampling function
          -> (p -> Phase s a -> IO ()) -- ^ aging function
          -> IORef (Phase s a)         -- ^ the mutable variable behind the signal
          -> IORef (UpdatePool p)      -- ^ the pool of update actions
          -> IO (Signal p a)
addSignal sample age ref pool = do
  let  commit (Aged s _)  = Ready s
       commit _           = error "commit error: signal not aged"

       sig = S $ \p -> readIORef ref >>= sample p

  update <- mkWeak sig (\p -> readIORef ref >>= age p, modifyIORef ref commit) Nothing
  modifyIORef pool (update:)
  return sig

-- | The 'delay' transfer function emits the value of a signal from
-- the previous superstep, starting with the filler value given in the
-- first argument.
delay :: a                        -- ^ initial output
      -> Signal p a               -- ^ the signal to delay
      -> SignalGen p (Signal p a)
delay x0 (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let  sample _ (Ready x)   = return x
       sample _ (Aged _ x)  = return x

       age p (Ready x)  = s p >>= \x' -> x' `seq` writeIORef ref (Aged x' x)
       age _ _          = return ()

  addSignal sample age ref pool

-- | Memoising combinator.  It can be used to cache results of
-- applicative combinators in case they are used in several
-- places. Other than that, it is equivalent to 'return'.
memo :: Signal p a               -- ^ signal to memoise
     -> SignalGen p (Signal p a)
memo (S s) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)

  let  sample p (Ready _)      = s p >>= \x -> writeIORef ref (Aged undefined x) >> return x
       sample _ (Aged _ x)     = return x

       age p (Ready _)      = s p >>= \x -> writeIORef ref (Aged undefined x)
       age _ _              = return ()

  addSignal sample age ref pool

-- | A reactive signal that takes the value to output from a monad
-- carried by its input.  It is possible to create new signals in the
-- monad.
generator :: Signal p (SignalGen p a) -- ^ a stream of generators to potentially run
          -> SignalGen p (Signal p a)
generator (S gen) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)

  let  next p = ($pool).unSG =<< gen p

       sample p (Ready _)  = next p >>= \x' -> writeIORef ref (Aged x' x') >> return x'
       sample _ (Aged _ x) = return x

       age p (Ready _) = next p >>= \x' -> writeIORef ref (Aged x' x')
       age _ _         = return ()

  addSignal sample age ref pool

-- | A signal that can be directly fed through the sink function
-- returned.  This can be used to attach the network to the outer
-- world.  Note that this is optional, as all the input of the network
-- can be fed in through the global parameter, although that is not
-- really convenient for many signals.
external :: a                           -- ^ initial value
         -> IO (Signal p a, a -> IO ()) -- ^ the signal and an IO function to feed it
external x = do
  ref <- newIORef x
  return (S (const (readIORef ref)), writeIORef ref)

-- | A pure stateful signal.  The initial state is the first output,
-- and every following output is calculated from the previous one and
-- the value of the global parameter.
stateful :: a -> (p -> a -> a) -> SignalGen p (Signal p a)
stateful x0 f = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let  sample _ (Ready x)  = return x
       sample _ (Aged _ x) = return x

       age p (Ready x) = let x' = f p x in x' `seq` writeIORef ref (Aged x' x)
       age _ _         = return ()

  addSignal sample age ref pool

-- | A stateful transfer function.  The current input affects the
-- current output, i.e. the initial state given in the first argument
-- is considered to appear before the first output, and can never be
-- observed.  Every output is derived from the current value of the
-- input signal, the global parameter and the previous output.
transfer :: a -> (p -> t -> a -> a) -> Signal p t -> SignalGen p (Signal p a)
transfer x0 f (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let  sample p (Ready x)  = s p >>= \y -> let x' = f p y x in
                                            x' `seq` writeIORef ref (Aged x' x') >> return x'
       sample _ (Aged _ x) = return x

       age p (Ready x) = s p >>= \y -> let x' = f p y x in
                                        x' `seq` writeIORef ref (Aged x' x')
       age _ _         = return ()

  addSignal sample age ref pool

-- | A random signal.  For efficiency reasons it is not guaranteed to
-- read the same value when sampled several times in the same
-- superstep.  If you need consistent noise input, you can produce it
-- through an 'external' signal from whatever source you prefer.
noise :: MTRandom a => Signal p a
noise = S (const randomIO)

-- | A random source within the 'SignalGen' monad.
getRandom :: MTRandom a => SignalGen p a
getRandom = SG (const randomIO)

-- | A printing action within the 'SignalGen' monad.
debug :: String -> SignalGen p ()
debug = SG . const . putStrLn

-- | The @Show@ instance is only defined for the sake of 'Num'...
instance Show (Signal p a) where
  showsPrec _ _ s = "<SIGNAL>" ++ s

-- | Equality test is impossible.
instance Eq (Signal p a) where
  _ == _ = False

-- | Error message for unimplemented instance functions.
unimp :: String -> a
unimp = error . ("Signal: "++)

instance Ord t => Ord (Signal p t) where
  compare = unimp "compare"
  min = liftA2 min
  max = liftA2 max

instance Enum t => Enum (Signal p t) where
  succ = fmap succ
  pred = fmap pred
  toEnum = pure . toEnum
  fromEnum = unimp "fromEnum"
  enumFrom = unimp "enumFrom"
  enumFromThen = unimp "enumFromThen"
  enumFromTo = unimp "enumFromTo"
  enumFromThenTo = unimp "enumFromThenTo"

instance Bounded t => Bounded (Signal p t) where
  minBound = pure minBound
  maxBound = pure maxBound

instance Num t => Num (Signal p t) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  signum = fmap signum
  abs = fmap abs
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Real t => Real (Signal p t) where
  toRational = unimp "toRational"

instance Integral t => Integral (Signal p t) where
  quot = liftA2 quot
  rem = liftA2 rem
  div = liftA2 div
  mod = liftA2 mod
  quotRem a b = (fst <$> qrab,snd <$> qrab)
    where qrab = quotRem <$> a <*> b
  divMod a b = (fst <$> dmab,snd <$> dmab)
    where dmab = divMod <$> a <*> b
  toInteger = unimp "toInteger"

instance Fractional t => Fractional (Signal p t) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating t => Floating (Signal p t) where
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
