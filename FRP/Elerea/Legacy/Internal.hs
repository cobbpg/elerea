{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|

This is the core module of Elerea, which contains the signal
implementation and the atomic constructors.

The basic idea is to create a dataflow network whose structure closely
resembles the user's definitions by turning each combinator into a
mutable variable (an 'IORef').  In other words, each signal is
represented by a variable.  Such a variable contains information about
the operation to perform and (depending on the operation) references
to other signals.  For instance, a pointwise function application
created by the '<*>' operator contains an 'SNA' node, which holds two
references: one to the function signal and another to the argument
signal.

In order to have a pure(-looking) applicative interface for the most
part, the library relies on 'unsafePerformIO' to create the references
of stateless signals, while stateful signals have to be obtained from
a special 'SignalMonad', which is just a wrapping of 'IO' that doesn't
allow any other action to be performed.

The execution of the network is explicitly marked as an IO operation.
The core library exposes a single function to animate the network
called 'superstep', which takes a signal and a time interval, and
mutates all the variables the signal depends on.  It is supposed to be
called repeatedly in a loop that also takes care of user input.

To ensure consistency, a superstep has three phases: sampling, aging
and finalisation.  Each signal reachable from the top-level signal
passed to 'superstep' is sampled at the current point of time
('sample'), and the sample is stored along with the old signal in its
reference.  If the value of a signal is requested multiple times, the
sample is simply reused.  After successfully sampling the top-level
signal, the network is traversed again to advance by the desired time
('advance'), and when that's completed, the finalisation process
throws away the intermediate samples and marks the aged signals as the
current ones, ready to be sampled again.  If there is a dependency
loop, the system tries to use the 'sampleDelayed' function instead of
'sample' to get a useful value at the problematic spot instead of
entering an infinite loop.  Evaluation is initiated by the
'signalValue' function (which is used in both the sampling and the
aging phase to calculate samples and retrieve the cached values if
they are requested again), aging is performed by 'age', while
finalisation is done by 'commit'.  Since these functions are invoked
recursively on a data structure with existential types, their types
also need to be explicity quantified.

As a bonus, applicative nodes are automatically collapsed into lifted
functions of up to five arguments.  This optimisation significantly
reduces the number of nodes in the network.

-}

module FRP.Elerea.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import System.IO.Unsafe

-- * Implementation

-- ** Some type synonyms

{-| Time is continuous.  Nothing fancy. -}

type DTime = Double

{-| Sinks are used when feeding input into peripheral-bound signals. -}

type Sink a = a -> IO ()

-- ** The data structures behind signals

{-| A restricted monad to create stateful signals in. -}

newtype SignalMonad a = SM { createSignal :: IO a } deriving (Monad,Applicative,Functor,MonadFix)

{-| A printing function that can be used in the 'SignalMonad'.
Provided for debugging purposes. -}

signalDebug :: Show a => a -> SignalMonad ()
signalDebug = SM . print

{-| A signal is conceptually a time-varying value. -}

newtype Signal a = S (IORef (SignalTrans a))

{-| A node can have four states that distinguish various stages of
sampling and aging. -}

data SignalTrans a
    -- | @Ready s@ is simply the signal @s@ that was not sampled yet
    = Ready (SignalNode a)
    -- | @Sampling s@ is signal @s@ after its current value was
    -- requested, but not yet delivered
    | Sampling (SignalNode a)
    -- | @Sampled x s@ is signal @s@ paired with its current value @x@
    | Sampled a (SignalNode a)
    -- | @Aged x s@ is the aged version of signal @s@ paired with its
    -- current value @x@
    | Aged a (SignalNode a)

{-| The possible structures of a node are defined by the 'SignalNode'
type.  Note that the @SNFx@ nodes are only needed to optimise
applicatives, they can all be expressed in terms of @SNK@ and
@SNA@. -}

data SignalNode a
    -- | @SNK x@: constantly @x@
    = SNK a
    -- | @SNS x t@: stateful generator, where @x@ is current state and
    -- @t@ is the update function
    | SNS a (DTime -> a -> a)
    -- | @SNT s x t@: stateful transfer function, which also depends
    -- on an input signal @s@
    | forall t . SNT (Signal t) a (DTime -> t -> a -> a)
    -- | @SNA sf sx@: pointwise function application
    | forall t . SNA (Signal (t -> a)) (Signal t)
    -- | @SNH ss r@: the higher-order signal @ss@ collapsed into a
    -- signal cached in reference @r@; @r@ is used during the aging
    -- phase
    | SNH (Signal (Signal a)) (IORef (Signal a))
    -- | @SNM b sm@: signal generator that executes the monad carried
    -- by @sm@ whenever @b@ is true, and outputs the result (or
    -- undefined when @b@ is false)
    | SNM (Signal Bool) (Signal (SignalMonad a))
    -- | @SNE r@: opaque reference to connect peripherals
    | SNE (IORef a)
    -- | @SND s@: the @s@ signal delayed by one superstep
    | SND a (Signal a)
    -- | @SNKA s l@: equivalent to @s@ while aging signal @l@
    | forall t . SNKA (Signal a) (Signal t)
    -- | @SNF1 f@: @fmap f@
    | forall t . SNF1 (t -> a) (Signal t)
    -- | @SNF2 f@: @liftA2 f@
    | forall t1 t2 . SNF2 (t1 -> t2 -> a) (Signal t1) (Signal t2)
    -- | @SNF3 f@: @liftA3 f@
    | forall t1 t2 t3 . SNF3 (t1 -> t2 -> t3 -> a) (Signal t1) (Signal t2) (Signal t3)
    -- | @SNF4 f@: @liftA4 f@
    | forall t1 t2 t3 t4 . SNF4 (t1 -> t2 -> t3 -> t4 -> a) (Signal t1) (Signal t2) (Signal t3) (Signal t4)
    -- | @SNF5 f@: @liftA5 f@
    | forall t1 t2 t3 t4 t5 . SNF5 (t1 -> t2 -> t3 -> t4 -> t5 -> a) (Signal t1) (Signal t2) (Signal t3) (Signal t4) (Signal t5)

{-| You can uncomment the verbose version of this function to see the
applicative optimisations in action. -}

debugLog :: String -> IO a -> IO a
--debugLog s io = putStrLn s >> io
debugLog _ io = io

instance Functor Signal where
    fmap = (<*>) . pure

{-| The 'Applicative' instance with run-time optimisation.  The '<*>'
operator tries to move all the pure parts to its left side in order to
flatten the structure, hence cutting down on book-keeping costs.  Since
applicatives are used with pure functions and lifted values most of
the time, one can gain a lot by merging these nodes. -}

instance Applicative Signal where
    -- | A constant signal
    pure = makeSignalUnsafe . SNK
    -- | Point-wise application of a function and a data signal (like @ZipList@)

--  --mf <*> mx = sampler (fmap (\f -> sampler (fmap (pure . f) mx)) mf)
--  sf <*> sx = sampler (makeSignalUnsafe (SNF1 (\f -> sampler (makeSignalUnsafe (SNF1 (pure . f) sx))) sf))

    f@(S rf) <*> x@(S rx) = unsafePerformIO $ do
      -- General fall-back case
      c <- newIORef (Ready (SNA f x))

      let opt s = writeIORef c (Ready s)

      -- Optimisations might go haywire in the presence of loops,
      -- so we need to prepare to meeting undefined references by
      -- wrapping reads into exception handlers.

      flip catch (const (debugLog "no_fun" $ return ())) $ do
        Ready nf <- readIORef rf

        merged <- flip catch (const (debugLog "no_arg" $ return False)) $ do
          -- Merging constant branches from the two sides
          Ready nx <- readIORef rx
          case (nf,nx) of
            (SNK g,SNK y)                  -> debugLog "merge_00" $ opt (SNK (g y))
            (SNK g,SNF1 h y1)              -> debugLog "merge_01" $ opt (SNF1 (g.h) y1)
            (SNK g,SNF2 h y1 y2)           -> debugLog "merge_02" $ opt (SNF2 (\y1 y2 -> g (h y1 y2)) y1 y2)
            (SNK g,SNF3 h y1 y2 y3)        -> debugLog "merge_03" $ opt (SNF3 (\y1 y2 y3 -> g (h y1 y2 y3)) y1 y2 y3)
            (SNK g,SNF4 h y1 y2 y3 y4)     -> debugLog "merge_04" $ opt (SNF4 (\y1 y2 y3 y4 -> g (h y1 y2 y3 y4)) y1 y2 y3 y4)
            (SNK g,SNF5 h y1 y2 y3 y4 y5)  -> debugLog "merge_05" $ opt (SNF5 (\y1 y2 y3 y4 y5 -> g (h y1 y2 y3 y4 y5)) y1 y2 y3 y4 y5)
            (SNK g,_)                      -> debugLog "lift_1x" $ opt (SNF1 g x)
            (SNF1 g x1,SNK y)              -> debugLog "merge_10" $ opt (SNF1 (\x1 -> g x1 y) x1)
            (SNF1 g x1,SNF1 h y1)          -> debugLog "merge_11" $ opt (SNF2 (\x1 y1 -> g x1 (h y1)) x1 y1)
            (SNF1 g x1,SNF2 h y1 y2)       -> debugLog "merge_12" $ opt (SNF3 (\x1 y1 y2 -> g x1 (h y1 y2)) x1 y1 y2)
            (SNF1 g x1,SNF3 h y1 y2 y3)    -> debugLog "merge_13" $ opt (SNF4 (\x1 y1 y2 y3 -> g x1 (h y1 y2 y3)) x1 y1 y2 y3)
            (SNF1 g x1,SNF4 h y1 y2 y3 y4) -> debugLog "merge_14" $ opt (SNF5 (\x1 y1 y2 y3 y4 -> g x1 (h y1 y2 y3 y4)) x1 y1 y2 y3 y4)
            (SNF1 g x1,_)                  -> debugLog "lift_2x" $ opt (SNF2 g x1 x)
            (SNF2 g x1 x2,SNK y)           -> debugLog "merge_20" $ opt (SNF2 (\x1 x2 -> g x1 x2 y) x1 x2)
            (SNF2 g x1 x2,SNF1 h y1)       -> debugLog "merge_21" $ opt (SNF3 (\x1 x2 y1 -> g x1 x2 (h y1)) x1 x2 y1)
            (SNF2 g x1 x2,SNF2 h y1 y2)    -> debugLog "merge_22" $ opt (SNF4 (\x1 x2 y1 y2 -> g x1 x2 (h y1 y2)) x1 x2 y1 y2)
            (SNF2 g x1 x2,SNF3 h y1 y2 y3) -> debugLog "merge_23" $ opt (SNF5 (\x1 x2 y1 y2 y3 -> g x1 x2 (h y1 y2 y3)) x1 x2 y1 y2 y3)
            (SNF2 g x1 x2,_)               -> debugLog "lift_3x" $ opt (SNF3 g x1 x2 x)
            (SNF3 g x1 x2 x3,SNK y)        -> debugLog "merge_30" $ opt (SNF3 (\x1 x2 x3 -> g x1 x2 x3 y) x1 x2 x3)
            (SNF3 g x1 x2 x3,SNF1 h y1)    -> debugLog "merge_31" $ opt (SNF4 (\x1 x2 x3 y1 -> g x1 x2 x3 (h y1)) x1 x2 x3 y1)
            (SNF3 g x1 x2 x3,SNF2 h y1 y2) -> debugLog "merge_32" $ opt (SNF5 (\x1 x2 x3 y1 y2 -> g x1 x2 x3 (h y1 y2)) x1 x2 x3 y1 y2)
            (SNF3 g x1 x2 x3,_)            -> debugLog "lift_4x" $ opt (SNF4 g x1 x2 x3 x)
            (SNF4 g x1 x2 x3 x4,SNK y)     -> debugLog "merge_40" $ opt (SNF4 (\x1 x2 x3 x4 -> g x1 x2 x3 x4 y) x1 x2 x3 x4)
            (SNF4 g x1 x2 x3 x4,SNF1 h y1) -> debugLog "merge_41" $ opt (SNF5 (\x1 x2 x3 x4 y1 -> g x1 x2 x3 x4 (h y1)) x1 x2 x3 x4 y1)
            (SNF4 g x1 x2 x3 x4,_)         -> debugLog "lift_5x" $ opt (SNF5 g x1 x2 x3 x4 x)
            (SNF5 g x1 x2 x3 x4 x5,SNK y)  -> debugLog "merge_50" $ opt (SNF5 (\x1 x2 x3 x4 x5 -> g x1 x2 x3 x4 x5 y) x1 x2 x3 x4 x5)
            _                              -> return ()
          return True

        -- Lifting into higher arity not knowing the argument
        when (not merged) $ case nf of
          SNK g              -> debugLog "lift_1" $ opt (SNF1 g x)
          SNF1 g x1          -> debugLog "lift_2" $ opt (SNF2 g x1 x)
          SNF2 g x1 x2       -> debugLog "lift_3" $ opt (SNF3 g x1 x2 x)
          SNF3 g x1 x2 x3    -> debugLog "lift_4" $ opt (SNF4 g x1 x2 x3 x)
          SNF4 g x1 x2 x3 x4 -> debugLog "lift_5" $ opt (SNF5 g x1 x2 x3 x4 x)
          _                  -> return ()

      -- The final version
      return (S c)

{-| The @Show@ instance is only defined for the sake of 'Num'... -}

instance Show (Signal a) where
    showsPrec _ _ s = "<SIGNAL>" ++ s

{-| The equality test checks whether two signals are physically the same. -}

instance Eq (Signal a) where
    S s1 == S s2 = s1 == s2

{-| Error message for unimplemented instance functions. -}

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

-- ** Internal functions to run the network

{-| Creating a reference within the 'SignalMonad'.  Used for stateful
signals. -}

makeSignal :: SignalNode a -> SignalMonad (Signal a)
makeSignal node = SM $ do
  ref <- newIORef (Ready node)
  return (S ref)

{-| Creating a reference as a pure value.  Used for stateless
signals. -}

makeSignalUnsafe :: SignalNode a -> Signal a
makeSignalUnsafe = S . unsafePerformIO . newIORef . Ready

{-| Sampling the signal and all of its dependencies, at the same time.
We don't need the aged signal in the current superstep, only the
current value, so we sample before propagating the changes, which
might require the fresh sample because of recursive definitions. -}

signalValue :: forall a . Signal a -> DTime -> IO a
signalValue (S r) dt = do
  t <- readIORef r
  case t of
    Ready s     -> do writeIORef r (Sampling s)
                      -- TODO: advance can be evaluated in a separate
                      -- thread, since we don't need its result right
                      -- away, only in the next superstep.
                      v <- sample s dt
                      -- We memorise the sample to handle loops
                      -- nicely.  The undefined future signal cannot
                      -- bite us, because we don't need it during the
                      -- evaluation phase.
                      writeIORef r (Sampled v s)
                      return v
    Sampling s  -> do -- We started sampling this already, so there is
                      -- a dependency cycle we have to resolve by
                      -- adding a delay to stateful signals. Stateless
                      -- signals should not form a loop, which is
                      -- obvious...
                      v <- sampleDelayed s dt
                      writeIORef r (Sampled v s)
                      -- Since we are sampling it already, this node
                      -- will be overwritten by the case above when
                      -- the loop is closed.
                      return v
    Sampled v _ -> return v
    Aged v _    -> return v

{-| Aging the network of signals the given signal depends on. -}

age :: forall a . Signal a -> DTime -> IO ()
age (S r) dt = do
  t <- readIORef r
  case t of
    Sampled v s -> do s' <- advance s v dt
                      writeIORef r (Aged v s')
                      -- TODO: branching can be trivially parallelised
                      case s' of
                        SNT s _ _             -> age s dt
                        SNA sf sx             -> age sf dt >> age sx dt
                        SNH ss r              -> age ss dt >> readIORef r >>= \s -> age s dt
                        SNM b sm              -> age b dt >> age sm dt
                        SND _ s               -> age s dt
                        SNKA s l              -> age s dt >> age l dt
                        SNF1 _ s              -> age s dt
                        SNF2 _ s1 s2          -> age s1 dt >> age s2 dt
                        SNF3 _ s1 s2 s3       -> age s1 dt >> age s2 dt >> age s3 dt
                        SNF4 _ s1 s2 s3 s4    -> age s1 dt >> age s2 dt >> age s3 dt >> age s4 dt
                        SNF5 _ s1 s2 s3 s4 s5 -> age s1 dt >> age s2 dt >> age s3 dt >> age s4 dt >> age s5 dt
                        _                     -> return ()
    Aged _ _    -> return ()
    _           -> error "Inconsistent state: signal not sampled properly!"

{-| Finalising aged signals for the next round. -}

commit :: forall a . Signal a -> IO ()
commit (S r) = do
  t <- readIORef r
  case t of
    Aged _ s -> do writeIORef r (Ready s)
                   -- TODO: branching can be trivially parallelised
                   case s of
                     SNT s _ _             -> commit s
                     SNA sf sx             -> commit sf >> commit sx
                     SNH ss r              -> commit ss >> readIORef r >>= \s -> commit s
                     SNM b sm              -> commit b >> commit sm
                     SND _ s               -> commit s
                     SNKA s l              -> commit s >> commit l
                     SNF1 _ s              -> commit s
                     SNF2 _ s1 s2          -> commit s1 >> commit s2
                     SNF3 _ s1 s2 s3       -> commit s1 >> commit s2 >> commit s3
                     SNF4 _ s1 s2 s3 s4    -> commit s1 >> commit s2 >> commit s3 >> commit s4
                     SNF5 _ s1 s2 s3 s4 s5 -> commit s1 >> commit s2 >> commit s3 >> commit s4 >> commit s5
                     _                     -> return ()
    Ready _     -> return ()
    _           -> error "Inconsistent state: signal not aged properly!"

{-| Aging the signal.  Stateful signals have their state forced to
prevent building up big thunks.  The other nodes are structurally
static. -}

advance :: SignalNode a -> a -> DTime -> IO (SignalNode a)
advance (SNS x f)    _ dt = x `seq` return (SNS (f dt x) f)
advance (SNT s _ f)  v _  = v `seq` return (SNT s v f)
advance (SND _ s)    _ dt = do x <- signalValue s dt
                               return (SND x s)
advance s            _ _  = return s

{-| Sampling the signal at the current moment.  This is where static
nodes propagate changes to those they depend on.  Transfer functions
('SNT') work without delay, i.e. the effects of their input signals
can be observed in the same superstep. -}

sample :: SignalNode a -> DTime -> IO a
sample (SNK x)                 _  = return x
sample (SNS x _)               _  = return x
sample (SNT s x f)             dt = do t <- signalValue s dt
                                       return $! f dt t x
sample (SNA sf sx)             dt = signalValue sf dt <*> signalValue sx dt
sample (SNH ss r)              dt = do s <- signalValue ss dt
                                       writeIORef r s
                                       signalValue s dt
sample (SNM b sm)              dt = do c <- signalValue b dt
                                       SM m <- signalValue sm dt
                                       if c then m else return undefined
sample (SNE r)                 _  = readIORef r
sample (SND v _)               _  = return v
sample (SNKA s l)              dt = do signalValue l dt
                                       signalValue s dt
sample (SNF1 f s)              dt = f <$> signalValue s dt
sample (SNF2 f s1 s2)          dt = liftM2 f (signalValue s1 dt) (signalValue s2 dt)
sample (SNF3 f s1 s2 s3)       dt = liftM3 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt)
sample (SNF4 f s1 s2 s3 s4)    dt = liftM4 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt) (signalValue s4 dt)
sample (SNF5 f s1 s2 s3 s4 s5) dt = liftM5 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt) (signalValue s4 dt) (signalValue s5 dt)

{-| Sampling the signal with some kind of delay in order to resolve
dependency loops.  Transfer functions simply return their previous
output (delays can be considered a special case, because they always
do that, so 'sampleDelayed' is never called with them), while other
types of signals are always handled by the 'sample' function, so it is
not possible to create a working stateful loop composed of solely
stateless combinators. -}

sampleDelayed :: SignalNode a -> DTime -> IO a
sampleDelayed (SNT _ x _) _  = return x
sampleDelayed sn          dt = sample sn dt

-- ** Userland combinators

{-| Advancing the whole network that the given signal depends on by
the amount of time given in the second argument. -}

superstep :: Signal a -- ^ the top-level signal
          -> DTime    -- ^ the amount of time to advance
          -> IO a     -- ^ the current value of the signal
superstep world dt = do
  snapshot <- signalValue world dt
  age world dt
  commit world
  return snapshot

{-| A pure stateful signal.  The initial state is the first output. -}

stateful :: a                 -- ^ initial state
         -> (DTime -> a -> a) -- ^ state transformation
         -> SignalMonad (Signal a)
stateful x0 f = makeSignal (SNS x0 f)

{-| A stateful transfer function.  The current input affects the
current output, i.e. the initial state given in the first argument is
considered to appear before the first output, and can only be directly
observed by the `sampleDelayed` function. -}

transfer :: a                      -- ^ initial internal state
         -> (DTime -> t -> a -> a) -- ^ state updater function
         -> Signal t               -- ^ input signal
         -> SignalMonad (Signal a)
transfer x0 f s = makeSignal (SNT s x0 f)

{-| A continuous sampler that flattens a higher-order signal by
outputting its current snapshots. -}

sampler :: Signal (Signal a) -- ^ signal to flatten
        -> Signal a
sampler ss = makeSignalUnsafe (SNH ss (unsafePerformIO (newIORef undefined)))

{-| A reactive signal that takes the value to output from a monad
carried by its input when a boolean control signal is true, otherwise
it outputs 'Nothing'.  It is possible to create new signals in the
monad and also to print debug messages. -}

generator :: Signal Bool            -- ^ control (trigger) signal
          -> Signal (SignalMonad a) -- ^ a stream of monads to potentially run
          -> Signal (Maybe a)
generator b sm = toMaybe <$> b <*> makeSignalUnsafe (SNM b sm)

{-| A helper function to wrap any value in a 'Maybe' depending on a
boolean condition. -}

toMaybe :: Bool -> a -> Maybe a
toMaybe c v = if c then Just v else Nothing

{-| A signal that can be directly fed through the sink function
returned.  This can be used to attach the network to the outer
world. -}

external :: a                     -- ^ initial value
         -> IO (Signal a, Sink a) -- ^ the signal and an IO function to feed it
external x0 = do
  ref <- newIORef x0
  snr <- newIORef (Ready (SNE ref))
  return (S snr,writeIORef ref)

{-| The `delay` transfer function emits the value of a signal from the
previous superstep, starting with the filler value given in the first
argument.  It has to be a primitive, otherwise it could not be used to
prevent automatic delays. -}

delay :: a        -- ^ initial output
      -> Signal a -- ^ the signal to delay
      -> SignalMonad (Signal a)
delay x0 s = makeSignal (SND x0 s)

{-| Dependency injection to allow aging signals whose output is not
necessarily needed to produce the current sample of the first
argument.  It's equivalent to @(flip . liftA2 . flip) const@, as it
evaluates its second argument first. -}

keepAlive :: Signal a -- ^ the actual output
          -> Signal t -- ^ a signal guaranteed to age when this one is sampled
          -> Signal a
keepAlive s l = makeSignalUnsafe (SNKA s l)
