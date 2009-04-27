{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|

This is the core module of Elerea, which contains the signal
implementation and the primitive constructors.

The basic idea is to create a dataflow network whose structure closely
resembles the user's definitions by turning each combinator into a
mutable variable (an 'IORef').  In other words, each signal is
represented by a variable.  Such a variable contains information about
the operation to perform and (depending on the operation) references
to other signals.  For instance, a pointwise function application
created by the '<*>' operator contains an 'SNA' node, which holds two
references: one to the function signal and another to the argument
signal.

In order to have a pure(-looking) applicative interface, the library
relies on 'unsafePerformIO' to create the references on demand.  In
contrast, the execution of the network is explicitly marked as an IO
operation.  The core library exposes a single function to animate the
network called 'superstep', which takes a signal and a time interval,
and mutates all the variables the signal depends on.  It is supposed
to be called repeatedly in a loop that also takes care of user input.

To ensure consistency, a superstep has two phases: evaluation and
finalisation.  During evaluation, each signal affected is sampled at
the current point of time ('sample'), advanced by the desired time
('advance'), and both of these pieces of data are stored in its
reference.  If the value of a signal is requested multiple times, the
sample is simply reused, and no further aging is performed.  After
successfully sampling the top-level signal, the finalisation process
throws away the intermediate samples and marks the aged signals as the
current ones, ready to be sampled again.  If there is a dependency
loop, the system tries to use the `sampleDelayed` function instead of
`sample` to get a useful value at the problematic spot instead of
entering an infinite loop.  Evaluation is done by the 'signalValue'
function, while finalisation is done by 'commit'.  Since these
functions are invoked recursively on a data structure with existential
types, their types also need to be explicity quantified.

As a bonus, applicative nodes are automatically collapsed into lifted
functions of up to five arguments.  This optimisation significantly
reduces the number of nodes in the network.

-}

module FRP.Elerea.Internal where

import Control.Applicative
import Control.Monad
import Data.IORef
import System.IO.Unsafe

-- * Implementation

-- ** Some type synonyms

{-| Time is continuous.  Nothing fancy. -}

type Time = Double

type DTime = Double

{-| Sinks are used when feeding input into peripheral-bound signals. -}

type Sink a = a -> IO ()

-- ** The data structures behind signals

{-| A signal is represented as a /transactional/ structural node. -}

newtype Signal a = S (IORef (SignalTrans a))

{-| A signal of unknown type. -}

newtype AnySignal = AS (forall a . Signal a)

{-| A node can have four states that distinguish various stages of
sampling and aging. -}

data SignalTrans a
    -- | @Ready s@ is simply the signal @s@ that was not sampled yet
    = Ready (SignalNode a)
    -- | @Sampling s@ is still @s@ after its current value was
    -- requested, but still not delivered
    | Sampling (SignalNode a)
    -- | @Sample x@ is just the value @x@, eventually to be replaced
    -- by the aged version of its corresponding signal
    | Sample a
    -- | @Aged x s@ is an already sampled signal, where @x@ is the
    -- current value and @s@ is the new version of the signal for the
    -- next superstep
    | Aged a (SignalNode a)

{-| The possible structures of a node are defined by the 'SignalNode'
type.  Note that the @SNLx@ nodes are only needed to optimise
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
    -- | @SNE s e ss@: latcher that starts out as @s@ and becomes the
    -- current value of @ss@ at every moment when @e@ is true
    | SNE (Signal a) (Signal Bool) (Signal (Signal a))
    -- | @SNR r@: opaque reference to connect peripherals
    | SNR (IORef a)
    -- | @SNKA s l@: equivalent to @s@ while aging each signal in @l@
    | SNKA (Signal a) [AnySignal]
    -- | @SNL1 f@: @fmap f@
    | forall t . SNL1 (t -> a) (Signal t)
    -- | @SNL2 f@: @liftA2 f@
    | forall t1 t2 . SNL2 (t1 -> t2 -> a) (Signal t1) (Signal t2)
    -- | @SNL3 f@: @liftA3 f@
    | forall t1 t2 t3 . SNL3 (t1 -> t2 -> t3 -> a) (Signal t1) (Signal t2) (Signal t3)
    -- | @SNL4 f@: @liftA4 f@
    | forall t1 t2 t3 t4 . SNL4 (t1 -> t2 -> t3 -> t4 -> a) (Signal t1) (Signal t2) (Signal t3) (Signal t4)
    -- | @SNL5 f@: @liftA5 f@
    | forall t1 t2 t3 t4 t5 . SNL5 (t1 -> t2 -> t3 -> t4 -> t5 -> a) (Signal t1) (Signal t2) (Signal t3) (Signal t4) (Signal t5)

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
    pure = createSignal . SNK
    -- | Point-wise application of a function and a data signal (like @ZipList@)
    f@(S rf) <*> x@(S rx) = unsafePerformIO $ do
      -- General fall-back case
      c <- newIORef (Ready (SNA f x))

      let opt s = writeIORef c (Ready s)

      -- Optimisations might go haywire in the presence of loops,
      -- so we need to prepare to meeting undefined references by
      -- wrapping reads into exception handlers.

      flip catch (const (return ())) $ do
        Ready nf <- readIORef rf

        merged <- flip catch (const (return False)) $ do
          -- Merging constant branches from the two sides
          Ready nx <- readIORef rx
          case (nf,nx) of
            (SNK g,SNK y)                  -> debugLog "merge_00" $ opt (SNK (g y))
            (SNK g,SNL1 h y1)              -> debugLog "merge_01" $ opt (SNL1 (g.h) y1)
            (SNK g,SNL2 h y1 y2)           -> debugLog "merge_02" $ opt (SNL2 (\y1 y2 -> g (h y1 y2)) y1 y2)
            (SNK g,SNL3 h y1 y2 y3)        -> debugLog "merge_03" $ opt (SNL3 (\y1 y2 y3 -> g (h y1 y2 y3)) y1 y2 y3)
            (SNK g,SNL4 h y1 y2 y3 y4)     -> debugLog "merge_04" $ opt (SNL4 (\y1 y2 y3 y4 -> g (h y1 y2 y3 y4)) y1 y2 y3 y4)
            (SNK g,SNL5 h y1 y2 y3 y4 y5)  -> debugLog "merge_05" $ opt (SNL5 (\y1 y2 y3 y4 y5 -> g (h y1 y2 y3 y4 y5)) y1 y2 y3 y4 y5)
            (SNK g,_)                      -> debugLog "lift_1x" $ opt (SNL1 g x)
            (SNL1 g x1,SNK y)              -> debugLog "merge_10" $ opt (SNL1 (\x1 -> g x1 y) x1)
            (SNL1 g x1,SNL1 h y1)          -> debugLog "merge_11" $ opt (SNL2 (\x1 y1 -> g x1 (h y1)) x1 y1)
            (SNL1 g x1,SNL2 h y1 y2)       -> debugLog "merge_12" $ opt (SNL3 (\x1 y1 y2 -> g x1 (h y1 y2)) x1 y1 y2)
            (SNL1 g x1,SNL3 h y1 y2 y3)    -> debugLog "merge_13" $ opt (SNL4 (\x1 y1 y2 y3 -> g x1 (h y1 y2 y3)) x1 y1 y2 y3)
            (SNL1 g x1,SNL4 h y1 y2 y3 y4) -> debugLog "merge_14" $ opt (SNL5 (\x1 y1 y2 y3 y4 -> g x1 (h y1 y2 y3 y4)) x1 y1 y2 y3 y4)
            (SNL1 g x1,_)                  -> debugLog "lift_2x" $ opt (SNL2 g x1 x)
            (SNL2 g x1 x2,SNK y)           -> debugLog "merge_20" $ opt (SNL2 (\x1 x2 -> g x1 x2 y) x1 x2)
            (SNL2 g x1 x2,SNL1 h y1)       -> debugLog "merge_21" $ opt (SNL3 (\x1 x2 y1 -> g x1 x2 (h y1)) x1 x2 y1)
            (SNL2 g x1 x2,SNL2 h y1 y2)    -> debugLog "merge_22" $ opt (SNL4 (\x1 x2 y1 y2 -> g x1 x2 (h y1 y2)) x1 x2 y1 y2)
            (SNL2 g x1 x2,SNL3 h y1 y2 y3) -> debugLog "merge_23" $ opt (SNL5 (\x1 x2 y1 y2 y3 -> g x1 x2 (h y1 y2 y3)) x1 x2 y1 y2 y3)
            (SNL2 g x1 x2,_)               -> debugLog "lift_3x" $ opt (SNL3 g x1 x2 x)
            (SNL3 g x1 x2 x3,SNK y)        -> debugLog "merge_30" $ opt (SNL3 (\x1 x2 x3 -> g x1 x2 x3 y) x1 x2 x3)
            (SNL3 g x1 x2 x3,SNL1 h y1)    -> debugLog "merge_31" $ opt (SNL4 (\x1 x2 x3 y1 -> g x1 x2 x3 (h y1)) x1 x2 x3 y1)
            (SNL3 g x1 x2 x3,SNL2 h y1 y2) -> debugLog "merge_32" $ opt (SNL5 (\x1 x2 x3 y1 y2 -> g x1 x2 x3 (h y1 y2)) x1 x2 x3 y1 y2)
            (SNL3 g x1 x2 x3,_)            -> debugLog "lift_4x" $ opt (SNL4 g x1 x2 x3 x)
            (SNL4 g x1 x2 x3 x4,SNK y)     -> debugLog "merge_40" $ opt (SNL4 (\x1 x2 x3 x4 -> g x1 x2 x3 x4 y) x1 x2 x3 x4)
            (SNL4 g x1 x2 x3 x4,SNL1 h y1) -> debugLog "merge_41" $ opt (SNL5 (\x1 x2 x3 x4 y1 -> g x1 x2 x3 x4 (h y1)) x1 x2 x3 x4 y1)
            (SNL4 g x1 x2 x3 x4,_)         -> debugLog "lift_5x" $ opt (SNL5 g x1 x2 x3 x4 x)
            (SNL5 g x1 x2 x3 x4 x5,SNK y)  -> debugLog "merge_50" $ opt (SNL5 (\x1 x2 x3 x4 x5 -> g x1 x2 x3 x4 x5 y) x1 x2 x3 x4 x5)
            _                              -> return ()
          return True

        -- Lifting into higher arity not knowing the argument
        when (not merged) $ case nf of
          SNK g              -> debugLog "lift_1" $ opt (SNL1 g x)
          SNL1 g x1          -> debugLog "lift_2" $ opt (SNL2 g x1 x)
          SNL2 g x1 x2       -> debugLog "lift_3" $ opt (SNL3 g x1 x2 x)
          SNL3 g x1 x2 x3    -> debugLog "lift_4" $ opt (SNL4 g x1 x2 x3 x)
          SNL4 g x1 x2 x3 x4 -> debugLog "lift_5" $ opt (SNL5 g x1 x2 x3 x4 x)
          _                  -> return ()

      -- The final version
      return (S c)

{-| The @Show@ instance is only defined for the sake of 'Num'... -}

instance Show (Signal a) where
    showsPrec _ _ s = "<SIGNAL>" ++ s

{-| The equality test checks whether two signals are physically the same. -}

instance Eq (Signal a) where
    S s1 == S s2 = s1 == s2

instance Num t => Num (Signal t) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    signum = fmap signum
    abs = fmap abs
    negate = fmap negate
    fromInteger = pure . fromInteger

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

{-| This function is really just a shorthand to create a reference to
a given node. -}

createSignal :: SignalNode a -> Signal a
createSignal = S . unsafePerformIO . newIORef . Ready

{-| Sampling and aging the signal and all of its dependencies, at the
same time.  We don't need the aged signal in the current superstep,
only the current value, so we sample before propagating the changes,
which might require the fresh sample because of recursive
definitions. -}

signalValue :: forall a . Signal a -> DTime -> IO a
signalValue (S r) dt = do
  t <- readIORef r
  case t of
    Ready s    -> do writeIORef r (Sampling s)
                     -- TODO: advance can be evaluated in a separate
                     -- thread, since we don't need its result right
                     -- away, only in the next superstep.
                     v <- sample s dt
                     -- We memorise the sample to handle loops
                     -- nicely.  The undefined future signal cannot
                     -- bite us, because we don't need it during the
                     -- evaluation phase.
                     writeIORef r (Sample v)
                     s' <- advance s v dt
                     writeIORef r (Aged v s')
                     return v
    Sampling s -> do -- We started sampling this already, so there is
                     -- a dependency cycle we have to resolve by
                     -- adding a delay to stateful signals. Stateless
                     -- signals should not form a loop, which is
                     -- obvious...
                     v <- sampleDelayed s dt
                     writeIORef r (Sample v)
                     -- Since we are sampling this already, aging
                     -- will be performed by the case above.  Also,
                     -- the result is memoised by the system, so we
                     -- are not calculating anything twice.  Note
                     -- that this is an old value, so it shouldn't be
                     -- used for aging anyway.
                     return v
    Sample v   -> return v
    Aged v _   -> return v

{-| Finalising the aged signals for the next round. -}

commit :: forall a . Signal a -> IO ()
commit (S s) = do
  t <- readIORef s
  case t of
    Aged _ s' -> do writeIORef s (Ready s')
                    -- TODO: branching can be trivially parallelised
                    case s' of
                      SNT s _ _             -> commit s
                      SNA sf sx             -> commit sf >> commit sx
                      SNL1 _ s              -> commit s
                      SNL2 _ s1 s2          -> commit s1 >> commit s2
                      SNL3 _ s1 s2 s3       -> commit s1 >> commit s2 >> commit s3
                      SNL4 _ s1 s2 s3 s4    -> commit s1 >> commit s2 >> commit s3 >> commit s4
                      SNL5 _ s1 s2 s3 s4 s5 -> commit s1 >> commit s2 >> commit s3 >> commit s4 >> commit s5
                      SNE s e ss            -> commit s >> commit e >> commit ss
                      SNKA s l              -> commit s >> mapM_ (\(AS s) -> commit s) l
                      _                     -> return ()
    Ready _   -> return () 
    _         -> error "Inconsistent state: signal not aged!"

{-| Aging the signal.  Stateful signals have their state forced to
prevent building up big thunks, and the latcher also does its job
here.  The other nodes are structurally static. -}

advance :: SignalNode a -> a -> DTime -> IO (SignalNode a)
advance (SNS x f)       _ dt = x `seq` return (SNS (f dt x) f)
advance (SNT s _ f)     v _  = v `seq` return (SNT s v f)
advance sw@(SNE _ e ss) _ dt = do -- These are ready samples!
                                  b <- signalValue e dt
                                  s' <- signalValue ss dt
                                  if b
                                    then return (SNE s' e ss)
                                    else return sw
advance s               _ _  = return s

{-| Sampling the signal at the current moment.  This is where static
nodes propagate changes to those they depend on.  Transfer functions
('SNT') and latchers ('SNE') work without delay, i.e. the effects of
their input signals can be observed in the same superstep. -}

sample :: SignalNode a -> DTime -> IO a
sample (SNK x)                 _  = return x
sample (SNS x _)               _  = return x
sample (SNT s x f)             dt = do t <- signalValue s dt
                                       return $! f dt t x
sample (SNA sf sx)             dt = signalValue sf dt <*> signalValue sx dt
sample (SNE s e ss)            dt = do b <- signalValue e dt
                                       s' <- signalValue ss dt
                                       signalValue (if b then s' else s) dt
sample (SNR r)                 _  = readIORef r
sample (SNKA s l)              dt = do mapM_ (\(AS s) -> signalValue s dt) l
                                       signalValue s dt
sample (SNL1 f s)              dt = f <$> signalValue s dt
sample (SNL2 f s1 s2)          dt = liftM2 f (signalValue s1 dt) (signalValue s2 dt)
sample (SNL3 f s1 s2 s3)       dt = liftM3 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt)
sample (SNL4 f s1 s2 s3 s4)    dt = liftM4 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt) (signalValue s4 dt)
sample (SNL5 f s1 s2 s3 s4 s5) dt = liftM5 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt) (signalValue s4 dt) (signalValue s5 dt)

{-| Sampling the signal with some kind of delay in order to resolve
dependency loops.  Transfer functions simply return their previous
output, while latchers postpone the change and pass through the
current value of their current signal even if the latch control signal
is true at the moment.  Other types of signals are always handled by
the `sample` function, so it is not possible to create a stateful loop
composed of solely stateless combinators. -}

sampleDelayed :: SignalNode a -> DTime -> IO a
sampleDelayed (SNT _ x _) _  = return x
sampleDelayed (SNE s _ _) dt = signalValue s dt
sampleDelayed sn          dt = sample sn dt

-- ** Userland primitives

{-| Advancing the whole network that the given signal depends on by
the amount of time given in the second argument. -}

superstep :: Signal a -- ^ the top-level signal
          -> DTime    -- ^ the amount of time to advance
          -> IO a     -- ^ the current value of the signal
superstep world dt = do
  snapshot <- signalValue world dt
  commit world
  return snapshot

{-| A pure stateful signal.  The initial state is the first output. -}

stateful :: a                 -- ^ initial state
         -> (DTime -> a -> a) -- ^ state transformation
         -> Signal a
stateful x0 f = createSignal (SNS x0 f)

{-| A stateful transfer function.  The current input affects the
current output, i.e. the initial state given in the first argument is
considered to appear before the first output, and can only be directly
observed by the `sampleDelayed` function. -}

transfer :: a                      -- ^ initial internal state
         -> (DTime -> t -> a -> a) -- ^ state updater function
         -> Signal t               -- ^ input signal
         -> Signal a
transfer x0 f s = createSignal (SNT s x0 f)

{-| Reactive signal that starts out as @s@ and can change its
behaviour to the one supplied in @ss@ whenever @e@ is true. The change
can be observed immediately, unless the signal is sampled by
`sampleDelayed`, which puts a delay on the latch control (but not on
the latched signal!). -}

latcher :: Signal a          -- ^ @s@: initial behaviour
        -> Signal Bool       -- ^ @e@: latch control signal
        -> Signal (Signal a) -- ^ @ss@: signal of potential future behaviours
        -> Signal a
latcher s e ss = createSignal (SNE s e ss)

{-| A signal that can be directly fed through the sink function
returned. This can be used to attach the network to the outer
world. -}

external :: a                     -- ^ initial value
         -> IO (Signal a, Sink a) -- ^ the signal and an IO function to feed it
external x0 = do
  ref <- newIORef x0
  snr <- newIORef (Ready (SNR ref))
  return (S snr,writeIORef ref)

{-| Dependency injection to allow aging signals whose output is not
necessarily needed to produce the current sample of the first
argument. -}

keepAlive :: Signal a    -- ^ the actual output
          -> [AnySignal] -- ^ a list of signals guaranteed to age when this one is sampled
          -> Signal a
keepAlive s l = createSignal (SNKA s l)
