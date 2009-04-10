{-# LANGUAGE ExistentialQuantification #-}
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
current ones, ready to be sampled again.  Evaluation is done by the
'signalValue' function, while finalisation is done by 'commit'.  Since
these functions are invoked recursively on a data structure with
existential types, their types also need to be explicity quantified.

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

{-| A node can have two states: stable (freshly created or finalised)
or mutating (in the process of aging). -}

data SignalTrans a
    -- | @Cur s@ is simply the signal @s@
    = Cur (SignalNode a)
    -- | @Tra x s@ is an already sampled signal, where @x@ is the
    -- current value and @s@ is the new version of the signal
    | Tra a (SignalNode a)

{-| The possible structures of a node are defined by the 'SignalNode'
type.  Note that the @SNLx@ nodes are only needed to optimise
applicatives, they can all be expressed in terms of @SNK@ and
@SNA@. -}

data SignalNode a
    -- | @SNK x@: constantly @x@
    = SNK a
    -- | @SNF f@: time function @f@ (absolute time)
    | SNF (Time -> a)
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
      c <- newIORef (Cur (SNA f x))

      let opt s = writeIORef c (Cur s)

      -- Optimisations might go haywire in the presence of loops,
      -- so we need to prepare to meeting undefined references by
      -- wrapping reads into exception handlers.

      flip catch (const (return ())) $ do
        Cur nf <- readIORef rf

        merged <- flip catch (const (return False)) $ do
          -- Merging constant branches from the two sides
          Cur nx <- readIORef rx
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

{-| The equality test checks whether to signals are physically the same. -}

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

-- ** Internal functions to run the network

{-| This function is really just a shorthand to create a reference to
a given node. -}

createSignal :: SignalNode a -> Signal a
createSignal = S . unsafePerformIO . newIORef . Cur

{-| Sampling and aging the signal and all of its dependencies, at the
same time.  We don't need the aged signal in the current superstep,
only the current value, so we sample before propagating the changes,
which might require the fresh sample because of recursive
definitions. -}

signalValue :: forall a . Signal a -> DTime -> IO a
signalValue (S r) dt = do
  t <- readIORef r
  case t of
    Cur s   -> do -- TODO: advance can be evaluated in a separate
                  -- thread, since we don't need its result right away,
                  -- only in the next superstep.
                  v <- sample s dt
                  -- We memorise the sample to handle loops nicely.
                  -- The undefined future signal cannot bite us,
                  -- because we don't need it during the evaluation
                  -- phase.
                  writeIORef r (Tra v undefined)
                  s' <- advance s dt
                  writeIORef r (Tra v s')
                  return v
    Tra v _ -> return v

{-| Finalising the aged signals for the next round. -}

commit :: forall a . Signal a -> IO ()
commit (S s) = do
  t <- readIORef s
  case t of
    Tra _ s' -> do writeIORef s (Cur s')
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
                     _                     -> return ()
    _        -> return () 

{-| Aging the signal.  Stateful signals have their state forced to
prevent building up big thunks, and the latcher also does its job
here.  The other nodes are structurally static. -}

advance :: SignalNode a -> DTime -> IO (SignalNode a)
advance (SNS x f)       dt = x `seq` return (SNS (f dt x) f)
advance (SNT s x f)     dt = x `seq` do t <- signalValue s dt
                                        return (SNT s (f dt t x) f)
advance sw@(SNE _ e ss) dt = do b <- signalValue e dt
                                s' <- signalValue ss dt
                                if b
                                  then return (SNE s' e ss)
                                  else return sw
advance s               _  = return s

{-| Sampling the signal at the current moment.  This is where static
nodes propagate changes to those they depend on.  Note the latcher
rule ('SNE'): the signal is sampled before latching takes place,
therefore even if the change is instantaneous, its effect cannot be
observed at the moment of latching.  This is needed to prevent
dependency loops and make recursive definitions involving latching
possible.  The stateful signals 'SNS' and 'SNT' are similar, although
it is only the transfer function where it matters that the input
signal cannot affect the current output, only the next one. -}

sample :: SignalNode a -> DTime -> IO a
sample (SNK x)                 _  = return x
sample (SNF f)                 _  = f <$> readIORef timeRef
sample (SNS x _)               _  = return x
sample (SNT _ x _)             _  = return x
sample (SNA sf sx)             dt = signalValue sf dt <*> signalValue sx dt
sample (SNE s _ _)             dt = signalValue s dt
sample (SNR r)                 _  = readIORef r
sample (SNL1 f s)              dt = f <$> signalValue s dt
sample (SNL2 f s1 s2)          dt = liftM2 f (signalValue s1 dt) (signalValue s2 dt)
sample (SNL3 f s1 s2 s3)       dt = liftM3 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt)
sample (SNL4 f s1 s2 s3 s4)    dt = liftM4 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt) (signalValue s4 dt)
sample (SNL5 f s1 s2 s3 s4 s5) dt = liftM5 f (signalValue s1 dt) (signalValue s2 dt) (signalValue s3 dt) (signalValue s4 dt) (signalValue s5 dt)

{-| The actual variable that keeps track of global time. -}

{-# NOINLINE timeRef #-}
timeRef :: IORef Time
timeRef = unsafePerformIO (newIORef 0)

-- ** Userland primitives

{-| Advancing the whole network that the given signal depends on by
the amount of time given in the second argument. Note that the shared
'time' signal is also advanced, so this function should only be used
for sampling the top level. -}

superstep :: Signal a -- ^ the top-level signal
          -> DTime    -- ^ the amount of time to advance
          -> IO a     -- ^ the value of the signal before the update
superstep world dt = do
  snapshot <- signalValue world dt
  commit world
  t <- readIORef timeRef
  let t' = t+dt
  writeIORef timeRef $! t'
  return snapshot

{-| The global time. -}

{-# NOINLINE time #-}
time :: Signal Time
time = createSignal (SNR timeRef)

{-| A pure time function. -}

stateless :: (Time -> a) -- ^ the function to wrap
          -> Signal a
stateless = createSignal . SNF

{-| A pure stateful signal. -}

stateful :: a                 -- ^ initial state
         -> (DTime -> a -> a) -- ^ state transformation
         -> Signal a
stateful x0 f = createSignal (SNS x0 f)

{-| A stateful transfer function.  The current input can only affect
the next output, i.e. there is an implicit delay. -}

transfer :: a                      -- ^ initial state
         -> (DTime -> t -> a -> a) -- ^ state updater function
         -> Signal t               -- ^ input signal
         -> Signal a
transfer x0 f s = createSignal (SNT s x0 f)

{-| Reactive signal that starts out as @s@ and can change its
behaviour to the one supplied in @ss@ whenever @e@ is true. The change
can only be observed in the next instant. -}

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
  snr <- newIORef (Cur (SNR ref))
  return (S snr,writeIORef ref)
