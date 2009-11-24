{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

This module provides efficient higher-order discrete signals.  For a
motivating example, let's create a dynamic collection of countdown
timers, where each expired timer is removed from the collection.
First of all, we'll need a simple tester function:

@
 sigtest gen = 'replicateM' 20 '=<<' 'start' gen
@

We can try it with a simple example:

@
 \> sigtest $ 'stateful' 2 (+3)
 [2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47]
@

Our first definition will be a signal representing a simple named
timer:

@
 countdown :: String -\> Int -\> SignalGen (Signal (String,Maybe Int))
 countdown name t = do
   let tick prev = do { t \<- prev ; 'guard' (t \> 0) ; 'return' (t-1) }
   timer \<- 'stateful' (Just t) tick
   'return' ((,) name '<$>' timer)
@

We can easily test it:

@
 \> sigtest $ countdown \"foo\" 4
 [(\"foo\",Just 4),(\"foo\",Just 3),(\"foo\",Just 2),(\"foo\",Just 1),
  (\"foo\",Just 0),(\"foo\",Nothing),(\"foo\",Nothing),...]
@

Next, we will define a timer source that takes a list of timer names,
starting values and start times and creates a signal that delivers the
list of new timers at every point:

@
 timerSource :: [(String, Int, Int)] -\> SignalGen (Signal [Signal (String, Maybe Int)])
 timerSource ts = do
   let gen t = 'mapM' ('uncurry' countdown) newTimers
           where newTimers = [(n,v) | (n,v,st) \<- ts, st == t]
   cnt \<- 'stateful' 0 (+1)
   'return' $ 'generator' (gen '<$>' cnt)
@

Now we need to encapsulate the timer source signal in another signal
expression that takes care of maintaining the list of live timers.
Since working with dynamic collections is a recurring task, let's
define a generic combinator that maintains a dynamic list of signals
given a source and a test that tells from the output of each signal
whether it should be kept.  We can use @mdo@ expressions (a variant of
@do@ expressions allowing forward references) as syntactic sugar for
'mfix' to make life easier:

@
 collection :: Signal [Signal a] -\> (a -\> Bool) -\> SignalGen (Signal [a])
 collection source isAlive = mdo
   coll \<- 'liftA2' (++) source '<$>' 'delay' [] coll'
   let  collWithVals = 'zip' '<$>' ('sequence' '=<<' coll) '<*>' coll
        collWithVals' = 'filter' (isAlive.'fst') '<$>' collWithVals
        coll' = 'map' 'snd' '<$>' collWithVals'
   'return' $ 'map' 'fst' '<$>' collWithVals'
@

We need recursion to define the @coll@ signal as a delayed version of
@coll'@, which represents its continuation.  At every point of time
its output is concatenated with that of the source (we need to lift
the (++) operator twice in order to get behind both the 'SignalGen'
and the 'Signal' abstraction).  Then we define @collWithVals@, which
simply pairs up every signal with its current output.  The output is
obtained by extracting the current value of the signal container and
sampling each element with 'sequence'.  We can then derive
@collWithVals'@, which contains only the signals that must be kept for
the next round along with their output.  By throwing out the
respective parts, we can get both the final output and the collection
for the next step (@coll'@).

Now we can easily finish the original task:

@
 timers :: [(String, Int, Int)] -\> SignalGen (Signal [(String, Int)])
 timers timerData = do
   src \<- timerSource timerData
   getOutput '<$>' collection src (isJust.'snd')
     where getOutput = 'fmap' ('map' (\(name,Just val) -> (name,val)))
@

As a test, we can start four timers: /a/ at t=0 with value 3, /b/ and
/c/ at t=1 with values 5 and 3, and /d/ at t=3 with value 4:

@
 \> sigtest $ timers [(\"a\",3,0),(\"b\",5,1),(\"c\",3,1),(\"d\",4,3)]
 [[(\"a\",3)],[(\"b\",5),(\"c\",3),(\"a\",2)],[(\"b\",4),(\"c\",2),(\"a\",1)],
  [(\"d\",4),(\"b\",3),(\"c\",1),(\"a\",0)],[(\"d\",3),(\"b\",2),(\"c\",0)],
  [(\"d\",2),(\"b\",1)],[(\"d\",1),(\"b\",0)],[(\"d\",0)],[],[],[],[],[],[],[],[]]
@

If the noise of the applicative lifting operators feels annoying, she
(<http://personal.cis.strath.ac.uk/~conor/pub/she/>) comes to the
save.  Among other features it provides idiom brackets, which can
substitute the explicit lifting.  For instance, it allows us to define
@collection@ this way:

@
 collection :: Stream [Stream a] -> (a -> Bool) -> StreamGen (Stream [a])
 collection source isAlive = mdo
   sig <- 'delay' [] coll'
   let  coll = (|source ++ sig|)
        collWithVals = (|'zip' ('sequence' '=<<' coll) coll|)
        collWithVals' = (|'filter' ~(isAlive.'fst') collWithVals|)
        coll' = (|'map' ~'snd' collWithVals'|)
   'return' (|'map' ~'fst' collWithVals'|)
@

-}

module FRP.Elerea.Experimental.Simple
    ( Signal
    , SignalGen
    , start
    , external
    , delay
    , generator
    , memo
    , stateful
    , transfer
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Maybe
import System.Mem.Weak

{-| A signal can be thought of as a function of type @Nat -> a@, where
the argument is the sampling time, and the 'Monad' instance agrees
with the intuition (bind corresponds to extracting the current
sample). -}

newtype Signal a = S (IO a) deriving (Functor, Applicative, Monad)

{-| A dynamic set of actions to update a network without breaking
consistency. -}

type UpdatePool = [Weak (IO (),IO ())]

{-| A signal generator is the only source of stateful signals.  It can
be thought of as a function of type @Nat -> a@, where the result is an
arbitrary data structure that can potentially contain new signals, and
the argument is the creation time of these new signals.  It exposes
the 'MonadFix' interface, which makes it possible to define signals in
terms of each other. -}

newtype SignalGen a = SG { unSG :: IORef UpdatePool -> IO a }

{-| The phases every signal goes through during a superstep. -}

data Phase a = Ready a | Updated a a

instance Functor SignalGen where
    fmap = (<*>).pure

instance Applicative SignalGen where
    pure = return
    (<*>) = ap

instance Monad SignalGen where
    return = SG . const . return
    SG g >>= f = SG $ \p -> g p >>= \x -> unSG (f x) p

instance MonadFix SignalGen where
    mfix f = SG $ \p -> mfix (($p).unSG.f)

{-| Embedding a signal into an 'IO' environment.  Repeated calls to
the computation returned cause the whole network to be updated, and
the current sample of the top-level signal is produced as a result.
This is the only way to extract a signal generator outside the
network, and it is equivalent to passing zero to the function
representing the generator. -}

start :: SignalGen (Signal a) -- ^ the generator of the top-level signal
      -> IO (IO a)            -- ^ the computation to sample the signal
start (SG gen) = do
  pool <- newIORef []
  S sample <- gen pool
  return $ do
    let deref ptr = (fmap.fmap) ((,) ptr) (deRefWeak ptr)
    res <- sample
    (ptrs,acts) <- unzip.catMaybes <$> (mapM deref =<< readIORef pool)
    writeIORef pool ptrs
    mapM_ fst acts
    mapM_ snd acts
    return res

{-| Auxiliary function used by all the primitives that create a
mutable variable. -}

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

{-| The 'delay' transfer function emits the value of a signal from the
previous superstep, starting with the filler value given in the first
argument.  It can be thought of as the following function (which
should also make it clear why the return value is 'SignalGen'):

@
 delay x0 s t_start t_sample
   | t_start == t_sample = x0
   | t_start < t_sample  = s (t_sample-1)
   | otherwise           = 'error' \"Premature sample!\"
@

The way signal generators are extracted ensures that the error can
never happen. -}

delay :: a                    -- ^ initial output at creation time
      -> Signal a             -- ^ the signal to delay
      -> SignalGen (Signal a) -- ^ the delayed signal
delay x0 (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let update x = s >>= \x' -> x' `seq` writeIORef ref (Updated x' x)

  addSignal return update ref pool

{-| A reactive signal that takes the value to output from a signal
generator carried by its input with the sampling time provided as the
time of generation.  It is possible to create new signals in the
monad.  It can be thought of as the following function:

@
 generator g t_start t_sample = g t_sample t_sample
@

It has to live in the 'SignalGen' monad, because it needs to maintain
an internal state to be able to cache the current sample for
efficiency reasons. However, this state is not carried between
samples, therefore starting time doesn't matter and can be ignored.

-}

generator :: Signal (SignalGen a) -- ^ the signal of generators to run
          -> SignalGen (Signal a) -- ^ the signal of generated structures
generator (S s) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)
  
  let sample = do  SG g <- s
                   x <- g pool
                   writeIORef ref (Updated undefined x)
                   return x

  addSignal (const sample) (const (sample >> return ())) ref pool

{-| Memoising combinator.  It can be used to cache results of
applicative combinators in case they are used in several places.  It
is observationally equivalent to 'return' in the 'SignalGen' monad. -}

memo :: Signal a             -- ^ the signal to cache
     -> SignalGen (Signal a) -- ^ a signal observationally equivalent to the argument
memo (S s) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)

  let sample = s >>= \x -> writeIORef ref (Updated undefined x) >> return x

  addSignal (const sample) (const (sample >> return ())) ref pool

{-| A signal that can be directly fed through the sink function
returned.  This can be used to attach the network to the outer
world. -}

external :: a                         -- ^ initial value
         -> IO (Signal a, a -> IO ()) -- ^ the signal and an IO function to feed it
external x = do
  ref <- newIORef x
  return (S (readIORef ref), writeIORef ref)

{-| A pure stateful signal.  The initial state is the first output. It
is equivalent to the following expression:

@
 stateful x0 f = 'mfix' $ \sig -> 'delay' x0 (f '<$>' sig)
@
-}

stateful :: a                    -- ^ initial state
         -> (a -> a)             -- ^ state transformation
         -> SignalGen (Signal a)
stateful x0 f = mfix $ \sig -> delay x0 (f <$> sig)

{-| A stateful transfer function.  The current input affects the
current output, i.e. the initial state given in the first argument is
considered to appear before the first output, and can never be
observed.  It is equivalent to the following expression:

@
 transfer x0 f s = 'mfix' $ \sig -> 'liftA2' f s '<$>' 'delay' x0 sig
@
-}

transfer :: a                    -- ^ initial internal state
         -> (t -> a -> a)        -- ^ state updater function
         -> Signal t             -- ^ input signal
         -> SignalGen (Signal a)
transfer x0 f s = mfix $ \sig -> liftA2 f s <$> delay x0 sig

{-

The interface of this module differs from the old Elerea in the
following ways:

* there is no delta time argument, as this module provides discrete
  signals (see "FRP.Elerea.Experimental.Param" for a version that
  recovers this argument), so time step can only be provided as an
  'external' signal;

* there is no 'sampler' any more, it is substituted by 'join', as
  signals are monads;

* there is no automatic delay in order to preserve semantic soundness
  (e.g. the monad laws for signals);

* all signals are aged regardless of whether they are sampled
  (i.e. their behaviour doesn't depend on the context any more);

* the user needs to cache the results of applicative operations to be
  reused in multiple places explicitly using the 'memo' combinator.

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Maybe
import System.Mem
import System.Mem.Weak

--import FRP.Elerea.Experimental.WeakRef

{-| A signal can be thought of as a function of type @Nat -> a@, and
its 'Monad' instance agrees with that intuition.  Internally, is
represented by a sampling computation. -}

newtype Signal a = S (IO a)
    deriving (Functor, Applicative, Monad)

{-| A dynamic set of actions to update a network without breaking
consistency. -}

type UpdatePool = [Weak (IO (),IO ())]

{-| A signal generator is the only source of stateful signals.
Internally, computes a signal structure and adds the new variables to
an existing update pool. -}

newtype SignalGen a = SG { unSG :: IORef UpdatePool -> IO a }

{-| The phases every signal goes through during a superstep. -}

data Phase a = Ready a | Aged a a

instance Functor SignalGen where
    fmap = (<*>).pure

instance Applicative SignalGen where
    pure = return
    (<*>) = ap

instance Monad SignalGen where
    return = SG . const . return
    SG g >>= f = SG $ \p -> g p >>= \x -> unSG (f x) p

instance MonadFix SignalGen where
    mfix f = SG $ \p -> mfix (($p).unSG.f)

{-| Embedding a signal into an 'IO' environment.  Repeated calls to
the computation returned cause the whole network to be updated, and
the current sample of the top-level signal is produced as a result. -}

createSampler :: SignalGen (Signal a) -- ^ the generator of the top-level signal
              -> IO (IO a)            -- ^ the computation to sample the signal
createSampler (SG gen) = do
  pool <- newIORef []
  (S sample) <- gen pool
  return $ do
    let deref ptr = (fmap.fmap) ((,) ptr) (deRefWeak ptr)
    performGC
    res <- sample
    (ptrs,acts) <- unzip.catMaybes <$> (mapM deref =<< readIORef pool)
    writeIORef pool ptrs
    mapM_ fst acts
    mapM_ snd acts
    return res

{-| Auxiliary function used by all the primitives that create a
mutable variable. -}

addSignal :: (a -> IO a)      -- ^ sampling function
          -> (a -> IO ())     -- ^ aging function
          -> IORef (Phase a)  -- ^ the mutable variable behind the signal
          -> IORef UpdatePool -- ^ the pool of update actions
          -> IO (Signal a)
addSignal sample age ref pool = do
  let  sample' (Ready x)    = sample x
       sample' (Aged _ x)   = return x

       age' (Ready x)    = age x
       age' _            = return ()

       commit (Aged x _)  = Ready x
       commit _           = error "commit error: signal not aged"

       sig = S $ sample' =<< readIORef ref
  
  update <- mkWeak sig (age' =<< readIORef ref,modifyIORef ref commit) Nothing
  modifyIORef pool (update:)
  return sig

{-| The 'delay' transfer function emits the value of a signal from the
previous superstep, starting with the filler value given in the first
argument. -}

delay :: a                    -- ^ initial output
      -> Signal a             -- ^ the signal to delay
      -> SignalGen (Signal a)
delay x0 (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let age x = s >>= \x' -> x' `seq` writeIORef ref (Aged x' x)

  addSignal return age ref pool

{-| Memoising combinator.  It can be used to cache results of
applicative combinators in case they are used in several places.  It
is observationally equivalent to 'return'. -}

memo :: Signal a             -- ^ signal to memoise
     -> SignalGen (Signal a)
memo (S s) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)

  let  sample _ = s >>= \x -> writeIORef ref (Aged undefined x) >> return x       
       age _ = writeIORef ref . Aged undefined =<< s

  addSignal sample age ref pool

{-| A reactive signal that takes the value to output from a monad
carried by its input when a boolean control signal is true, otherwise
it repeats its previous output.  It is possible to create new signals
in the monad. -}

generator :: Signal Bool          -- ^ control (trigger) signal
          -> (SignalGen a)        -- ^ the generator of the initial output
          -> Signal (SignalGen a) -- ^ a signal of generators to potentially run
          -> SignalGen (Signal a)
generator (S ctr) (SG gen0) (S gen) = SG $ \pool -> do
  ref <- newIORef . Ready =<< gen0 pool

  let  next x = ctr >>= \b -> if b then ($pool).unSG =<< gen else return x
       sample x = next x >>= \x' -> writeIORef ref (Aged x' x') >> return x'
       age x = next x >>= \x' -> writeIORef ref (Aged x' x')

  addSignal sample age ref pool

{-| A signal that can be directly fed through the sink function
returned.  This can be used to attach the network to the outer
world. -}

external :: a                         -- ^ initial value
         -> IO (Signal a, a -> IO ()) -- ^ the signal and an IO function to feed it
external x = do
  ref <- newIORef x
  return (S (readIORef ref), writeIORef ref)

-}