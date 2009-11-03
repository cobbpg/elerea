module FRP.Elerea.Experimental.Param
    ( Signal
    , SignalGen
    , createSampler
    , external
    , delay
    , stateful
    , transfer
    , memo
    , generator
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Maybe
import System.Mem.Weak

import FRP.Elerea.Experimental.WeakRef

-- | A signal is represented by a sampling computation that takes a
-- global parameter.
newtype Signal p a = S { unS :: p -> IO a }

-- | A dynamic set of actions to update a network without breaking
-- consistency.
type UpdatePool p = [Weak (p -> IO (), IO ())]

-- | A signal generator computes a signal structure and adds the new
-- variables to an existing update pool, where update actions receive
-- a global parameter.
newtype SignalGen p a = SG { unSG :: IORef (UpdatePool p) -> IO a }

-- | The phases every signal goes through during a superstep: before
-- or after sampling.
data Phase s a = Ready s | Aged s a

instance Functor (Signal p) where
    fmap = (<*>).pure

instance Applicative (Signal p) where
    pure = return
    (<*>) = ap

instance Monad (Signal p) where
    return = S . const . return
    S g >>= f = S $ \p -> g p >>= \x -> unS (f x) p

instance Functor (SignalGen p) where
    fmap = (<*>).pure

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
-- the current sample of the top-level signal is produced as a result.
-- The computation accepts a global parameter that will be distributed
-- to all signals.  For instance, this can be the time step, if we
-- want to model continuous-time signals.
createSampler :: SignalGen p (Signal p a) -- ^ the generator of the top-level signal
              -> IO (p -> IO a)           -- ^ the computation to sample the signal
createSampler (SG gen) = do
  pool <- newIORef []
  (S sample) <- gen pool
  return $ \param -> do
    res <- sample param
    let deref ptr = (fmap.fmap) ((,) ptr) (deRefWeak ptr)
    (ptrs,acts) <- unzip.catMaybes <$> (mapM deref =<< readIORef pool)
    writeIORef pool ptrs
    mapM_ (($param).fst) acts
    mapM_ snd acts
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

  update <- mkWeakRef ref (\p -> readIORef ref >>= age p, modifyIORef ref commit) Nothing
  modifyIORef pool (update:)
  return (S $ \p -> readIORef ref >>= sample p)

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
-- applicative combinators in case they are used in several places.
-- Other than that, it is equivalent to 'return'.
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
-- carried by its input when a boolean control signal is true,
-- otherwise it repeats its previous output.  It is possible to create
-- new signals in the monad.
generator :: Signal p Bool            -- ^ control (trigger) signal
          -> (SignalGen p a)          -- ^ the generator of the initial output
          -> Signal p (SignalGen p a) -- ^ a stream of generators to potentially run
          -> SignalGen p (Signal p a)
generator (S ctr) (SG gen0) (S gen) = SG $ \pool -> do
  ref <- newIORef . Ready =<< gen0 pool

  let  next p x = ctr p >>= \b -> if b then ($pool).unSG =<< gen p else return x

       sample p (Ready x)      = next p x >>= \x' -> writeIORef ref (Aged x' x') >> return x'
       sample _ (Aged _ x)     = return x

       age p (Ready x)      = next p x >>= \x' -> writeIORef ref (Aged x' x')
       age _ _              = return ()

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

  let  sample _ (Ready x)   = return x
       sample _ (Aged _ x)  = return x

       age p (Ready x)  = let x' = f p x in x' `seq` writeIORef ref (Aged x' x)
       age _ _          = return ()

  addSignal sample age ref pool

-- | A stateful transfer function.  The current input affects the
-- current output, i.e. the initial state given in the first argument
-- is considered to appear before the first output, and can never be
-- observed.  Every output is derived from the current value of the
-- input signal, the global parameter and the previous output.
transfer :: a -> (p -> t -> a -> a) -> Signal p t -> SignalGen p (Signal p a)
transfer x0 f (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let  sample p (Ready x)   = s p >>= \y -> let x' = f p y x in
                                            x' `seq` writeIORef ref (Aged x' x') >> return x'
       sample _ (Aged _ x)  = return x

       age p (Ready x)  = s p >>= \y -> let x' = f p y x in
                                        x' `seq` writeIORef ref (Aged x' x')
       age _ _          = return ()

  addSignal sample age ref pool
