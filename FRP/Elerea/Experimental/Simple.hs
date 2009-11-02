{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FRP.Elerea.Experimental.Simple
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

-- | A signal is represented by a sampling computation.
newtype Signal a = S (IO a)
    deriving (Functor, Applicative, Monad)

-- | A dynamic set of actions to update a network without breaking
-- | consistency.
type UpdatePool = [Weak (IO (),IO ())]

-- | A signal generator computes a signal structure and adds the new
-- | variables to an existing update pool.
newtype SignalGen a = SG { unSG :: IORef UpdatePool -> IO a }

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

createSampler :: SignalGen (Signal a) -> IO (IO a)
createSampler (SG gen) = do
  pool <- newIORef []
  (S sample) <- gen pool
  return $ do
    --performGC
    res <- sample
    let deref ptr = (fmap.fmap) ((,) ptr) (deRefWeak ptr)
    (ptrs,acts) <- unzip.catMaybes <$> (mapM deref =<< readIORef pool)
    --let deref trp@(chk,_) = (,) trp <$> chk
    --ptrs <- map fst . filter snd <$> (mapM deref =<< readIORef pool)
    writeIORef pool ptrs
    --print $ length ptrs
    mapM_ fst acts
    mapM_ snd acts
    return res

addSignal :: (a -> IO a) -> (a -> IO ()) -> IORef (Phase a) -> IORef UpdatePool -> IO (Signal a)
addSignal sample age ref pool = do
  let  sample' (Ready x)    = sample x
       sample' (Aged _ x)   = return x

       age' (Ready x)    = age x
       age' _            = return ()

       commit (Aged x _)  = Ready x
       commit _           = error "commit error: signal not aged"

  update <- mkWeakRef ref (age' =<< readIORef ref,modifyIORef ref commit) Nothing
  modifyIORef pool (update:)
  return (S $ sample' =<< readIORef ref)

delay :: a -> Signal a -> SignalGen (Signal a)
delay x0 (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let age x = s >>= \x' -> x' `seq` writeIORef ref (Aged x' x)

  addSignal return age ref pool

memo :: Signal a -> SignalGen (Signal a)
memo (S s) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)

  let  sample _ = s >>= \x -> writeIORef ref (Aged undefined x) >> return x       
       age _ = writeIORef ref . Aged undefined =<< s

  addSignal sample age ref pool

generator :: Signal Bool -> (SignalGen a) -> Signal (SignalGen a) -> SignalGen (Signal a)
generator (S ctr) (SG gen0) (S gen) = SG $ \pool -> do
  ref <- newIORef . Ready =<< gen0 pool

  let  next x = ctr >>= \b -> if b then ($pool).unSG =<< gen else return x
       sample x = next x >>= \x' -> writeIORef ref (Aged x' x') >> return x'
       age x = next x >>= \x' -> writeIORef ref (Aged x' x')

  addSignal sample age ref pool

external :: a -> IO (Signal a, a -> IO ())
external x = do
  ref <- newIORef x
  return (S (readIORef ref), writeIORef ref)

stateful :: a -> (a -> a) -> SignalGen (Signal a)
stateful x0 f = mfix $ \sig -> delay x0 (f <$> sig)

transfer :: a -> (t -> a -> a) -> Signal t -> SignalGen (Signal a)
transfer x0 f s = mfix $ \sig -> liftA2 f s <$> delay x0 sig
