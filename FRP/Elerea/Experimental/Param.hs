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

-- | A signal is represented by a sampling computation.
newtype Signal p a = S { unS :: p -> IO a }

-- | A dynamic set of actions to update a network without breaking
-- | consistency.
type UpdatePool p = [Weak (p -> IO (), IO ())]

-- | A signal generator computes a signal structure and adds the new
-- | variables to an existing update pool.
newtype SignalGen p a = SG { unSG :: IORef (UpdatePool p) -> IO a }

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

createSampler :: SignalGen p (Signal p a) -> IO (p -> IO a)
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

addSignal :: (p -> Phase s a -> IO a) -> (p -> Phase s a -> IO ()) -> IORef (Phase s a)
          -> IORef (UpdatePool p) -> IO (Signal p a)
addSignal sample age ref pool = do
  let  commit (Aged s _)  = Ready s
       commit _           = error "commit error: signal not aged"

  update <- mkWeakRef ref (\p -> readIORef ref >>= age p, modifyIORef ref commit) Nothing
  modifyIORef pool (update:)
  return (S $ \p -> readIORef ref >>= sample p)

delay :: a -> Signal p a -> SignalGen p (Signal p a)
delay x0 (S s) = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let  sample _ (Ready x)   = return x
       sample _ (Aged _ x)  = return x

       age p (Ready x)  = s p >>= \x' -> x' `seq` writeIORef ref (Aged x' x)
       age _ _          = return ()

  addSignal sample age ref pool

memo :: Signal p a -> SignalGen p (Signal p a)
memo (S s) = SG $ \pool -> do
  ref <- newIORef (Ready undefined)

  let  sample p (Ready _)      = s p >>= \x -> writeIORef ref (Aged undefined x) >> return x
       sample _ (Aged _ x)     = return x

       age p (Ready _)      = s p >>= \x -> writeIORef ref (Aged undefined x)
       age _ _              = return ()

  addSignal sample age ref pool

generator :: Signal p Bool -> (SignalGen p a) -> Signal p (SignalGen p a) -> SignalGen p (Signal p a)
generator (S ctr) (SG gen0) (S gen) = SG $ \pool -> do
  ref <- newIORef . Ready =<< gen0 pool

  let  next p x = ctr p >>= \b -> if b then ($pool).unSG =<< gen p else return x

       sample p (Ready x)      = next p x >>= \x' -> writeIORef ref (Aged x' x') >> return x'
       sample _ (Aged _ x)     = return x

       age p (Ready x)      = next p x >>= \x' -> writeIORef ref (Aged x' x')
       age _ _              = return ()

  addSignal sample age ref pool

external :: a -> IO (Signal p a, a -> IO ())
external x = do
  ref <- newIORef x
  return (S (const (readIORef ref)), writeIORef ref)

stateful :: a -> (p -> a -> a) -> SignalGen p (Signal p a)
stateful x0 f = SG $ \pool -> do
  ref <- newIORef (Ready x0)

  let  sample _ (Ready x)   = return x
       sample _ (Aged _ x)  = return x

       age p (Ready x)  = let x' = f p x in x' `seq` writeIORef ref (Aged x' x)
       age _ _          = return ()

  addSignal sample age ref pool

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

{-
counter x0 = stateful x0 (+)

integral x0 = transfer x0 (\dt x' x -> x+x'*dt)

--trigs :: SignalGen Double (Signal Double Double)
trigs = mdo
  sine <- integral 0 cosine
  cosine <- delay 1 =<< integral 1 (negate <$> sine)
  return $ do s <- sine
              c <- cosine
              return $ s*s+c*c

sigtest s = replicateM 20 =<< (($ 0.1) <$> createSampler s)

ctrs = do
  c1 <- counter 0
  c2 <- counter 10
  let f x = if x `mod` 5 < 3 then c1 else c2
  return $ join (f <$> c1)

dctrs = mdo
  cs0 <- mapM counter [2,8,5]
  cs <- delay cs0 cs'
  let cvals = join (sequenceA <$> cs)
      cs' = map snd . filter ((<15).fst) <$> (zip <$> cvals <*> cs)
  return cvals

cres = do
  ctr <- counter 0
  let f x = x `mod` 5 == 3
  fmap join $ generator (f <$> ctr) (counter 100) (counter . (*2) <$> ctr)

main = do s <- createSampler trigs
          replicateM_ 1000000 (s 0.000001)
          print =<< (s 0.000001)

--main = print =<< (sigtest $ integral 10 (pure 2))
-}