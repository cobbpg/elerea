{-# LANGUAGE ExistentialQuantification #-}

module FRP.Elerea.Graph (signalToDot) where

import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import Foreign.Ptr
import Foreign.StablePtr
import FRP.Elerea.Internal

type Id = Int

type SignalStore = Map.Map Id SignalInfo

data SignalInfo
    = Const
    | Stateful
    | Transfer Id
    | App Id Id
    | Latcher Id Id Id
    | External
    | Lift1 Id
    | Lift2 Id Id
    | Lift3 Id Id Id
    | Lift4 Id Id Id Id
    | Lift5 Id Id Id Id Id
    | None

getPtr :: a -> IO Id
getPtr x = fmap (fromIntegral . ptrToIntPtr . castStablePtrToPtr) (newStablePtr x)

buildStore :: SignalStore -> Signal a -> IO (Id,SignalStore)
buildStore st (S r) = do
  p <- getPtr r
  case Map.lookup p st of
    Just _  -> return (p,st)
    Nothing -> do Ready s <- readIORef r  
                  st' <- insertSignal st p s
                  return (p,st')

insertSignal :: SignalStore -> Id -> SignalNode a -> IO SignalStore
insertSignal st p (SNK _) = return (Map.insert p Const st)
insertSignal st p (SNS _ _) = return (Map.insert p Stateful st)
insertSignal st p (SNT s _ _) = do
  (s',st') <- buildStore (Map.insert p None st) s
  return (Map.insert p (Transfer s') st')
insertSignal st p (SNA sf sx) = do
  (sf',st') <- buildStore (Map.insert p None st) sf
  (sx',st'') <- buildStore st' sx
  return (Map.insert p (App sf' sx') st'')
insertSignal st p (SNE s e ss) = do
  (s',st') <- buildStore (Map.insert p None st) s
  (e',st'') <- buildStore st' e
  (ss',st''') <- buildStore st'' ss
  return (Map.insert p (Latcher s' e' ss') st''')
insertSignal st p (SNR _) = return (Map.insert p External st)
insertSignal st p (SNL1 _ s1) = do
  (s1',st') <- buildStore (Map.insert p None st) s1
  return (Map.insert p (Lift1 s1') st')
insertSignal st p (SNL2 _ s1 s2) = do
  (s1',st') <- buildStore (Map.insert p None st) s1
  (s2',st'') <- buildStore st' s2
  return (Map.insert p (Lift2 s1' s2') st'')
insertSignal st p (SNL3 _ s1 s2 s3) = do
  (s1',st') <- buildStore (Map.insert p None st) s1
  (s2',st'') <- buildStore st' s2
  (s3',st''') <- buildStore st'' s3
  return (Map.insert p (Lift3 s1' s2' s3') st''')
insertSignal st p (SNL4 _ s1 s2 s3 s4) = do
  (s1',st') <- buildStore (Map.insert p None st) s1
  (s2',st'') <- buildStore st' s2
  (s3',st''') <- buildStore st'' s3
  (s4',st'''') <- buildStore st''' s4
  return (Map.insert p (Lift4 s1' s2' s3' s4') st'''')
insertSignal st p (SNL5 _ s1 s2 s3 s4 s5) = do
  (s1',st') <- buildStore (Map.insert p None st) s1
  (s2',st'') <- buildStore st' s2
  (s3',st''') <- buildStore st'' s3
  (s4',st'''') <- buildStore st''' s4
  (s5',st''''') <- buildStore st'''' s5
  return (Map.insert p (Lift5 s1' s2' s3' s4' s5') st''''')

nodeLabel :: (Maybe Id,SignalInfo) -> [Char]
nodeLabel (i,node) = case node of
                       Const           -> "pure"
                       Stateful        -> "stateful"
                       Transfer _      -> "transfer"
                       App _ _         -> "app"
                       Latcher _ _ _   -> "latcher"
                       External        -> "external"
                       Lift1 _         -> "lift1"
                       Lift2 _ _       -> "lift2"
                       Lift3 _ _ _     -> "lift3"
                       Lift4 _ _ _ _   -> "lift4"
                       Lift5 _ _ _ _ _ -> "lift5"
                       None            -> "NONE"
                     ++ (maybe "" show i)

signalToDot :: Signal a -> IO SignalStore
signalToDot sig = do
  (_,st) <- buildStore Map.empty sig
  putStrLn "digraph G {"
  forM_ (Map.assocs st) $ \(ix,n) -> do
    let mkl t = " [label=" ++ t ++ "];"
        rd i = (Just i,st Map.! i)
        erule s l = putStrLn $ "  " ++ nodeLabel (Just ix,n) ++
                               " -> " ++ nodeLabel (rd s) ++ mkl l
    putStrLn $ "  " ++ nodeLabel (Just ix,n) ++ mkl (nodeLabel (Nothing,n))
    case n of
      Transfer s           -> do erule s  ""
      App sf sx            -> do erule sf "f"
                                 erule sx "x"
      Latcher s e ss       -> do erule s  "init"
                                 erule e  "ctl"
                                 erule ss "in"
      Lift1 s1             -> do erule s1 "x1"
      Lift2 s1 s2          -> do erule s1 "x1"
                                 erule s2 "x2"
      Lift3 s1 s2 s3       -> do erule s1 "x1"
                                 erule s2 "x2"
                                 erule s3 "x3"
      Lift4 s1 s2 s3 s4    -> do erule s1 "x1"
                                 erule s2 "x2"
                                 erule s3 "x3"
                                 erule s4 "x4"
      Lift5 s1 s2 s3 s4 s5 -> do erule s1 "x1"
                                 erule s2 "x2"
                                 erule s3 "x3"
                                 erule s4 "x4"
                                 erule s5 "x5"
      _                    -> return ()
  putStrLn "}"
  return st
