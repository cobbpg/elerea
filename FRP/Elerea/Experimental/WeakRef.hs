{-# OPTIONS_GHC -fglasgow-exts #-}

module FRP.Elerea.Experimental.WeakRef
    ( mkWeakRef
    ) where

import GHC.STRef
import GHC.IOBase
import GHC.Prim
import GHC.Weak

-- The abomination below is an IORef-tuned version of mkWeak
mkWeakRef :: IORef t -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakRef (IORef (STRef raw)) v f = IO $ \s ->
  case mkWeak# raw v f s of (# s', w #) -> (# s', Weak w #)
