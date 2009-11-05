{-# LANGUAGE UnboxedTuples, MagicHash #-}

module FRP.Elerea.Experimental.WeakRef
    ( mkWeakRef
    ) where

import GHC.STRef
import GHC.IOBase
import GHC.Prim
import GHC.Weak

{-| A variation of 'mkWeak' that uses an IORef as a key.  As opposed
to 'mkWeak', it works even with optimisations on, but it crashes in
ghci. -}

mkWeakRef :: IORef t -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakRef (IORef (STRef raw)) v f = IO $ \s ->
  case mkWeak# raw v f s of (# s', w #) -> (# s', Weak w #)
