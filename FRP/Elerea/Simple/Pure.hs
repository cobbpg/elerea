{-|

This module contains the reference implementation for the pure subset
of the simple variant of Elerea.  I/O embedding is substituted by
conversion from and to lists.

-}

module FRP.Elerea.Simple.Pure
    (
    -- * The signal abstraction
      Signal
    , SignalGen
    -- * Inputs and outputs
    , fromList
    , toList
    , start
    -- * Basic building blocks
    , delay
    , snapshot
    , generator
    , memo
    , until
    -- * Derived combinators
    , stateful
    , transfer
    , transfer2
    , transfer3
    , transfer4
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

type Signal a = Int -> a

type SignalGen a = Int -> a

fromList :: [a] -> Signal a
fromList = (!!)

toList :: Signal a -> [a]
toList s = map s [0..]

start :: SignalGen (Signal a) -> [a]
start g = toList (g 0)

delay :: a -> Signal a -> SignalGen (Signal a)
delay x0 s t_start t_sample
    | t_start < t_sample  = s (t_sample-1)
    | t_start == t_sample = x0
    | otherwise           = error "This signal doesn't exist yet."

generator :: Signal (SignalGen a) -> SignalGen (Signal a)
generator s _t_start t_sample = s t_sample t_sample

snapshot :: Signal a -> SignalGen a
snapshot = id

memo :: Signal a -> SignalGen (Signal a)
memo = const

stateful :: a -> (a -> a) -> SignalGen (Signal a)
stateful x0 f = mfix $ \sig -> delay x0 (f <$> sig)

transfer :: a -> (t -> a -> a) -> Signal t -> SignalGen (Signal a)
transfer x0 f s = mfix $ \sig -> do
    sig' <- delay x0 sig
    memo (liftA2 f s sig')

transfer2 :: a -> (t1 -> t2 -> a -> a) -> Signal t1 -> Signal t2 -> SignalGen (Signal a)
transfer2 x0 f s1 s2 = mfix $ \sig -> do
    sig' <- delay x0 sig
    memo (liftA3 f s1 s2 sig')

transfer3 :: a -> (t1 -> t2 -> t3 -> a -> a) -> Signal t1 -> Signal t2 -> Signal t3 -> SignalGen (Signal a)
transfer3 x0 f s1 s2 s3 = mfix $ \sig -> do
    sig' <- delay x0 sig
    memo (liftM4 f s1 s2 s3 sig')

transfer4 :: a -> (t1 -> t2 -> t3 -> t4 -> a -> a) -> Signal t1 -> Signal t2 -> Signal t3 -> Signal t4 -> SignalGen (Signal a)
transfer4 x0 f s1 s2 s3 s4 = mfix $ \sig -> do
    sig' <- delay x0 sig
    memo (liftM5 f s1 s2 s3 s4 sig')
