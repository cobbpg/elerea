{-|

Elerea (Eventless Reactivity) is a simplistic FRP implementation that
parts with the concept of events, and introduces various constructs
that can be used to define completely dynamic higher-order dataflow
networks.  The user sees the functionality through a hybrid
monadic-applicative interface, where stateful signals can only be
created through a specialised monad, while most combinators are purely
applicative.  The combinators build up a network of interconnected
mutable references in the background.  The network is executed
iteratively, where each superstep consists of three phases: sampling,
aging, and finalisation.  As an example, the following code is a
possible way to define an approximation of our beloved trig functions:

@
 (sine,cosine) <- mdo
   s <- integral 0 c
   c <- integral 1 (-s)
   return (s,c)
@

Note that @integral@ is not a primitive, it can be defined by the user
as a transfer function.  A possible implementation that can be used on
any 'Fractional' signal looks like this:

@
 integral x0 s = transfer x0 (\\dt x x0 -> x0+x*realToFrac dt) s
@

Head to "FRP.Elerea.Internal" for the implementation details.  To get
a general idea how to use the library, check out the sources in the
@elerea-examples@ package.

The "FRP.Elerea.Experimental" branch provides a similar interface with
a rather different underlying structure, which is likely to be more
efficient.

-}

module FRP.Elerea
    ( DTime, Sink, Signal, SignalMonad
    , createSignal, superstep
    , external
    , stateful, transfer, delay
    , sampler, generator
    , storeJust, toMaybe
    , edge
    , keepAlive, (.@.)
    , (==@), (/=@), (<@), (<=@), (>=@), (>@)
    , (&&@), (||@)
    , signalDebug
) where

import Control.Applicative
import FRP.Elerea.Internal

infix  4 ==@, /=@, <@, <=@, >=@, >@
infixr 3 &&@
infixr 2 ||@

{-| A short alternative name for 'keepAlive'. -}

(.@.) :: Signal a -> Signal t -> Signal a
(.@.) = keepAlive

{-| The `edge` transfer function takes a bool signal and emits another
bool signal that turns true only at the moment when there is a rising
edge on the input. -}

edge :: Signal Bool -> SignalMonad (Signal Bool)
edge b = delay True b >>= \db -> return $ (not <$> db) &&@ b

{-| The `storeJust` transfer function behaves as a latch on a 'Maybe'
input: it keeps its state when the input is 'Nothing', and replaces it
with the input otherwise. -}

storeJust :: a                      -- ^ Initial output
          -> Signal (Maybe a)       -- ^ Maybe signal to latch on
          -> SignalMonad (Signal a)
storeJust x0 s = transfer x0 store s
    where store _ Nothing  x = x
          store _ (Just x) _ = x

{-| Point-wise equality of two signals. -}

(==@) :: Eq a => Signal a -> Signal a -> Signal Bool
(==@) = liftA2 (==)

{-| Point-wise inequality of two signals. -}

(/=@) :: Eq a => Signal a -> Signal a -> Signal Bool
(/=@) = liftA2 (/=)

{-| Point-wise comparison of two signals. -}

(<@) :: Ord a => Signal a -> Signal a -> Signal Bool
(<@) = liftA2 (<)

{-| Point-wise comparison of two signals. -}

(<=@) :: Ord a => Signal a -> Signal a -> Signal Bool
(<=@) = liftA2 (<=)

{-| Point-wise comparison of two signals. -}

(>=@) :: Ord a => Signal a -> Signal a -> Signal Bool
(>=@) = liftA2 (>=)

{-| Point-wise comparison of two signals. -}

(>@) :: Ord a => Signal a -> Signal a -> Signal Bool
(>@) = liftA2 (>)

{-| Point-wise OR of two boolean signals. -}

(||@) :: Signal Bool -> Signal Bool -> Signal Bool
(||@) = liftA2 (||)

{-| Point-wise AND of two boolean signals. -}

(&&@) :: Signal Bool -> Signal Bool -> Signal Bool
(&&@) = liftA2 (&&)
