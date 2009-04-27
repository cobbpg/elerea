{-|

Elerea (Eventless Reactivity) is a simplistic FRP implementation that
parts with the concept of events, and uses a continuous latching
construct instead. The user sees the functionality through an
applicative interface, which is used to build up a network of
interconnected mutable references. The network is executed
iteratively, where each superstep consists of two phases:
sampling-aging and finalisation.  As an example, the following code is
a possible way to define an approximation of our beloved trig
functions:

@
 sine = integral 0 cosine
 cosine = integral 1 (-sine)
@

Note that @integral@ is not a primitive, it can be defined by the user
as a transfer function. A possible implementation that can be used on
any 'Fractional' signal looks like this:

@
 integral x0 s = transfer x0 (\\dt x x0 -> x0+x*realToFrac dt) s
@

Head to "FRP.Elerea.Internal" for the implementation details.  To get
a general idea how to use the library, check out the sources in the
@elerea-examples@ package.

-}

module FRP.Elerea (
  Time, DTime,
  Sink,
  Signal,
  superstep, keepAlive,
  stateful, transfer, latcher, external,
  delay, edge,
  (==@), (/=@), (<@), (<=@), (>=@), (>@),
  (&&@), (||@)
) where

import Control.Applicative
import FRP.Elerea.Internal

infix  4 ==@, /=@, <@, <=@, >=@, >@
infixr 3 &&@
infixr 2 ||@

{-| The `delay` transfer function emits the value of a signal from the
previous superstep, starting with the filler value given in the first
argument. -}

delay :: a -> Signal a -> Signal a
delay v0 s = snd <$> transfer (v0,v0) (\_ v' (v,_) -> (v',v)) s

{-| The `edge` transfer function takes a bool signal and emits another
bool signal that turns true only at the moment when there is a rising
edge on the input. -}

edge :: Signal Bool -> Signal Bool
edge b = (not <$> delay True b) &&@ b

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
