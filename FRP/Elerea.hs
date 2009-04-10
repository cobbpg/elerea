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

Head to "FRP.Elerea.Internal" for the implementation details.

-}

module FRP.Elerea (
  Time, DTime,
  Sink,
  Signal,
  superstep,
  time,
  stateless,
  stateful,
  transfer,
  latcher,
  external
) where

import FRP.Elerea.Internal