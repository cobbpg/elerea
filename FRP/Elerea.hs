{-|

Elerea (Eventless reactivity) is a tiny discrete time FRP
implementation without the notion of event-based switching and
sampling, with first-class signals (time-varying values).  Reactivity
is provided through various higher-order constructs that also allow
the user to work with arbitrary time-varying structures containing
live signals.  Signals have precise and simple denotational semantics.

Stateful signals can be safely generated at any time through a monadic
interface, while stateless combinators can be used in a purely
applicative style.  Elerea signals can be defined recursively, and
external input is trivial to attach.  The library comes in two major
variants, one of which you need to import:

* "FRP.Elerea.Simple": signals are plain discrete streams isomorphic
to functions over natural numbers;

* "FRP.Elerea.Param": adds a globally accessible input signal for
convenience;

Elerea is a minimal library that defines only some basic primitives,
and you are advised to install @elerea-examples@ as well to get an
idea how to build non-trivial systems with it.  The examples are
separated in order to minimise the dependencies of the core library.
The @dow@ package contains a full game built on top of the simple
variant.

The basic idea of the implementation is described in the WFLP 2010
paper /Efficient and Compositional Higher-Order Streams/
(<http://sgate.emt.bme.hu/documents/patai/publications/PataiWFLP2010.pdf>).

In short, the basic idea is to define completely dynamic data-flow
networks through a pure combinator-style monadic interface.  The
network can be turned into an I/O action that samples it sequentially
by the @start@ function.  Under the hood, the network is represented
by mutable variables whose interconnections are encapsulated in
closures, and consistency is ensured by a two-phase update process
(essentially double buffering).  The library keeps track of the
variables through weak pointers, so all of the live variables can be
updated (this is necessary to ensure referential transparency), and
unused ones can be garbage collected.

-}

module FRP.Elerea where

