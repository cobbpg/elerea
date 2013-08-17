# Introduction

Elerea (Eventless reactivity) is a tiny discrete time functional
reactive programming (FRP) system without the notion of event-based
switching and sampling, but with first-class signals (time-varying
values).  Reactivity is provided through various higher-order
constructs that also allow the user to work with arbitrary
time-varying structures containing live signals.  Signals have precise
and simple denotational semantics.

Stateful signals can be safely generated at any time through a monadic
interface, while stateless combinators can be used in a purely
applicative style.  Elerea signals can be defined recursively, and
external input is trivial to attach.  The library comes in three major
variants:

* **Simple**: signals are plain discrete streams isomorphic to
    functions over natural numbers;

* **Param**: adds a globally accessible input signal for convenience;

* **Clocked**: adds the ability to freeze whole subnetworks at will.

This is a minimal library that defines only some basic primitives, and
you are advised to install
[elerea-examples](https://github.com/cobbpg/elerea-examples) as well
to get an idea how to build non-trivial systems with it. The examples
are separated in order to minimise the dependencies of the core
library. The dow package contains a full game built on top of the
simple variant.

The basic idea of the implementation is described in the WFLP 2010
paper [Efficient and Compositional Higher-Order
Streams](http://sgate.emt.bme.hu/documents/patai/publications/PataiWFLP2010.pdf).

# Related projects

* [Euphoria](https://github.com/tsurucapital/euphoria): a complex
  reactive framework built around Elerea.

* [Helm](https://github.com/z0w0/helm): a game engine that uses Elerea
  for describing interactions and behaviours.

* [LambdaCube 3D](https://github.com/csabahruska/lc-dsl): a purely
  functional GPU pipeline description language, whose showcases mostly
  rely on Elerea.
