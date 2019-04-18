How to Develop Strongly-Typed Concurrent Programs Using Effpi
=============================================================

Effpi's DSL is still in flux, and its documentation is not complete.
Here we provide a general outline (with references to the Effpi source
code), and pointers to various examples (with references to the
companion paper, when applicable).

Structure of Effpi's DSL, and Source Code References
----------------------------------------------------

Effpi's DSL provides:

  1. Dotty types to describe the intended behaviour of message-passing
     programs. Such Dotty types are based on the 2nd and 3rd row of
     types in Definition 3.1 in the companion paper --- plus various
     experimental extensions, not (yet) covered by our theory. See
     type definitions in the following files (from the root of Effpi's
     source code):

       * ``src/main/scala/ProcesDSL.scala``
       * ``src/main/scala/ActorDSL.scala`` (note that most types
         defined in this file are just sintactic sugar on top of those
         in ``ProcessDSL.scala``, as discussed in Section 5 of the
         companion paper.

  2. Dotty methods to create DSL terms, having the types in point 1
     above. Such methods implement the communication primitives
     described in the last line of Figure 2 of the companion paper ---
     plus various experimental extensions, not (yet) covered by our
     theory. See method definitions in:

       * ``src/main/scala/ProcesDSL.scala``
       * ``src/main/scala/ActorDSL.scala`` (note that most methods
         defined in this file are just sintactic sugar on top of those
         in ``ProcessDSL.scala``, as discussed in Section 5 of the
         companion paper.

.. _effpi-usage:

Simple Usage Examples
---------------------

The ``examples`` directory contains various examples using Effpi. The source
code also contains instructions on how to run each example.

**NOTE:** here we only discuss Effpi's DSL, which ensures that a
program implements a desired protocol specification, written as a
type. You will notice that most of the following examples also have
*verification annotations*, to statically ensure that a protocol (and
its implementation) have certain runtime properties. The annotations
are discussed in :ref:`effpi-verification`.

  * ``examples/src/main/scala/demo/PingPong.scala``: implements
    Example 2.2 and the types of Example 3.3 of the companion paper.

  * ``examples/src/main/scala/DiningPhilo.scala``: the Dining
    Philosophers problem, with various implementations (follow the
    source code comments to experiment with it).

  * ``examples/src/main/scala/demo/MiniAudit.scala``: the example in
    Figure 1 of the companion paper (a boiled-down version of the next
    example). It allows to repeat the experiment described in lines 111-114,
    obtaining compilation errors when the code does not match the intended
    behavioural type.

  * ``examples/src/main/scala/FilterServer.scala``: implementation of
    Example 3.4 in the companion paper: a filtering server receives typed
    custom code from a client, and executes it. 

  * ``examples/src/main/scala/audit/Audit.scala``: the "full" audit example,
    implementing a `use case for Akka Typed Session <https://github.com/rkuhn/akka-typed-session/blob/master/src/test/scala/com/rolandkuhn/akka_typed_session/auditdemo/ProcessBased.scala>`_
    (mentioned on lines 37-38 of the companion paper). We used this example
    to design the Effpi API, and it uses some features and types that are not
    (yet) covered by our theory (e.g., the "ask pattern"). It also produces
    some compilation warnings about non-exhaustive pattern matching: we left
    them on purpose, as they highlight a difference wrt. the Akka Typed Session
    implementation, where the Scala compiler is unable to produce such warnings.
