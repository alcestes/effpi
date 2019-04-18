Verifying Effpi Programs
========================

We now discuss how Effpi implements the verification methodology
discussed in Section 4 of the companion paper.

.. _effpi-verification:

Verification Properties
-----------------------

Effpi provides a compiler plugin to perform static verification of
programs written in the Effpi DSL. The overall approach is described
in the companion paper: see Section 5, paragraph *"Type-level model
checking"*.

In its current incarnation, Effpi supports the properties listed in
Figure 7 of the companion paper, plus one. Methods can be annotated
as:

::

    @effpi.verifier.verify(property = "prop_name(args...)")
    def method(...) = ...

where ``prop_name`` must correspond to one of the currently available
properties:

  * ``no_output_use``: Figure 7(1);
  * ``deadlock_free``: Figure 7(2);
  * ``eventual_output_use``: Figure 7(3);
  * ``forwarding``: Figure 7(4);
  * ``reactive``: Figure 7(5);
  * ``responsive``: Figure 7(6);
  * ``output_eventually_followed``: briefly described in lines 1107-1110.

The arguments to the property specify which
variables/channels/mailboxes should be analysed. Moreover, the base
properties above can be combined into larger properties using
conjunction ``&&``, disjunction ``||``, and negation ``!``.

To see the properties in action, you can refer to the following examples:

  * ``examples/src/main/scala/demo/PingPong.scala``: implements
    Example 2.2 and the types of Example 3.3 of the companion paper,
    plus the verification described in lines 1101-1107;

  * ``examples/src/main/scala/DiningPhilo.scala``: uses
    ``deadlock_free`` in various ways, to show how properties can be
    verified before implementing a behavioural specification, or
    while/after implementing it;

  * ``examples/src/main/scala/demo/MiniAudit.scala``: verifies properties
    on the example in Figure 1 of the companion paper, showing how to
    compose them using ``&&``. Also notice that the implicit mailbox
    is denoted using ``mb_``.

You can also have a look at all the files under
``plugin-benchmarks/src/main/scala/``: they verify a set of rather
large protocols (that are not implemented), using some extra options
to benchmark the verification speed, and save the results.

Compiler Plugin Internals
-------------------------

Here is a brief outline of how the Effpi compiler plugin works, with
pointers to its source code:

The starting point is ``plugin/src/main/scala/Plugin.scala``: it
defines the plugin interface, and looks AST nodes decorated with
verification annotations. When the plugin finds an annotated node, it
invokes its internal method ``check()``, that does the heavy-lifting:

  1. it determines which property is being verified. This causes the
     instantiation of a ``Property`` object (defined in
     ``plugin/src/main/scala/mcrl2/Property.scala``);

       * each available property corresponds to a ``.mcf`` file
         template, available under the directory
         ``plugin/src/main/resources/mcrl2/``. Such templates are
         instantiated depending on how programmers annotate their code:
         you can notice that each file contains placeholders, marked
         with ``$`` symbols;

  2. then, ``check()`` tries to transform the annotated node into a
     behavioural type, very close to Definition 3.1 (3rd row of types)
     in the companion paper;

  3. then, ``check()`` transforms the behavioural type (point 2) into
     a further simplified object, of type ``CCST`` (defined in
     ``plugin/src/main/scala/CCST.scala``): they are similar to terms
     of Robin Milner's CCS (Calculus of Communicating Systems), and
     represent an intermediate step towards...

  4. ...the conversion of the ``CCST`` object into an mCRL2 process
     specification, of type ``Spec`` (defined in
     ``plugin/src/main/scala/mcrl2/Spec.scala``);

  5. the ``Spec`` object is verified against the ``Property`` object
     at point 1, by a ``Verifier`` instance (defined in
     ``plugin/src/main/scala/mcrl2/Verifier.scala``): it handles most
     technicalities (handling temporary directories, invoking mCRL2,
     etc.)

  6. if the ``Verifier`` returns ``true``, the verification was
     successful; otherwise, if it returns ``false``, or one of the
     steps above fails (e.g., because the conversion into behavioural
     types failed), then the plugin prints a warning.

A Note on Type Alias Annotations (or Lack Thereof)
--------------------------------------------------

The submitted version of the companion paper, on lines 1077-1078,
mentions the possibility of annotating type aliases. The use case is
described on lines 1094--1097:

    "Checking type aliases like ``T2``, instead, allows to compose the
    types/protocols of multiple services, and **verify their
    interactions, even without their implementation**" *(emphasis added)*

However, after we submitted our paper, we realised that the same use
case is *already covered by method annotations* (i.e., case ``T1`` in
the companion paper), that are much easier to use! In fact, to verify
behavioural types without actually implementing them, it is sufficient
to just type-annotate a method that is left unimplemented, i.e.,
defined as ``???`` in Dotty.

The approach is illustrated in the dining philosophers example: see
the file ``examples/src/main/scala/DiningPhilo.scala``, and notice
that we verify the protocols both *before* implementing them (by
annotating stub methods defined as ``???``), and when actually
implementing them. Similarly, the files under
``plugin-benchmarks/src/main/scala/`` verify various rather large
protocols (without implementing them), for benchmarking purposes.

Consequently, we decided to remove the support for type alias
annotations. We plan to update lines 1077--1097 in the final version
of the companion paper, to explain the alternative approach outlined
above, and match the current status of the artifact.
