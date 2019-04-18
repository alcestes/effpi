Effpi: Verifying Message-Passing Programs with Dependent Behavioural Types
==========================================================================

Effpi is an internal domain-specific language for Dotty (a.k.a. the
future Scala 3), that allows to define strongly-typed message-passing
programs, and perform type-level verification via model-checking: this
allows to statically ensure that an Effpi-based program is, for
instance, deadlock-free, or responsive to its inputs.

Effpi has two purposes:

  1. provide a playground to explore how Dotty types can be used to
     describe protocol specifications, enforce them on programs, and
     verify them. As part of this purpose, Effpi provides an
     implementation of the theory described in the companion paper:
     *"Verifying message-passing programs with dependent behavioural
     types"* (conditionally accepted at PLDI'19);

  2. evaluate how to efficiently run the strongly-typed concurrent
     programs resulting from point 1 above, with various run-time
     systems.

.. toctree::
   :maxdepth: 2

   _source/toolkit_setup
   _source/toolkit_usage
   _source/verification
   _source/runtime

.. Indices and tables
.. ==================

.. * :ref:`genindex`
.. * :ref:`modindex`
.. * :ref:`search`
.. * :ref:`runtime`
