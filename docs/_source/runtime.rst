..  runtime stuff

Effpi's Runtime System
======================

Effpi provides various runtime systems to run its programs. For simplicity,
some examples in :ref:`effpi-usage` use a naive runtime that creates one
JVM thread per concurrent process/actor. However, this does not scale, and
only works with a limited number of concurrent processes.

For this reason, Effpi also provides two optimised runtime systems, that
manage a limited number of JVM threads on which processes can be scheduled for
execution; the runtime system decides when to schedule a process, and how to
(cooperatively) context switch. This is mentioned in Section 5 of the companion
paper.

The optimised runtime system has two main components:

* The *Process System:* it maintains the whole scheduling and execution state. It
  stores two
  queues in memory, one specific to deal with processes of input type (the only
  type that is blocking) and one for all other types. The code for the process
  system can be found at ``src/main/scala/ProcessSystem.scala``. There are two
  concrete process system implementations to choose from:

    * ``ProcessSystemRunnerImproved``: this is considered the default runtime
      and it is referred to as *Effpi default* in Figure 8 of the companion
      paper;
    * ``ProcessSystemStateMachineMultiStep``: this is an alternative
      implementation, inspired by Akka Typed's runtime, that uses finite
      state machines to keep track of input channels state, which helps minimise
      over-scheduling of input process. It is referred to in the paper as *Effpi
      with channel FSM*.

* The *Executors*: these are agents that consume the queues of the process system.
  They dequeue a process, execute it for a while, and finally they arbitrate
  when to perform context switching by putting the process back onto the queue,
  and dequeueing another one. There are two types of executors:

    * ``src/main/scala/InputExecutor.scala``: only handles processes waiting
      for input;
    * ``src/main/scala/Executor.scala``: handles all processes, possibly
      delegating them to ``InputExecutor`` when they wait for input.
