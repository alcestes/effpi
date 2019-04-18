Software Requirements & Setup, and Browsing the Source Code
===========================================================

For the software requirements and setup, please refer to ``README.txt``
in the PLDI'19 artifact submission.

To browse the Effpi source code, you can invoke ``sbt launchIDE`` to
execute Visual Studio Code (that is already installed in the artifact
virtual machine).

Issues with Visual Studio Code
------------------------------

Dealing with Plugin Benchmarks
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We recommend to run ``launchIDE`` only *after* running the
benchmarks described in ``README.txt``; otherwise, ``sbt`` will try to
perform the benchmarks anyway! To avoid this situation, you can edit
``build.sbt``, and change the way ``pluginOpts`` is invoked in the
sub-project ``pluginBenchmarks``; for details, see the comments in the
definition of ``pluginOpts``.

Lack of On-The-Fly Typing, Jump-To-Definition, etc.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Unfortunately, the Dotty plugin for Visual Studio Code may stop working while
browsing the ``examples/`` directory, due to
`this open issue <https://github.com/lampepfl/dotty/issues/5579>`_.

If it happens, you can try the following (also in combination):

a. exit from VS Code, try ``sbt launchIDE`` again, but browse the examples
   by going through the ``demo/`` directory (that is just a symbolic link); or

b. exit from VS Code, launch ``sbt``, then:

     1. execute ``launchIDE`` from its shell, and use Visual Studio Code to
        browse Effpi's source code, and

     2. to see the full compilation logs and warnings, invoke by hand
        ``examples/compile`` from the ``sbt`` shell, every time you change
        something in the source code.
