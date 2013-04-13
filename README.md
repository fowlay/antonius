Antonius
================================

This directory contains the "antonius" chess engine. It is named
after the knight, of questionable chess-playing strength, in an
Ingmar Bergman movie.


Requirements
------------

* A Gnu/Linux environment.

* An installation of Erlang (http://erlang.org). The installed
executables (erl, erlc, etc) must be in the path.

* A C compiler, such as gcc.

* Optionally: The "xboard" GUI.

* Optionally: Eclipse with ErlIDE for browsing/editing the source.


Playing a console game
----------------------

Type

    make game-cli

and follow instructions on the screen.


Playing an xboard game
----------------------

It is assumed that you have installed 'xboard'
(http://www.gnu.org/software/xboard/). Type

    make game-xboard

A chessboard window should appear, and a console window where some
debug output is shown. Play by drag-and-drop.


Running the tests
-----------------

The provided test suites are expected to pass. Some of the tests
check various aspects of correctness and should always succeed.
Other tests depend on the evaluation function and may have to be
adjusted if the evaluation function is changed.

To run the tests, type

    make test-quick
    make test-long
    
Allow several minutes for the tests to complete on a dual-core or
better host.


Running the dialyzer
--------------------

Type

    make dialyze

Allow a couple of minutes for the var/dialyze_plt file to be built
when this command is run for the first time. A "passed successfully"
message should result.


Using Eclipse
-------------

Two Eclipse projects are provided: 'antonius' (the Erlang source)
and code_nif (functions coded in C).

For the 'antonius' project Eclipse with the Erlide plugin from
http://erlide.org is needed. For the 'core_nif' project Eclipse
with the C/C++ development tools is needed. An easy way set up both
development environments is to download Eclipse CDT from
http://eclipse.org and then add the Erlide plugin.

The 'core_nif' project refers to Erlang header files for C. To adapt
the .cproject file to the current Erlang environment, type

    make eclipse-setup


Feedback
--------

Comments, ideas and improvements are welcome. Contact the author at

<pre>
                     se
                   .
           bahnhof
         @
   rabbe
</pre>
