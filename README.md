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

Eclipse with Erlide plugin from erlide.org can be used to browse
and modify the source. The include files .project and
.settings/org.erlide.core.prefs define a project that can be
imported into Eclipse.


Feedback
--------

Comments, ideas and improvements are welcome. Contact the author at

                     se
                   .
           bahnhof
         @
   rabbe
