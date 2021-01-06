%%% 'antonius' chess engine
%%% Copyright (C) 2013 Rabbe Fogelholm
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.




-define(HRL_ANTONIUS, true).

-define(ENGINE_NAME, "Antonius").
-define(ENGINE_VERSION_MAJOR, 1).
-define(ENGINE_VERSION_MINOR, 2).

-define(MAX_THREADS_DEFAULT, 4).
-define(RECURSION_DEPTH_DEFAULT, 3).


-define(SMALLINT_MIN_VALUE, -100000000).
-define(SMALLINT_MAX_VALUE, 100000000).


-define(CASTLING_BONUS, 300).

-define(OUT, user).

-define(CLI_PROMPT, "> ").

-define(CLI_TOP, 1).
-define(CLI_BOTTOM, 4).
-define(CLI_INDENT, 5).
-define(CLI_RIGHT_COL, 42).
-define(CLI_RIGHT_FLOOR1, 8).

-define (DISPLAY_LIMIT, 7).

-type sid() :: tuple()|xboard|cli|test.
-define(IS_XBOARD(SID), (SID =:= xboard)).

-type smallint() :: ?SMALLINT_MIN_VALUE..?SMALLINT_MAX_VALUE.
-type xstring()  :: string() | null.


% TODO, understand if 'user' is appropriate. Related to eunit tests?
-type o_device() :: file:io_device() | user.

-type i_device() :: file:io_device().
