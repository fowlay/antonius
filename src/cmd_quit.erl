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

%% Author: erarafo
%% Created: Nov 10, 2012
%% Description: TODO: Add description to cmd_quit
-module(cmd_quit).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).
-export([quit/1]).

%%
%% Include files
%%
-include("antonius.hrl").

-include("piecetype.hrl").
-include("board.hrl").
-include("node.hrl").
-include("abresult.hrl").
-include("game.hrl").
-include("gameState.hrl").

-include("cmdresult.hrl").
-include("exception.hrl").

%%
%% API Functions
%%

init() ->
	cmd_dict:mergeCommand(
	  "quit",
	  fun ?MODULE:quit/1,
	  0,
	  0,
	  "quit this program",
	  ["Quits this program."
	  ]).


%%
%% Local Functions
%%





%% @doc Terminate the ongoing game.

-spec quit([string()]) -> #cmdresult{}. 

quit(_Args) ->
	%say("welcome back~n", []),
	#cmdresult{proceed=false}.