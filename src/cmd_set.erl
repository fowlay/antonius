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
%% Description: TODO: Add description to cmd_set
-module(cmd_set).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([setdepth/1]).

-export([setthreads/1]).

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
	  "setdepth", 
	  fun ?MODULE:setdepth/1,
	  1,
	  0,				  
	  "set recursion depth",
	  [
	   "Synopsis:",
	   "",
	   "    setdepth DEPTH",
	   "",
	   "The given DEPTH will be used as the recursion depth by",
	   "commands that do not specify the depth explicitly."
	  ]),
	
	cmd_dict:mergeCommand(
	  "setthreads", 
	  fun ?MODULE:setthreads/1,
	  1,
	  0,
	  "set number of execution threads",
	  [
	   "Synopsis:",
	   "",
	   "    setthreads THREADS",
	   "",
	   "Execute on the given number of threads."
	  ]).

%%
%% Local Functions
%%







%% @doc Sets the recursion depth.
%%
%% TODO, sanity checking?

-spec setdepth([string()]) -> #cmdresult{}.

setdepth([Depth]) ->
	param_parameter:setRecursionDepth(list_to_integer(Depth)),
    #cmdresult{}.


%% @doc Sets the number of threads.
%%
%% TODO, sanity checking?

setthreads([Threads]) ->
    param_parameter:setNumberOfThreads(list_to_integer(Threads)),
	#cmdresult{}.