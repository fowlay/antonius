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
%% Description: TODO: Add description to cmd_setup
-module(cmd_setup).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([setupempty/1]).

-export([setupHelper/1]).

-export([setup/1]).

-export([resetParameters/0]).

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
	  "setup",	  
	  fun ?MODULE:setup/1, 
	  0, 
	  1, 
	  "set up pieces for a new game",
	  ["Sets up the pieces for a new game. If the optional",
	   "argument r is given then parameters are reset to",
	   "default values, as follows:",
	   "",
	   "  search depth:        " ++integer_to_list(?RECURSION_DEPTH_DEFAULT),
	   "  number of threads:   "++integer_to_list(?MAX_THREADS_DEFAULT)
	  ]),
	cmd_dict:mergeCommand(
	  "empty",
	  fun ?MODULE:setupempty/1,
	  0,
	  0,
	  "set up an empty board",
	  ["This command sets up an empty board. Pieces may then be",
	   "placed with the 'put' command."
	  ]).
	  



%%
%% Local Functions
%%





%% @doc For tests, set up an empty game.

-spec setupempty([string()]) -> #cmdresult{}.

setupempty(_Args) ->
	core_gamestate:clear(),
	resetParameters(),
	Node = core_node:create(
			      "",
			      "",
			      white),
	core_gamestate:addNode(Node),
	#cmdresult{gsMoveMade=true}.



-spec setupHelper(boolean()) -> #cmdresult{}.

setupHelper(Reset) ->
	core_gamestate:clear(),
	
	if
		Reset ->
			resetParameters();
		true ->
			ok
	end,

	Node = core_node:create(
			      "*Ra1 Nb1 Bc1 Qd1 *Ke1 Bf1 Ng1 *Rh1 a2 b2 c2 d2 e2 f2 g2 h2",
			      "*Ra8 Nb8 Bc8 Qd8 *Ke8 Bf8 Ng8 *Rh8 a7 b7 c7 d7 e7 f7 g7 h7",
			      white),
	core_gamestate:addNode(Node),
	core_gamestate:setCurrentState(open),
	#cmdresult{}.

% getSuggestion() ->
	% state:get(suggestion).


%% @doc Set up pieces for a new game. The return type
%% is the one deduced by dialyzer.
%%
%% TODO: Do we really need to handle UC?

-spec setup([string()]) -> #cmdresult{}.

setup(["r"]) ->
	setupHelper(true);
	
setup(["R"]) ->
		setupHelper(true);

setup([]) ->
		setupHelper(false);
				 
setup([Other]) ->
	core_util:userException("illegal option: "++Other).





-spec resetParameters() -> ok.

resetParameters() ->
	param_parameter:setRecursionDepth(?RECURSION_DEPTH_DEFAULT),
	param_parameter:setNumberOfThreads(?MAX_THREADS_DEFAULT).