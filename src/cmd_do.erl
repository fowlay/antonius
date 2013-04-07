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
%% Description: TODO: Add description to cmd_do
-module(cmd_do).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([do/1]).

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

%%
%% API Functions
%%

init() ->
	cmd_dict:mergeCommand("do", fun ?MODULE:do/1, 0, 2, "make suggested move",
			[
				"Synopsis:",
				"",
				"    do [index] [depth]",
				"",
				"The index selects a move from the listed suggestions. If not",
				"specified the best move is selected.",
				"",
				"If several suggestions at different depths have been made the",
				"desired depth may be indicated. If not specified the deepest",
				"suggestion will be used."
			]).
%%
%% Local Functions
%%


%% @doc TODO

-spec do([string()]) -> #cmdresult{}.

do([]) ->
	do(["1"]);
	
do([X]) ->
	do([X, "-1"]);
		 
do([X, Y]) ->
	Index = list_to_integer(X),
	Depth = list_to_integer(Y),
	
	#node{toMove=ToMove}=CurrentNode = core_gamestate:getCurrentNode(),
	
	AbResult =
		if
			Depth =:= -1 ->
				core_gamestate:getAbResult(core_node:key(CurrentNode));
			true ->
				core_gamestate:getAbResult(core_node:key(CurrentNode), Depth)
		end,
	
	if
		AbResult =:= null, Depth =/= -1 ->
			core_util:userException("no suggestion available at depth: ~w", [Depth]);
		AbResult =:= null ->
			core_util:userException("no suggestion available");
		true ->
			AbNode = core_abresult:selectNode(AbResult, Index, ToMove),
			if
				AbNode =:= null ->
					core_util:userException("no such move");
				true ->
					SuggestedArgs = core_move:describeBrief(core_move:create(CurrentNode, AbNode)),
					cmd_move:move(SuggestedArgs)
			end
	end.