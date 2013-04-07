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
%% Created: Nov 11, 2012
%% Description: TODO: Add description to cmd_history
-module(cmd_history).

%%
%% Include files
%%

-include("cmdresult.hrl").
-include("statenode.hrl").

%%
%% Exported Functions
%%
-export([init/0]).
-export([history/1]).

%%
%% API Functions
%%

init() ->
	cmd_dict:mergeCommand(
	  "history", 
	  fun ?MODULE:history/1, 
	  0, 
	  0,
	  "list all moves up to current position",
	  [
				
	   "List the moves leading to the current",
	   "position."
	  ]).


%% @doc TODO, should do the text processing in this module.

history([]) ->
	StateNodes = core_gamestate:getHistory(),
	Lines = getHistoryHelper(StateNodes, 0, []),
	#cmdresult{text=Lines}.

%%
%% Local Functions
%%

-spec getHistoryHelper([#statenode{}], smallint(), [string()]) -> [string()].

getHistoryHelper([], _, R) ->
	lists:reverse(R);

getHistoryHelper([_], _, R) ->
	lists:reverse(R);

getHistoryHelper([#statenode{node=#node{toMove=ToMove}=P}, #statenode{node=C}=SNC|Tail], K, R) ->
	NewK = if ToMove =:= white -> K+1; true -> K end,
	B = io_lib:format("~w [~s] move", [NewK, core_colour:shortName(ToMove)]),
	Move = core_move:create(P, C),
	SS = core_move:describeBrief(Move),
	Entry = appendStrings(B, SS),
	getHistoryHelper([SNC|Tail], NewK, [Entry|R]).


-spec appendStrings(string(), [string()]) -> string().

appendStrings(U, []) ->
	U;

appendStrings(U, [X|Z]) ->
	W = U++" "++X,
	appendStrings(W, Z).
