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
%% Description: TODO: Add description to cmd_suggest
-module(cmd_suggest).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([init/0]).
-export([suggest/1]).

%%
%% Include files
%%

-include("cmdresult.hrl").

%%
%% API Functions
%%

init() ->
	cmd_dict:mergeCommand("suggest", fun ?MODULE:suggest/1, 0, 1,
"suggest a move for white", 
				[
				"Synopsis:",
				"",
				"    suggest [DEPTH]",
				"",
				"Suggest some moves. The recursion depth may",
				"be specified (it defaults to "++"*** FIXME ***"++"). A suggested",
				"move may be chosen with a subsequent 'do' command."
				]).


%% @doc Suggest a move (for white to make).

-spec suggest([sid()]) -> #cmdresult{}.

suggest([Sid]) ->
	suggest([Sid, integer_to_list(param_parameter:getRecursionDepth(Sid))]);

suggest([Sid, X]) ->
	RecursionDepth = list_to_integer(X),
	CurrentNode = core_gamestate:getCurrentNode(Sid),
	
	Key = core_node:key(CurrentNode),
	ABResult = core_gamestate:getAbResult(Sid, Key, RecursionDepth),
	
	if
		ABResult =/= null ->
			Text = core_abresult:bestMoves(Sid, ABResult, CurrentNode),
			#cmdresult{text=Text};
		true ->
			NewABResult = core_node:alphaBetaRoot(Sid, CurrentNode, RecursionDepth),
			core_gamestate:putAbResult(Sid, Key, RecursionDepth, NewABResult),
			Text = core_abresult:bestMoves(Sid, NewABResult, CurrentNode),
			#cmdresult{text=Text}
	end.

%%
%% Local Functions
%%
