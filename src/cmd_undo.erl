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
%% Description: TODO: Add description to cmd_undo
-module(cmd_undo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([undo/1]).

%%
%% Include files
%%

-include("cmdresult.hrl").

%%
%% API Functions
%%


init() ->
	cmd_dict:mergeCommand("undo", fun ?MODULE:undo/1, 0, 0,
						 
						 		"undo the latest move", 
				[
				"Undo the latest move."
				]).


%% @doc Undo the latest half-move.

-spec undo([sid()]) -> #cmdresult{}.

undo([Sid]) ->
	core_gamestate:removeCurrentNode(Sid),
	#cmdresult{gsMoveRetracted=true}.

%%
%% Local Functions
%%
