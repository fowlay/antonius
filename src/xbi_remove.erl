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
%% Created: Aug 27, 2012
%% Description: TODO: Add description to xboardcommand_remove
-module(xbi_remove).

%%
%% Include files
%%

-include("node.hrl").

%%
%% Exported Functions
%%

-export([execute/1]).

%%
%% API Functions
%%

-spec execute([string()]) -> ok.

execute([]) ->
	xbi_controller:log("execute: remove", []),
	
	Function = cmd_dict:getFunction("undo", []),

	apply(Function, [[]]),
	apply(Function, [[]]),
	
	#node{blackAdvantage=BA} = core_gamestate:getCurrentNode(),
	xbi_controller:log("black advantage: ~w", [BA]),
	
	ok.

%%
%% Local Functions
%%

