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
%% Created: Aug 22, 2012
%% Description: TODO: Add description to xboardcommand_sd
-module(xbi_sd).

%%
%% Include files
%%

-include("antonius.hrl").

%%
%% Exported Functions
%%
-export([execute/1]).

%%
%% API Functions
%%

-spec execute([sid()|string()]) -> ok.

execute([Sid, DepthArg]) ->
	xbi_controller:log("execute: sd ~s", [DepthArg]),
	Depth = list_to_integer(DepthArg),
	xbi_controller:log("setting recursion depth: ~p", [Depth]),
	param_parameter:setRecursionDepth(Sid, Depth),
	ok.
	


%%
%% Local Functions
%%

