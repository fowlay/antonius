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
%% Description: TODO: Add description to xboardcommand_protover
-module(xbi_protover).

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

-spec execute([string()]) -> ok.

execute([ProtocolVersion]) ->
	xbi_controller:log("execute: protover ~s", [ProtocolVersion]),
	
	% TODO: Verify that version 2 is offered
	
	xbi_controller:stdout("feature myname=~s~s ~w.~w~s",
					      [[34], ?ENGINE_NAME, ?ENGINE_VERSION_MAJOR, ?ENGINE_VERSION_MINOR, [34]]),
	
	xbi_controller:stdout("feature usermove=1"),
	xbi_controller:stdout("feature colors=0"),
	xbi_controller:stdout("feature debug=1"),
	xbi_controller:stdout("feature done=1"),
	ok.
	


%%
%% Local Functions
%%
