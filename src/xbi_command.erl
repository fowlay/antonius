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
%% Description: TODO: Add description to xboardcommand
-module(xbi_command).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([setup/0]).
-export([execute/3]).

%%
%% API Functions
%%

setup() ->
	dict:from_list(
	   [
		  {"protover", {fun xbi_protover:execute/1, 1}},
		  {"sd", {fun xbi_sd:execute/1, 1}},
		  {"new", {fun xbi_new:execute/1, 0}},
		  {"usermove", {fun xbi_usermove:execute/1, 1}},
		  {"go", {fun xbi_go:execute/1, 0}},
		  {"remove", {fun xbi_remove:execute/1, 0}},
		  {"quit", {fun xbi_quit:execute/1, 0}}
	   ]		
	).

execute(Command, Arguments, Dict) ->
	{Fun, Nargs} = dict:fetch(Command, Dict),
	
	case length(Arguments) of
		Nargs ->
			true;
		OtherSize ->
			core_util:inconsistencyException("command: ~s, wrong number of arguments, expected: ~p, actual: ~p", [Command, Nargs, OtherSize])
	end,
	apply(Fun, [Arguments]).
	

%%
%% Local Functions
%%

