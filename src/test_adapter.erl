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
%% Created: Jan 28, 2012
%% Description: See the batchtest.sh script on how to
%% invoke these tests.

-module(test_adapter).


%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("antonius.hrl").

%%
%% Exported Functions
%%

-export([run/2]).

%%
%% API Functions
%%



%% @doc Entry point for running tests from ct_run. The first
%% element of the given list is the test directory path.

-spec run({string(), o_device()}, atom()) -> ok.

run({TestDir, ResultDevice}, Test) ->
	ct:print("run/2 args: ~p", [[{TestDir, ResultDevice}, Test]]),
	io:fwrite(ResultDevice, "====== Running test: ~w ==========================~n", [Test]),
	ct:print("...", []),
	MetadataInDevice = getInDevice(TestDir, Test, "metadata"),
	ct:print("...", []),
	Mode = list_to_atom(getLine(MetadataInDevice)),
	run(TestDir, Test, MetadataInDevice, ResultDevice, Mode),
	file:close(MetadataInDevice).


%%
%% Local Functions
%%


%% @doc Executes a positive or negative test.

-spec run(string(), atom(), o_device(), o_device(), atom()) -> any().

run(TestDir, Test, MetadataInDevice, ResultDevice, positive) ->
	ct:print("run/5: ~p", [[TestDir, Test, MetadataInDevice, ResultDevice, positive]]),
	
	Sid = test,
	CommandsInDevice = getInDevice(TestDir, Test, "commands"),
	try cli_game:play(test, true, true, CommandsInDevice, ResultDevice) of
		GameState ->
			case core_gamestate:hasNodes(Sid) of
				true ->
					Key = getLine(MetadataInDevice),
					State = list_to_atom(getLine(MetadataInDevice)),
					Node = core_gamestate:getCurrentNodeFromGameState(GameState),
					?assertEqual(Key, core_node:toString(Node)),
					?assertEqual(State, core_gamestate:getState(Sid));
				_ ->
					State = list_to_atom(getLine(MetadataInDevice)),
					?assertEqual(State, core_gamestate:getState(Sid))
			end,
			file:close(CommandsInDevice),
			file:close(MetadataInDevice)
	catch
		throw:X ->
			file:close(CommandsInDevice),
			file:close(MetadataInDevice),
			ct:fail("caught throw: ~w~n", [X]);
		error:X ->
			file:close(CommandsInDevice),
			file:close(MetadataInDevice),
			ct:fail("caught error: ~w~n", [X]);
		exit:X ->
			file:close(CommandsInDevice),
			file:close(MetadataInDevice),
			ct:fail("caught exit: ~w~n", [X])
	end;


run(TestDir, Test, MetadataInDevice, ResultDevice, negative) ->
	CommandsInDevice = getInDevice(TestDir, Test, "commands"),
	try cli_game:play(test, true, true, CommandsInDevice, ResultDevice) of
		    GameState ->
			    file:close(CommandsInDevice),
			    file:close(MetadataInDevice),
			    ct:fail("test case did not fail as expected, game state: ~p~n", [core_gamestate:toString(GameState)])
	catch
		throw:{throw, CurrentNode, _X} ->
			file:close(CommandsInDevice),
			Key = getLine(MetadataInDevice),
 			file:close(MetadataInDevice),
			 ?assertEqual(Key, core_node:toString(CurrentNode));
		error:X ->
			file:close(CommandsInDevice),
			file:close(MetadataInDevice),
			ct:fail("caught error: ~w~n", [X]);
		exit:X ->
			file:close(CommandsInDevice),
			file:close(MetadataInDevice),
			ct:fail("caught exit: ~w~n", [X])
	end.





%% @doc Gets a newline-stripped line of input.

-spec getLine(i_device()) -> string().

getLine(InDevice) ->
	string:strip(string:strip(io:get_line(InDevice, ""), right, $\n)).



%% @doc Returns a device for the file specified by the
%% given test dir, test name and file name.

-spec getInDevice(string(), atom(), string()) -> i_device().

getInDevice(TestDir, Test, FileName) ->


ct:print("getInDevice: ~p", [[TestDir, Test, FileName]]),


	TestName = atom_to_list(Test),

ct:print("about to open: ~s", [TestDir++"/"++TestName++"/"++FileName]),



	case file:open(TestDir++"/"++TestName++"/"++FileName, [read]) of
		{ok, InDevice} ->
ct:print("have opened: ~s", [TestDir++"/"++TestName++"/"++FileName]),
			InDevice;
		Other ->
ct:print("failed to open: ~s", [TestDir++"/"++TestName++"/"++FileName]),
			core_util:inconsistencyException("failed to open file: ~p~n", [Other])
	end.



