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
%% Created: Jan 21, 2012
%% Description: TODO: Add description to new_file
-module(core_util).

-define(TEMP_FILE_DIRECTORY, "/dev/shm").

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("exception.hrl").
-include("antonius.hrl").

%%
%% Exported Functions
%%

-export([initVm/2]).
-export([userException/1]).
-export([userException/2]).
-export([inconsistencyException/1]).
-export([inconsistencyException/2]).
-export([strings_to_io_device/1]).

%%
%% API Functions
%%


%% @doc Perform once-per-VM initialization. LibPath is the directory
%% path to the libraries where native functions reside. Mode is
%% one of console, xboard, test.
%%
%% The value of init:core_nif/1 is returned.

-spec initVm(string(), atom()) -> tuple().

initVm(LibDir, Mode) ->
	
	LoadNative = core_nif:init(LibDir),
	
	core_state:start(),
	core_state:init([]),
	
	core_state:sput(mode, Mode),
	
	param_parameter:setNumberOfThreads(?MAX_THREADS_DEFAULT),
	
	LoadNative.



-spec userException(string()) -> no_return().

userException(Message) ->
	E = #exception{type=userException, message=Message, reason="invalid user input"},
	throw(E).



-spec userException(string(), [term()]) -> no_return().

userException(Format, Items) ->
	userException(io_lib:format(Format, Items)).




-spec inconsistencyException(string()) -> no_return().

inconsistencyException(Message) ->
	E = #exception{type=inconsistencyException, message=Message, reason="an internal error has occurred"},
	throw(E).


-spec inconsistencyException(string(), [term()]) -> no_return().

inconsistencyException(Format, Items) ->
	inconsistencyException(io_lib:format(Format, Items)).



%% @doc Returns an IO device to read from. The provided content
%% is that of the given list of strings.

-spec strings_to_io_device([string()]) -> {ok, i_device()}.

strings_to_io_device(List) ->
	FileName = ?TEMP_FILE_DIRECTORY ++ "/" ++ os:getpid() ++ "-" ++ integer_to_list(rand:uniform(1000000000)),
	case file:open(FileName, [write]) of
		{error, Reason} ->
			core_util:inconsistencyException("failed to write file, reason: ~p", [Reason]);
		{ok, OutDevice} ->
			strings_to_io_device_helper(List, OutDevice),
			case file:close(OutDevice) of
				{error, Reason} ->
					core_util:inconsistencyException("failed to close file, reason: ~p", [Reason]);
				ok ->
					case file:open(FileName, [read]) of
						{error, Reason} ->
							core_util:inconsistencyException("failed to read file, reason: ~p", [Reason]);
						{ok, _}=Result ->
							case file:delete(FileName) of
								{error, Reason} ->
										core_util:inconsistencyException("failed to delete file, reason: ~p", [Reason]);
								ok ->
									Result
							end
					end
			end
	end.


strings_to_io_device_helper([], _OutDevice) ->
	ok;

strings_to_io_device_helper([String|Tail], OutDevice) ->
	io:fwrite(OutDevice, "~s~n", [String]),
	strings_to_io_device_helper(Tail, OutDevice).


%%
%% Local Functions
%%



strings_as_file_test() ->
	{ok, InDevice} = strings_to_io_device(["abc", "", "123"]),
	strings_as_file_helper(InDevice, 0).

strings_as_file_helper(InDevice, N) ->
	case io:get_line(InDevice, "") of
		eof ->
			ok;
		Line ->
			if
				N =:= 0 ->
					?assertEqual(Line, "abc\n");
				N =:= 1 ->
					?assertEqual(Line, "\n");
				N =:= 2 ->
					?assertEqual(Line, "123\n")
			end,
			strings_as_file_helper(InDevice, N+1)
	end.


