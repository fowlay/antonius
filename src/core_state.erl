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
%% Created: Jan 20, 2012
%% Description: TODO: Add description to new_file
-module(core_state).

%%
%% Include files
%%


-define(PROCESS, ?MODULE).

%%
%% Exported Functions
%%
-export([start/0]).
-export([init/1]).
-export([stop/0]).
-export([sget/1]).
-export([sput/2]).
-export([remove/1]).
-export([reset/1]).
-export([incrementAndGet/2]).
-export([dump/0]).
-export([clear/0]).

-export([serverProcess/1]).

%%
%% API Functions
%%

start() ->
	Pid = spawn(?MODULE, serverProcess, [createMap()]),
	register(?PROCESS, Pid).

init(_) ->
	ok.



sget(Key) ->
	?PROCESS ! {get, self(), Key},
	receive
		T ->
			T
	end.


-spec sput(atom(), any()) ->  ok.

sput(Key, Value) ->
	?PROCESS ! {put, self(), Key, Value},
	receive
		{ok} ->
			ok
	end.


-spec remove(atom()) -> ok.

remove(Key) ->
	?PROCESS ! {remove, self(), Key},
	receive
		{ok} ->
			ok
	end.


stop() ->
	?PROCESS ! {stop, self()},
	receive
		{ok} ->
			ok
	end.


reset(Counter) ->
	?MODULE:sput(Counter, 0).


incrementAndGet(Counter, Increment) ->
	?PROCESS ! {incrementAndGet, self(), Counter, Increment},
	receive
		null ->
			null;
		T ->
			T
	end.


dump() ->
	?PROCESS ! {dump, self()},
	receive
		{ok, State} ->
			State
	end.


clear() ->
	?PROCESS ! {clear, self()},
	receive
		{ok} ->
			ok
	end.



%%
%% Local Functions
%%

createMap() ->
	[].


lookup(_, []) ->
	null;

lookup(Key, [[Key|Value]|_]) ->
	Value;

lookup(Key, [_|Tail]) ->
	lookup(Key, Tail).


update(Key, Value, []) ->
	[[Key|Value]];

update(Key, Value, [[Key|_]|Tail]) ->
	[[Key|Value]|Tail];

update(Key, Value, [Head|Tail]) ->
	[Head|update(Key, Value, Tail)].


remove(_, []) ->
	[];

remove(Key, [[Key|_]|Tail]) ->
	Tail;

remove(Key, [[_|_]|Tail]) ->
	remove(Key, Tail).


serverProcess(State) ->
	receive
		{get, Caller, Key} ->
			case lookup(Key, State) of
				null ->
					Caller ! null;
				Value ->
					Caller ! {ok, Value}
			end,
			serverProcess(State);
		{put, Caller, Key, Value} ->
			NewState = update(Key, Value, State),
			Caller ! {ok},
			serverProcess(NewState);
		{remove, Caller, Key} ->
			NewState = remove(Key, State),
			Caller ! {ok},
			serverProcess(NewState);
		{incrementAndGet, Caller, Counter, Increment} ->
			case lookup(Counter, State) of
				null ->
					Caller ! null,
					serverProcess(State);
				Value ->
					NewValue = Value + Increment,
					NewState = update(Counter, NewValue, State),
					Caller ! {ok, NewValue},
					serverProcess(NewState)
			end;
		{dump, Caller} ->
			Caller ! {ok, State},
			serverProcess(State);
		{clear, Caller} ->
			Caller ! {ok},
			serverProcess(createMap());
		{stop, Caller} ->
			Caller ! {ok}
	end.
