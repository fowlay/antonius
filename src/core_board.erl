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
%% Created: Jan 16, 2012
%% Description: TODO: Add description to board
-module(core_board).

%%
%% Include files
%%

-include("antonius.hrl").
-include("square.hrl").
-include("board.hrl").

-include_lib("eunit/include/eunit.hrl").


%%
%% Exported Functions
%%
-export([instance/1]).
-export([getSquare/3]).
-export([getSquare/2]).

%%
%% API Functions
%%

-spec instance(sid()) -> #board{}.

instance(Sid) ->
	case core_state:sget({Sid, board}) of
		null ->
			core_state:sput({Sid, board}, create()),
			instance(Sid);
		{ok, Board} ->
			Board
	end.


%% @doc ... no check against boundary error

-spec getSquare(sid(), integer(), integer()) -> #square{}.

getSquare(_Sid, File, Rank) ->
	#board{tuple=Tuple} = get(board),
	% array:get(square:linear(File, Rank), Array).
    element(File+(Rank-1)*8, Tuple).


%% @doc Lookup by board name.

-spec getSquare(sid(), string()) -> #square{}.

getSquare(Sid, Name) ->
	Letter = string:substr(Name, 1, 1),
	File = file(Letter),
	Rank = list_to_integer(string:substr(Name, 2, 1)),
	getSquare(Sid, File, Rank).

%%
%% Local Functions
%%

-spec create() -> #board{}.

create() ->
	create(0, 64, dict:new(), erlang:make_tuple(64, null)).



-spec create(smallint(), smallint(), dict:dict(), tuple()) -> #board{}.

create(J, N, Dict, Tuple) when J =:= N ->
	#board{dict=Dict, tuple=Tuple};

create(J, N, Dict, Tuple) ->
	File = (J rem 8) + 1,
	Rank = (J div 8) + 1,
	Square = core_square:square(File, Rank),
	NewDict = dict:store(key(File, Rank), Square, Dict),
	NewTuple = setelement(J+1, Tuple, Square),
	create(J+1, N, NewDict, NewTuple).


-spec key(smallint(), smallint()) -> string().

key(File, Rank) ->
	FileLetter = lists:nth(File, ["a", "b", "c", "d", "e", "f", "g", "h"]),
	RankNumber = lists:nth(Rank, ["1", "2", "3", "4", "5", "6", "7", "8"]),
	FileLetter ++ RankNumber.


%% @doc Return the file number corresponding to the
%% given letter (represented as a length-1 string).
%% No time-critical usage.

file(Letter) ->
	case lists:keyfind(Letter, 1, 
					   [{"a", 1}, 
						{"b", 2}, 
						{"c", 3}, 
						{"d", 4}, 
						{"e", 5}, 
						{"f", 6}, 
						{"g", 7}, 
						{"h", 8}]) of
		{_, Result} ->
			Result;
		_ ->
			core_util:inconsistencyException("cannot find file number for: ~s", [Letter])
	end.


board_test() ->
	
	Sid = test,
	
	core_state:start(),
	
	#board{dict=D, tuple=T} = instance(Sid),
	A1D = dict:fetch("a1", D),
	A2D = dict:fetch("a2", D),
	A1A = element(1, T),
	A2A = element(9, T),
	
	?assertEqual(A1D, A1A),
	%% ?assert(A1D =/= A2D), .. causes a dialyzer complaint
	?assertEqual(A2D, A2A),
	
	core_state:stop().



