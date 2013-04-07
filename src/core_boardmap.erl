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
%% Created: Jan 15, 2012
%% Description: Naive implementation, linear
%% search for any piece that occupies the given
%% square.
%%
%% Possible optimization is to use arrays, especially
%% if creation can be done quickly. Means avoid assigning
%% one element at a time.

-module(core_boardmap).

%%
%% Include files
%%
-include("antonius.hrl").
-include("material.hrl").
-include("boardmap.hrl").

%%
%% Exported Functions
%%


-export([createPiecesMap/2]).
-export([createPiecesMap/4]).
-export([get/2]).

%%
%% API Functions
%%



-spec createPiecesMap([#piece{}], [#piece{}]) -> #boardMap{}.

createPiecesMap(White, Black) ->	
	Tuple = list_to_tuple(waah(White, Black)),
	#boardMap{tuple=Tuple}.


%% @doc Returns a board map created by modification of a given map.

-spec createPiecesMap(#boardMap{}, #square{}, #square{}, #piece{}) -> #boardMap{}.

createPiecesMap(#boardMap{tuple=T}, #square{tupleIndex=J}, #square{tupleIndex=K}, Piece) ->
	U = setelement(J, T, null),
	#boardMap{tuple=setelement(K, U, Piece)}.




-spec get(#square{}, #boardMap{}) -> ppiece().

get(#square{tupleIndex=TupleIndex}, #boardMap{tuple=Tuple}) ->
	element(TupleIndex, Tuple).



waah(White, Black) ->
	R = doublets(White, []),
	Doublets = doublets(Black, R),
	complete(64, lists:reverse(lists:keysort(1, Doublets)), []).







doublets([], E) ->
	E;

doublets([#piece{square=#square{tupleIndex=TupleIndex}}=P|Tail], E) ->
	doublets(Tail, [{TupleIndex, P}| E]).
	



-spec complete(sqnum(), [{sqnum(), #piece{}}], [ppiece()]) -> [ppiece()].

complete(J, [], E) when J < 1 ->
	E;

complete(J, [], E) ->
	complete(J-1, [], [null|E]);

complete(J, [{TupleIndex, P}|Tail]=PP, E) ->
	if
		J > TupleIndex ->
			complete(J-1, PP, [null|E]);
		true ->
			complete(J-1, Tail, [P|E])
	end.

	














