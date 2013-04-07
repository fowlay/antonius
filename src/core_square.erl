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
%% Created: Sep 30, 2011
%% Description: TODO: Add description to square
-module(core_square).

%% -include_lib("eunit/include/eunit.hrl").



%%
%% Include files
%%

-include("antonius.hrl").
-include("square.hrl").
-include("node.hrl").

%%
%% Exported Functions
%%
-export([square/2, linear/2, beam/2, dropEmptyLists/1, rookBeams/2, bishopBeams/2]).

-export([knightJumps/2]).
-export([kingMoves/2]).

-export([unitVectors/0]).

-export([isThreated/3]).

-export([toString/1]).

%%
%% API Functions
%%

%% @doc Returns a newly created square of file X
%% and rank Y, range 1..8.

square(X, Y) ->
	#square{x=X, 
			y=Y, 
			
			%fileValue = 25 + (if X < 5 -> (X-1)*5; true -> (8-X)*5 end),
			
			colour=getColour(X, Y), 
			file=getFile(X), 
			rank=getRank(Y), 
			name=getName(X, Y), 

			tupleIndex=linear(X, Y)+1,
			
			rookBeams=rookBeams(X, Y),
			bishopBeams=bishopBeams(X, Y),
			knightJumps=knightJumps(X, Y),
			kingMoves=kingMoves(X, Y),
			
			whitePawnMoves=whitePawnMoves(X, Y),
			whitePawnKills=whitePawnKills(X, Y),
			whitePawnSkippedSquare=pawnSkippedSquare(X, Y),
			whitePawnEpSquares=whitePawnEpSquares(X, Y),
			
			blackPawnMoves=blackPawnMoves(X, Y),
			blackPawnKills=blackPawnKills(X, Y),
			blackPawnSkippedSquare=pawnSkippedSquare(X, Y),
			blackPawnEpSquares=blackPawnEpSquares(X, Y),
			
			epKillSquare=epKillSquare(X, Y),
			
			enPassantKiller=getEnPassantKiller(Y),
			
			castleLongCorner=castleLongCorner(X, Y),
			castleShortCorner=castleShortCorner(X, Y),
			castleLongPath=castleLongPath(X, Y),
			castleShortPath=castleShortPath(X, Y)
		   
		    
		   
		   
		   
		   }.
	
%% @doc Returns true if the piece at the given square is
%% threatened by any of the given pieces. Colours are
%% trusted to be opposite.
%%
%% TODO: Is this function well placed?
%%
%% TODO: The argument should be BoardMap instead of Node.

-spec isThreated(#square{}, #node{}, [#piece{}]) -> boolean().

isThreated(_Square, _Node, []) ->
	false;

isThreated(Square, #node{pieces=BoardMap}=N, [#piece{}=P|Tail]) ->
	case core_material:threats(P, Square, BoardMap) of
		true ->
			true;
		_->
			isThreated(Square, N, Tail)
	end.


	
	%%
	%% Local Functions
	%%

getColour(X, Y) ->
	Ex = isEven(X),
	Ey = isEven(Y),
	if
		Ex =:= Ey ->
			black;
		true ->
			white
	end.

isEven(K) ->
	if
		(K rem 2) == 1 -> false;
		true -> true
	end.
	
getName(X, Y) ->
	F = getFile(X),
	R = getRank(Y),
	lists:append(F, R).

getFile(X) ->
	lists:nth(X, ["a", "b", "c", "d", "e", "f", "g", "h"]).

getRank(Y) ->
	lists:nth(Y, ["1", "2", "3", "4", "5", "6", "7", "8"]).





rookBeams(X, Y) ->
	dropEmptyLists([beam({X, Y}, D) || D <- unitVectors()]).

bishopBeams(X, Y) ->
	dropEmptyLists([beam({X, Y}, D) || D <- diagonalVectors()]).


whitePawnMoves(X, Y) ->
	if
		Y =:= 2 ->
			[tupleIndex(X, Y+1), tupleIndex(X, Y+2)];
		Y < 8 ->
			[tupleIndex(X, Y+1)];
		true ->
			[]
	end.

whitePawnKills(X, Y) ->
	if
		Y < 8 ->
			[ tupleIndex(Ux, Uy) ||
				 {Ux, Uy} <- [{X-1, Y+1}, {X+1, Y+1}], onBoard(Ux, Uy)
			];
		true ->
			[]
	end.



whitePawnEpSquares(X, Y) ->
	if 
		Y =/= 5 ->
			[];
		true ->
			[tupleIndex(Ux, Uy) ||
			   {Ux, Uy} <- [{X-1, Y}, {X+1, Y}],
			   onBoard(Ux, Uy)
			]
	end.



blackPawnMoves(X, Y) ->
	if
		Y =:= 7 ->
			[tupleIndex(X, Y-1), tupleIndex(X, Y-2)];
		Y > 1 ->
			[tupleIndex(X, Y-1)];
		true ->
			[]
	end.

blackPawnKills(X, Y) ->
	if
		Y > 1 ->
			[ tupleIndex(Ux, Uy) ||
				 {Ux, Uy} <- [{X-1, Y-1}, {X+1, Y-1}], onBoard(Ux, Uy)
			];
		true ->
			[]
	end.


-spec pawnSkippedSquare(filenum(), ranknum()) -> xsqnum().

pawnSkippedSquare(X, Y) ->
	if
		Y =:= 4 ->
			tupleIndex(X, 3);
		Y =:= 5 ->
			tupleIndex(X, 6);
		true ->
			0
	end.


blackPawnEpSquares(X, Y) ->
	if 
		Y =/= 4 ->
			[];
		true ->
			[tupleIndex(Ux, Uy) ||
			   {Ux, Uy} <- [{X-1, Y}, {X+1, Y}],
			   onBoard(Ux, Uy)
			]
	end.


-spec epKillSquare(filenum(), ranknum()) -> xsqnum().

epKillSquare(X, Y) ->
	if 
		Y =:= 3 ->
			tupleIndex(X, 4);
		Y =:= 6 ->
			tupleIndex(X, 5);
		true ->
			0
	end.


castleLongCorner(X, Y) ->
	if 
		X =:= 5 ->
			if
				(Y =:= 1) or (Y =:= 8) ->
					tupleIndex(1, Y);
				true ->
					null
			end;
		true ->
			null
	end.

castleShortCorner(X, Y) ->
	if 
		X =:= 5 ->
			if
				(Y =:= 1) or (Y =:= 8) ->
					tupleIndex(8, Y);
				true ->
					null
			end;
		true ->
			null
	end.

castleLongPath(X, Y) ->
	if 
		X =:= 5 ->
			if
				(Y =:= 1) or (Y =:= 8) ->
					[tupleIndex(5, Y), tupleIndex(4, Y), tupleIndex(3, Y), tupleIndex(2, Y)];
				true ->
					null
			end;
		true ->
			null
	end.

castleShortPath(X, Y) ->
	if 
		X =:= 5 ->
			if
				(Y =:= 1) or (Y =:= 8) ->
					[tupleIndex(5, Y), tupleIndex(6, Y), tupleIndex(7, Y)];
				true ->
					null
			end;
		true ->
			null
	end.


%% @doc Returns a list of linear square coordinates that
%% represent an outward beam from Rx:Ry in the direction Dx:Dy.
%% Rx:Ry itself is not included. Coordinates are always on-board.
%% The resulting list may be empty.

beam({Rx, Ry}, {Dx, Dy}) ->
	[tupleIndex(Rx + M*Dx, Ry + M*Dy) || 
	   M <- [1, 2, 3, 4, 5, 6, 7],
	   onBoard(Rx + M*Dx, Ry + M*Dy)
	].


knightJumps(X, Y) ->
	[tupleIndex(X+2*Ax+Bx, Y+2*Ay+By) || 
	   {Ax, Ay} <- unitVectors(),
	   {Bx, By} <- unitVectors(),
	   Ax*Bx + Ay*By =:= 0,	
	   onBoard(X+2*Ax+Bx, Y+2*Ay+By)
	].

kingMoves(X, Y) ->
	[tupleIndex(X+Dx, Y+Dy) || 
	   Dx <- [-1, 0, 1],
	   Dy <- [-1, 0, 1],
	   Dx*Dx + Dy*Dy > 0,
	   onBoard(X+Dx, Y+Dy)
	].


%% sp({Ax, Ay}, {Bx, By}) ->
%%	 Ax*Bx + Ay*By.


unitVectors() ->
	[{X, Y} ||
	  X <- [-1, 0, 1],
	  Y <- [-1, 0, 1],
	  X*X + Y*Y =:= 1
	].

diagonalVectors() ->
	[{X, Y} ||
	  X <- [-1, 1],
	  Y <- [-1, 1]
	].


-spec linear(integer(), integer()) -> integer().

linear(X, Y) ->
	(X-1) + (Y-1)*8.



-spec tupleIndex(filenum(), ranknum()) -> sqnum().

tupleIndex(X, Y) ->
	X + (Y-1)*8.



onBoard(X, Y) ->
	  (X >= 1) andalso
	  (X =< 8) andalso
	  (Y >= 1) andalso
	  (Y =< 8).


dropEmptyLists(M) ->
	[ L || L <- M, L =/= []].




getEnPassantKiller(Y) ->
	if
		Y =:= 4 -> black;
		Y =:= 5 -> white;
		true -> null
	end.


-spec toString(xsquare()) -> string().


toString(null) ->
	"null";

toString(#square{name=Name}) ->
	Name.

% ----------------------

%% need rework since index mapping was changed

%% square_test() ->
%% 	S = square(1, 1),
%% 	?assertEqual(black, S#square.colour),
%% 	?assertEqual(1, S#square.x),
%% 	?assertEqual(1, S#square.y),
%% 	?assertEqual(0, S#square.number),
%% 	
%% 	T = square(4, 4),
%% 	?assertEqual(black, T#square.enPassantKiller).
%% 
%% basic_test() ->
%% 	?assertEqual(4, length(unitVectors())),
%% 	?assertEqual(8, length(knightJumps(3, 3))),
%% 	?assertEqual(8, length(kingMoves(2, 2))),
%% 	
%% 	?assertEqual([0, 1, 2, 8, 10, 16, 17, 18], lists:sort(kingMoves(2,2))),
%% 	?assertEqual([1, 8, 9], lists:sort(kingMoves(1,1))),
%% 	
%% 	?assertEqual([46, 53], lists:sort(knightJumps(8,8))),
%% 	?assertEqual([28, 30, 35, 39, 51, 55, 60, 62], lists:sort(knightJumps(6,6))),
%% 	
%% 	?assertEqual([[1, 2, 3, 4, 5, 6, 7], [8, 16, 24, 32, 40, 48, 56]], lists:sort(rookBeams(1,1))),
%% 	?assertEqual([[1], [8], [10, 11, 12, 13, 14, 15], [17, 25, 33, 41, 49, 57]], lists:sort(rookBeams(2,2))),
%% 	
%% 	?assertEqual([[9, 18, 27, 36, 45, 54, 63]], lists:sort(bishopBeams(1,1))),
%% 	?assertEqual([[0], [2], [16], [18, 27, 36, 45, 54, 63]], lists:sort(bishopBeams(2,2))),
%% 	
%% 	true.


