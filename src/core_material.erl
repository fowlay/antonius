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
%% Description: TODO: Add description to material
-module(core_material).

-define(Asterisk, 42).

%%
%% Include files
%%
-include("antonius.hrl").
-include("board.hrl").
-include("piecetype.hrl").
-include("boardmap.hrl").
-include("material.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([getSquares/4]).
-export([parse/2]).
-export([toString/1]).
-export([create/3]).
-export([create/4]).

-export([getKing/1]).

-export([threats/3]).

-export([lookup/2]).

%%
%% API Functions
%%




%% @doc Lightweight constructor that sets pristine
%% to false.

-spec create(atom(), colour(), #square{}) -> #piece{}.

create(Type, Colour, Square) ->
	create(Type, Colour, Square, false).


%% @doc Constructor.

-spec create(atom(), colour(), #square{}, boolean()) -> #piece{}.

create(Type, Colour, Square, Pristine) ->
	#pieceType{order=Order, value=Value} = core_piecetype:pieceType(Type),
	if
		Type =:= whitePawn, Colour =:= black ->
			throw("colour inconsistency when creating white pawn");
		Type =:= blackPawn, Colour =:= white ->
			throw("colour inconsistency when creating black pawn");
		true ->
			#piece{type=Type, colour=Colour, square=Square, order=Order, value=Value, pristine=Pristine}
	end.




%% @doc Returns material corresponding to the given
%% string.


toString([]) ->
	"";

toString([X]) ->
	toStringHelper(X);

toString([X|Tail]) ->
	toStringHelper(X) ++ " " ++ toString(Tail).


toStringHelper(#piece{type=Type, square=#square{name=SquareName}, pristine=Pristine}) ->
	case Type of
		whitePawn ->
			SquareName;
		blackPawn ->
			SquareName;
		_ when Pristine =:= true ->
			#pieceType{abbreviation=A} = core_piecetype:pieceType(Type),
			"*" ++ A ++ SquareName;
		_ ->
			#pieceType{abbreviation=A} = core_piecetype:pieceType(Type),
			A ++ SquareName
	end.
	

-spec parse(string(), colour()) -> [#piece{}].

parse(String, Colour) ->
	Tokens = string:tokens(String, " "),
	parseHelper(Tokens, Colour).


-spec parseHelper([string()], colour()) -> [#piece{}].

parseHelper([], _Colour) ->
	[];

parseHelper([Token|Tail], Colour) ->
	P = parsePiece(Token, Colour, false),
	parseInsert(P, parseHelper(Tail, Colour)).


-spec parseInsert(#piece{}, [#piece{}]) -> [#piece{}].

parseInsert(#piece{}=P, []) ->
	[P];

parseInsert(#piece{order=V}=P, [#piece{order=W} | _]=M) when V < W ->
	[P | M];

parseInsert(#piece{order=V}=P, [#piece{order=W}=Q | Tail]) when V > W ->
	[Q |parseInsert(P, Tail)];

parseInsert(#piece{square=#square{tupleIndex=S}}=P, [#piece{square=#square{tupleIndex=T}} | _]=M) when S < T ->
	[P | M];

parseInsert(#piece{square=#square{tupleIndex=S}}=P, [#piece{square=#square{tupleIndex=T}}=Q | Tail]) when S > T ->
	[Q |parseInsert(P, Tail)];

parseInsert(_, _) ->
	throw("material clash").


-spec parsePiece(string(), colour(), boolean()) -> #piece{}.

parsePiece([?Asterisk | StringTail], Colour, _Pristine) ->
	parsePiece(StringTail, Colour, true);

parsePiece(String, Colour, _Pristine) when length(String) =:= 2 ->
	% a pawn
	Square = parseSquare(String),
	#pieceType{name=Name} = core_piecetype:pieceType(pawn, Colour),
	create(Name, Colour, Square);

parsePiece(String, Colour, Pristine) when length(String) =:= 3 ->
	% a non-pawn
	[A, B, C] = String,
	#pieceType{name=Name} = core_piecetype:pieceType([A]),
	Square = parseSquare([B, C]),
	create(Name, Colour, Square, Pristine).


-spec parseSquare(string()) -> #square{}.

parseSquare(S) ->
	#board{dict=Dict} = get(board),
	case dict:is_key(S, Dict) of
		false ->
			throw("not a square on the board: "++S);
		true ->
			dict:fetch(S, Dict)
	end.





%% @doc Returns a list of square.
%%
%% The return type is not uniform: Either a list of beams,
%% or a list of squares.

-spec getSquares(#piece{}, colour(), #boardMap{}, ppiece()) -> [any()].
	
getSquares(#piece{type=queen, square=Square}, ToMove, BoardMap, _MovedPawn) ->
	#square{rookBeams=RookBeams, bishopBeams=BishopBeams} = Square,
	Board = get(board),
	R = doBeams(RookBeams, [], ToMove, BoardMap, Board),
	doBeams(BishopBeams, R, ToMove, BoardMap, Board);

getSquares(#piece{type=king, square=Square}, ToMove, BoardMap, _MovedPawn) ->
	#square{kingMoves=KingMoves} = Square,
	Board = get(board),
	doMoves(KingMoves, ToMove, BoardMap, Board);

getSquares(#piece{type=bishop, square=Square}, ToMove, BoardMap, _MovedPawn) ->
	#square{bishopBeams=BishopBeams} = Square,
	Board = get(board),
	doBeams(BishopBeams, [], ToMove, BoardMap, Board);

getSquares(#piece{type=rook, square=Square}, ToMove, BoardMap, _MovedPawn) ->
	#square{rookBeams=RookBeams} = Square,
	Board = get(board),
	doBeams(RookBeams, [], ToMove, BoardMap, Board);
	
getSquares(#piece{type=knight, square=Square}, ToMove, BoardMap, _MovedPawn) ->
	#square{knightJumps=KnightJumps} = Square,
	Board = get(board),
	doMoves(KnightJumps, ToMove, BoardMap, Board);

getSquares(#piece{type=whitePawn, square=Square}, ToMove, BoardMap, MovedPawn) ->
	#square{whitePawnMoves=WhitePawnMoves, whitePawnKills=WhitePawnKills, whitePawnEpSquares=WPES} = Square,
	Board = get(board),
	A = doPawnMoves(WhitePawnMoves, BoardMap, Board),
	B = doPawnKills(WhitePawnKills, A, ToMove, BoardMap, Board),
	doEpWhite(WPES, B, ToMove, BoardMap, Board, MovedPawn);

getSquares(#piece{type=blackPawn, square=Square}, ToMove, BoardMap, MovedPawn) ->
	#square{blackPawnMoves=BlackPawnMoves, blackPawnKills=BlackPawnKills, blackPawnEpSquares=BPES} = Square,
	Board = get(board),
	A = doPawnMoves(BlackPawnMoves, BoardMap, Board),
	B = doPawnKills(BlackPawnKills, A, ToMove, BoardMap, Board),
	doEpBlack(BPES, B, ToMove, BoardMap, Board, MovedPawn);

%% TODO ... not good style, do static analysis
getSquares(A, B, C, D) ->
	throw(lists:flatten(io_lib:format("match problem: ~w, ~w, ~w, ~w~n", [A, B, C, D]))).
	
	

%% @doc Pick up the en-passant square that a white pawn can go to.
%% This yields a square only if the white pawn is at rank 5, and
%% there is a just moved black pawn in the neighbour file at rank 5.

-spec doEpWhite([integer()], [#square{}], colour(), #boardMap{}, #board{}, #piece{}) -> [#square{}].

doEpWhite([], E, _, _, _, _) ->
	E;

doEpWhite([Index|Tail], E, ToMove, BoardMap, #board{tuple=Tuple}=B, MovedPawn) ->
	#square{blackPawnSkippedSquare=BPSS}=S = element(Index, Tuple),
	case bmget(S, BoardMap) of
		#piece{type=blackPawn}=X ->
			if
				X =:= MovedPawn ->
					[element(BPSS, Tuple) | E];
				true ->
					doEpWhite(Tail, E, ToMove, BoardMap, B, MovedPawn)
			end;
		_ ->
			doEpWhite(Tail, E, ToMove, BoardMap, B, MovedPawn)
	end.


-spec doEpBlack([integer()], [#square{}], colour(), #boardMap{}, #board{}, #piece{}) -> [#square{}].

doEpBlack([], E, _, _, _, _) ->
	E;

doEpBlack([Index|Tail], E, ToMove, BoardMap, #board{tuple=Tuple}=B, MovedPawn) ->
	#square{whitePawnSkippedSquare=WPSS}=S = element(Index, Tuple),
	case bmget(S, BoardMap) of
		#piece{type=whitePawn}=X ->
			if
				X =:= MovedPawn ->
					[element(WPSS, Tuple) | E];
				true ->
					doEpBlack(Tail, E, ToMove, BoardMap, B, MovedPawn)
			end;
		_->
			doEpBlack(Tail, E, ToMove, BoardMap, B, MovedPawn)
	end.
					
	
%% @doc Returns the king, which is trusted to exist.

-spec getKing([#piece{}]) -> #piece{}.

getKing([#piece{type=king}=P | _]) ->
	P;

getKing([_|Tail]) ->
	getKing(Tail).
  


-spec threats(#piece{}, #square{}, #boardMap{}) -> boolean().

%% @doc Returns true if the given piece threats the given
%% square.

threats(#piece{type=bishop, square=#square{x=X, y=Y, bishopBeams=Beams}},
		#square{x=Tx, y=Ty}=ThreatedSquare, 
		BoardMap) ->
	Dx = Tx - X,
	Dy = Ty - Y,
	if
		Dx =/= Dy, Dx =/= (-Dy) ->
			false;
		true ->
			Board = get(board),
 			threatsBishopOuter(Beams, ThreatedSquare, Board, BoardMap)
	end;

threats(#piece{type=rook, square=#square{x=X, y=Y, rookBeams=Beams}}, 
		#square{x=Tx, y=Ty}=ThreatedSquare,
		BoardMap) ->
	Dx = Tx - X,
	Dy = Ty - Y,
	if
		Dx =/= 0, Dy =/= 0 ->
			false;
		true ->
			Board = get(board),
			threatsBishopOuter(Beams, ThreatedSquare, Board, BoardMap)
	end;

threats(#piece{type=queen, square=#square{x=X, y=Y, rookBeams=RB, bishopBeams=BB}}, 
		#square{x=Tx, y=Ty}=ThreatedSquare,
		BoardMap) ->
	Dx = Tx - X,
	Dy = Ty - Y,
	if
		Dx =/= 0, Dy =/= 0, Dx =/= Dy, Dx =/= (-Dy) ->
			false;
		true ->
			Board = get(board),
			case threatsBishopOuter(RB, ThreatedSquare, Board, BoardMap) of
				true ->
					true;
				_ ->
					threatsBishopOuter(BB, ThreatedSquare, Board, BoardMap)
			end
	end;

threats(#piece{type=king, square=#square{kingMoves=KM}}, 
		ThreatedSquare,
		_BoardMap) ->
	Board = get(board),
	threatsKnightLike(KM, ThreatedSquare, Board);

threats(#piece{type=knight, square=#square{knightJumps=KJ}},
		ThreatedSquare,
		_BoardMap) ->
	Board = get(board),
	threatsKnightLike(KJ, ThreatedSquare, Board);

threats(#piece{type=whitePawn, square=#square{whitePawnKills=WPK}},
		ThreatedSquare,
		_BoardMap) ->
	Board = get(board),
	threatsKnightLike(WPK, ThreatedSquare, Board);


threats(#piece{type=blackPawn, square=#square{blackPawnKills=BPK}},
		ThreatedSquare,
		_BoardMap) ->
	Board = get(board),
	threatsKnightLike(BPK, ThreatedSquare, Board).
		


-spec threatsKnightLike([integer()], #square{}, #board{}) -> boolean().

threatsKnightLike([], _, _) ->
	false;

threatsKnightLike([Index|Tail], ThreatedSquare, #board{tuple=Tuple}=Board) ->
	%Square = array:get(Index, Array),
	Square = element(Index, Tuple),
	if 
		Square =:= ThreatedSquare ->
			true;
		true ->
			threatsKnightLike(Tail, ThreatedSquare, Board)
	end.




-spec threatsBishopOuter([[integer()]], #square{}, #board{}, #boardMap{}) -> boolean().

threatsBishopOuter([], _, _, _) ->
	false;

threatsBishopOuter([Beam|Tail], ThreatedSquare, Board, BoardMap) ->
	case threatsBishopInner(Beam, ThreatedSquare, Board, BoardMap) of
		true ->
			true;
		_ ->
			threatsBishopOuter(Tail, ThreatedSquare, Board, BoardMap)
	end.


-spec threatsBishopInner([integer()], #square{}, #board{}, #boardMap{}) -> boolean().

threatsBishopInner([], _, _, _) ->
	false;

threatsBishopInner([Index|Tail], ThreatedSquare, #board{tuple=Tuple}=Board, BoardMap) ->
	%Square = array:get(Index, Array),
	Square = element(Index, Tuple),
	if
		Square =:= ThreatedSquare ->
			true;
		true ->
			case bmget(Square, BoardMap) of
				null ->
					threatsBishopInner(Tail, ThreatedSquare, Board, BoardMap);
				_ ->
					false
			end
	end.

	
		




%%
%% Local Functions
%%

%% @doc Returns a list of reachable squares
%% prepended to the given list E.

-spec doBeams([[integer()]], [#square{}], colour(), #boardMap{}, #board{}) -> [#square{}].

doBeams([], E, _ToMove, _BoardMap, _Board) ->
	E;

doBeams([Beam|Tail], E, ToMove, BoardMap, Board) ->
	R = doBeam(Beam, E, ToMove, BoardMap, Board),
	doBeams(Tail, R, ToMove, BoardMap, Board).


%% @doc Returns a list of squares prepended to
%% the given list E.

-spec doBeam([integer()], [#square{}], colour(), #boardMap{}, #board{}) -> [#square{}].
	
doBeam([], E, _ToMove, _BoardMap, _Board) ->
	E;

doBeam([Index|Tail], E, ToMove, BoardMap, #board{tuple=Tuple}=B) ->
	%S = array:get(Index, Array),
	S = element(Index, Tuple),
	case bmget(S, BoardMap) of
		null ->
			doBeam(Tail, [S|E], ToMove, BoardMap, B);
		#piece{colour=ToMove} ->
			E;
		_ ->
			[S|E]
	end.


%% @doc Returns a list of reachable squares. The given
%% list of indexes is considered.
%%
%% TODO, optimize the number of clauses in case..end
%%
%% TODO, the naming of these functions is not so good

-spec doMoves([integer()], colour(), #boardMap{}, #board{}) -> [#square{}].

doMoves([], _ToMove, _BoardMap, _Board) ->
	[];

doMoves([Index|Tail], ToMove, BoardMap, #board{tuple=Tuple}=B) ->
	%S = array:get(Index, Array),
	S = element(Index, Tuple),
	case bmget(S, BoardMap) of
		null ->
			[S|doMoves(Tail, ToMove, BoardMap, B)];
		#piece{colour=ToMove} ->
			doMoves(Tail, ToMove, BoardMap, B);
		_ ->
			[S|doMoves(Tail, ToMove, BoardMap, B)]
	end.


%% @doc The given list is maximum length-2. Allows optimizing?

-spec doPawnMoves([integer()], #boardMap{}, #board{}) -> [#square{}].

%% doPawnMoves([], _ToMove, _BoardMap, _Board) ->
%% 	[];
%% 
%% doPawnMoves([Index|Tail], ToMove, BoardMap, #board{tuple=Tuple}=B) ->
%% 	%S = array:get(Index, Array),
%% 	S = element(Index+1, Tuple),
%% 	case boardmap:get(S, BoardMap) of
%% 		null ->
%% 			[S|doPawnMoves(Tail, ToMove, BoardMap, B)];
%% 		_ ->
%% 			[]
%% 	end.



%% manually optimized version, no recursive calls

doPawnMoves([J], #boardMap{tuple=BMTuple}, #board{tuple=BoardTuple}) ->
	#square{tupleIndex=TI}=S = element(J, BoardTuple),
	case element(TI, BMTuple) of
		null ->
			[S];
		_ ->
			[]
	end;

doPawnMoves([J, K], #boardMap{tuple=BMTuple}, #board{tuple=BTuple}) ->
	#square{tupleIndex=TI}=S = element(J, BTuple),
	case element(TI, BMTuple) of
		null ->
			#square{tupleIndex=TI2}=S2 = element(K, BTuple),
			case element(TI2, BMTuple) of
				null ->
					[S, S2];
				_ ->
					[S]
			end;
		_ ->
			[]
	end.


-spec doPawnKills([integer()], [#square{}], colour(), #boardMap{}, #board{}) -> [#square{}].

doPawnKills([], E, _ToMove, _BoardMap, _Board) ->
	E;

doPawnKills([Index|Tail], E, ToMove, BoardMap, #board{tuple=Tuple}=B) ->
	%S = array:get(Index, Array),
	S = element(Index, Tuple),
	case bmget(S, BoardMap) of
		null ->
			doPawnKills(Tail, E, ToMove, BoardMap, B);
		#piece{colour=ToMove} ->
			doPawnKills(Tail, E, ToMove, BoardMap, B);
		_ ->
			[S|doPawnKills(Tail, E, ToMove, BoardMap, B)]
	end.


lookup([], _) ->
	null;

lookup([#piece{square=Square}=P|_], Square) ->
	P;

lookup([_|Tail], Square) ->
	lookup(Tail, Square).



-compile({inline,[bmget/2]}).

bmget(#square{tupleIndex=TupleIndex}, #boardMap{tuple=Tuple}) ->
	element(TupleIndex, Tuple).

%% --------

%% do_test() ->
%% 	state:start(),
%% 	state:init([]),
%% 	
%% 	board:instance(),
%% 	
%% 	M = parse("a2", white),
%% 	?assertEqual(1, length(M)),
%% 	[#piece{type=Type, square=#square{number=Order}}] = M,
%% 	?assertEqual(whitePawn, Type),
%% 	?assertEqual(8, Order),
%% 	
%% 	S = toString(M),
%% 	?assertEqual("a2", S),
%% 	
%% 	
%% 	N = parse("*Ra5 Be4 e3 g7 Qa1", black),
%% 	?assertEqual("Qa1 Ra5 Be4 e3 g7", toString(N)),
%% 	
%% 	state:stop().




			



