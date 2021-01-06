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
%% Description: TODO: Add description to node
%% TODO: abBlack/4 has weird order of arguments, align with abWhite
-module(core_node).


%
% INHERITED BUG in ABBlack, abWhite ... see TODO
%




%%
%% Include files
%%
-include("antonius.hrl").
-include("piecetype.hrl").
-include("board.hrl").
-include("node.hrl").
-include("abresult.hrl").






%%
%% Exported Functions
%%

-export([create/3]).
-export([create/6]).
-export([insert/2]).

-export([insertPiece/2]).
-export([insertPiece/4]).

-export([expand/2]).
-export([makeMove/5]).

-export([verifyLegal/3]).

-export([expandAndSelect/3]).
-export([expandAndSelect/5]).

-export([alphaBetaRoot/3]).

-export([key/1]).
-export([toString/1]).

-export([abWhite/5]).
-export([abBlack/5]).

-export([isChecked/1]).

-export([faultyValue/1]).

-export([getState/1]).

-export([isCheckmated/2]).
-export([isStalemate/2]).

-export([toStringList/2]).

-export([loss/3]).

%% TODO, remove if optimizing was good
-export([coverageValueHelper/2]).


%%
%% API Functions
%%


%% @doc Return a stringlist that depicts the chessboard with
%% black and white pieces.

-spec toStringList(sid(), #node{}) ->  [string()].

toStringList(_Sid,        % TODO, does not need Sid
			 #node{pieces=Pieces}) ->
	#board{tuple=Tuple} = get(board),
	Files = "    A   B   C   D   E   F   G   H",
	Border = " +--------------------------------+",
	[_|Result] = toStringList(0, Pieces, Tuple, [Border, Files]),
	[Files, Border|Result].
	

%% @doc Builds a stringlist. Each rank is preceded by an extra
%% line.
	
-spec toStringList(integer(), #boardMap{}, tuple(), [string()]) -> 
                      [string()].
	
toStringList(8, _, _, R) ->
	R;

toStringList(Y, Pieces, Tuple, R) ->
	Rank = integer_to_list(Y+1),
	S = Rank ++ "|" ++ toString(0, Y, Pieces, Tuple) ++ "|" ++ Rank,
	toStringList(Y + 1, Pieces, Tuple, [" |                                |", S|R]).


%% @doc Returns a string that represents the given Y coordinate.

-spec toString(integer(), integer(), #boardMap{}, tuple()) ->  string().

toString(8, _, _, _) ->
	"";

toString(X, Y, Pieces, Tuple) ->
	Square = element(8*Y + X + 1, Tuple),
	case core_boardmap:get(Square, Pieces) of
		null ->
			"  - " ++ toString(X + 1, Y, Pieces, Tuple);
		#piece{colour=Colour, type=Type} ->
			#pieceType{abbreviation=A} = core_piecetype:pieceType(Type),
		case Colour of
			white ->
				 "  " ++ A ++ " " ++ toString(X + 1, Y, Pieces, Tuple);
			 black ->
				 " #" ++ A ++ " " ++ toString(X + 1, Y, Pieces, Tuple)
		 end
	end.



%% @doc Returns the piece that is present in the
%% given node and not present in the subsequent
%% node. If no such piece exists null is returned.

-spec loss(#node{}, colour(), #node{}) -> ppiece().

loss(Node, Player, SubsequentNode) ->
	Pieces = getMaterial(Node, Player),
	lossHelper(Pieces, Player, SubsequentNode).
	


-spec lossHelper([#piece{}], colour(), #node{}) -> ppiece().

lossHelper([], _, _) ->
	null;

lossHelper([P|Z], Player, SubsequentNode) ->
	case stillAround(Player, P, SubsequentNode) of
		true ->
			lossHelper(Z, Player, SubsequentNode);
		_ ->
			P
	end.


%% Return null if the material of the player has not changed.
%% Otherwise return the piece that was lost (i e the piece that
%% exists in this node but not in the subsequent node)

stillAround(Player, #piece{square=Square}, #node{pieces=BoardMap}) ->
	case core_boardmap:get(Square, BoardMap) of
		#piece{colour=Player} ->
			true;
		_ ->
			false
	end.





-spec create(string(), string(), colour()) -> #node{}.

create(WhiteMaterial, BlackMaterial, ToMove) ->
	W = core_material:parse(WhiteMaterial, white),
	B = core_material:parse(BlackMaterial, black),
	create(W, B, ToMove, null, 0, 0).
	


-spec create([#piece{}], [#piece{}], colour(), xpiece(), smallint(), smallint()) -> #node{}.

create(White, Black, ToMove, MovedPawn, IsWhiteCastled, IsBlackCastled) ->
	% state:incrementAndGet(nodeCount, 1),
	% Pieces = core_nif:createPiecesMap(White, Black, null),
	Pieces = core_nif:createPiecesMap(White, Black, boardMap, null),
	Result = #node{
		  white = White,
		  black = Black,
		  pieces = Pieces,
		  toMove = ToMove,
		  blackAdvantage = 
			(5*eval3(Black, Pieces, MovedPawn) + IsBlackCastled)
		  - (5*eval3(White, Pieces, MovedPawn) + IsWhiteCastled),
		  movedPawn = MovedPawn,
		  isWhiteCastled = IsWhiteCastled,
		  isBlackCastled = IsBlackCastled
		 },
%% 	,
%% 	logger:log("~s ~w~n", [key(Result), Result#node.blackAdvantage]), 
	Result.


%% -spec create([#piece{}], [#piece{}], colour(), xpiece(), smallint(), smallint(), tuple()) -> #node{}.
%% 
%% create(White, Black, ToMove, MovedPawn, IsWhiteCastled, IsBlackCastled, {#node{pieces=Map}, From, To, Piece}) ->
%% 	% state:incrementAndGet(nodeCount, 1),
%% 	
%% 	%Pieces = boardmap:createPiecesMap(White, Black),
%% 	% faster creation of the map ...
%% 	Pieces = core_boardmap:createPiecesMap(Map, From, To, Piece),
%% 	Result = #node{
%% 		  white = White,
%% 		  black = Black,
%% 		  pieces = Pieces,
%% 		  toMove = ToMove,
%% 		  blackAdvantage = 
%% 			  (5*eval3(Black, Pieces, MovedPawn) + IsBlackCastled)
%% 		 - (5*eval3(White, Pieces, MovedPawn) + IsWhiteCastled),
%% 		  movedPawn = MovedPawn,
%% 		  isWhiteCastled = IsWhiteCastled,
%% 		  isBlackCastled = IsBlackCastled
%% 		 },
%% 	Result.



%%
%% Local Functions
%%


-spec eval3([#piece{}], #boardMap{}, xpiece()) -> 
               smallint().

eval3([], _BoardMap, _MovedPawn) ->
	0;

%% eval3([#piece{value=V, colour=C}=P|Tail], BoardMap, MovedPawn) ->
%% 	Squares = core_material:getSquares(P, C, BoardMap, MovedPawn),
%% 	V + coverageValueHelper(Squares, 0) + eval3(Tail, BoardMap, MovedPawn).


eval3([#piece{value=V, colour=C}=P|Tail], BoardMap, MovedPawn) ->
	
	PosValue = core_material:getPosValue(P, C, BoardMap, MovedPawn),
	
	V + PosValue + eval3(Tail, BoardMap, MovedPawn).



%% @doc The list-folding version does not cause any speed gain
%% compared to the plain recursive version.
%%
%% The tail recursive version is slightly better, so go for it.

%%-type smallint()  :: 0..100000000.
-spec coverageValueHelper([#square{}], smallint()) -> smallint().

%% coverageValueHelper(SS) ->
%% 	lists:foldl(fun(#square{x=X}, B) -> B + 25 + (if X < 5 -> (X-1)*5; true -> (8-X)*5 end) end, 0, SS).

% fun (U, E) ->  (E + 25 + ()) end


%% coverageValueHelper([]) ->
%% 	0;
%% 
%% coverageValueHelper([#square{x=X}|Tail]) ->
%% 	if
%% 		X < 5 ->
%% 			25 + (X-1)*5 + coverageValueHelper(Tail);
%% 		true ->
%% 			25 + (8-X)*5 + 	coverageValueHelper(Tail)
%% 	end.


coverageValueHelper([], R) ->
	R;

coverageValueHelper([#square{x=X}|Tail], R) when X < 5 ->
	coverageValueHelper(Tail, R + 4 + X);
	
coverageValueHelper([#square{x=X}|Tail], R) ->
	coverageValueHelper(Tail, R + 13 - X).

%% coverageValueHelper([#square{fileValue=FV}|Tail], R) ->
%% 	coverageValueHelper(Tail, R + FV).

	

-spec faultyValue([#piece{}]) -> smallint().

faultyValue([]) ->
	0;

faultyValue([#piece{square=Square}|_]) ->
	#square{x=X, y=Y} = Square,
	if
		X >= 4, X =< 5, Y >= 4, Y =< 5 ->
			30;
		true ->
			0
	end.


-spec verifyLegal(sid(), #node{}, #node{}) -> ok | none().

verifyLegal(Sid, FromNode, ToNode) ->
	ToNodes = expand(Sid, FromNode),
	verifyLegalHelper(ToNode, ToNodes).


-spec verifyLegalHelper(#node{}, [#node{}]) -> ok | none().

verifyLegalHelper(_, []) ->
	throw("illegal move");

verifyLegalHelper(ToNode, [Node|Tail]) ->
	case compareTo(ToNode, Node) of
		0 ->
			ok;
		_ ->
			verifyLegalHelper(ToNode, Tail)
	end.
	




%% @doc Returns an ordered list of nodes.
%% TODO: Castling
%% TODO: Promotion

-spec expand(sid(), #node{}) -> [#node{}].

expand(Sid, #node{toMove=ToMove}=Node) ->
	%if   TODO ... this cannot possibly be important to open-code?
	%	ToMove =:= white ->
	%		Own = White;
	%	true ->
	%		Own = Black
	%end,
	Own = getMaterial(Node, ToMove),
	S = expandOuter(Sid, Own, [], Node),
	
	Opp = getMaterial(Node, core_colour:otherColour(ToMove)),
	
	% TODO, clean up
	case castle(Sid, Node, Own, Opp, true) of
		null ->
			T = S;
		N1 ->
			T = insert(N1, S)
	end,
	
	case castle(Sid, Node, Own, Opp, false) of
		null ->
			T;
		N2 ->
			insert(N2, T)
	end.
	


%% @doc Returns true if the path has a problem (blockage or threat), false otherwise.
%% Consider renaming to reflect semantics. TODO: Consider optimizing.

%-spec checkPath([smallint()], ignored, #boardMap{}, #node{}, [#node{}], boolean()) -> boolean().



-spec checkPath([smallint()], #boardMap{}, #node{}, [#piece{}], boolean(), tuple()) -> boolean().

checkPath([], _, _, _, _, _) ->
	false;

checkPath([Index|Tail], BoardMap, Node, Opp, LeadingItem, Tuple) ->
	%Square = array:get(Index, Array),
    Square = element(Index, Tuple),
	case bmget(Square, BoardMap) of
		null ->
			% the path square is not occupied
			   case core_square:isThreated(Square, Node, Opp) of
				   true ->
					   case Square#square.x of
                                             % a threat of file 2 does not matter
                                             2 ->
						   checkPath(Tail, BoardMap, Node, Opp, false, Tuple);
					                                        _ ->
                                                                                      true
					   end;
				   _ ->
					   checkPath(Tail, BoardMap, Node, Opp, false, Tuple)
			   end;
		   _ ->
			   % square is occupied, special case when looking at the king's square
			   if
				   LeadingItem ->
					   case core_square:isThreated(Square, Node, Opp) of
						   true ->
							   true;
						   _ ->
							   checkPath(Tail, BoardMap, Node, Opp, false, Tuple)
					   end;
				   true ->
					   true
			   end
	   end.


-spec castle(sid(), #node{}, [#piece{}], [#piece{}], boolean()) -> #node{} | null.


castle(_Sid,   % TODO, does not use Sid
	   #node{toMove=ToMove, pieces=BoardMap, isWhiteCastled=IWC, isBlackCastled=IBC}=N, Own, Opp, Long) ->
	#piece{pristine=Pristine, square=#square{castleLongCorner=CLC, castleShortCorner=CSC, castleLongPath=CLP, castleShortPath=CSP}=KingSquare} = core_material:getKing(Own),
	case Pristine of
		false ->
			null;
		true ->
			#board{tuple=Tuple} = get(board),
			%CornerSquare = case Long of true -> array:get(CLC, Array); _ -> array:get(CSC, Array) end,
			CornerSquare = case Long of true -> element(CLC, Tuple); _ -> element(CSC, Tuple)
                  end,
			case bmget(CornerSquare, BoardMap) of
				null ->
					null;
				#piece{type=rook, pristine=true} ->
					Path = case Long of true -> CLP; _ -> CSP end,
					case checkPath(Path, BoardMap, N, Opp, true, Tuple) of
						true ->
							null;
						_ ->
							% all set
							OldKingSquare = KingSquare,
							NewKingSquare = pathGet(2, Path, ignored, Tuple),
							NewRookSquare = pathGet(1, Path, ignored, Tuple),
					
							
							A = insertPiece(king, ToMove, NewKingSquare, Own),
							B = remove(OldKingSquare, A),
							C = insertPiece(rook, ToMove, NewRookSquare, B),
							D = remove(CornerSquare, C),
							case ToMove of
								white ->
									core_node:create(D, Opp, black, null, ?CASTLING_BONUS, IBC);
								black ->
									core_node:create(Opp, D, white, null, IWC, ?CASTLING_BONUS)
							end
					end;
				_ ->
					null
			end
	end.
			
			
	
pathGet(0, [Car|_], _Array, Tuple) ->
    element(Car, Tuple);

pathGet(_, [], _, _) ->
	core_util:inconsistencyException("getPath list too short");

pathGet(K, [_|Cdr], _Array, Tuple) ->
	pathGet(K-1, Cdr, ignored, Tuple).
	



-spec expandOuter(sid(), [#piece{}], [#node{}], #node{}) ->  [#node{}].

expandOuter(_Sid, [], E, _Node) ->
	E;

expandOuter(Sid, [#piece{square=Square}=P|Tail], E, #node{toMove=ToMove, pieces=BoardMap, movedPawn=MovedPawn}=Node) ->
	Squares = core_material:getSquares(P, ToMove, BoardMap, MovedPawn),
	F = expandInner(Sid, Squares, E, Node, Square, P),
	expandOuter(Sid, Tail, F, Node).



-spec expandInner(sid(), [#square{}], [#node{}], #node{}, #square{}, #piece{}) -> 
                     [#node{}].

expandInner(_Sid, [], E, _Node, _FromSquare, _P) ->
	E;

expandInner(Sid, [#square{y=Y}=Place|Tail], E, #node{toMove=ToMove}=Node, FromSquare, #piece{type=Type}=P) ->
	if
		ToMove =:= white, Y =:= 8, Type=:=whitePawn ->
			F = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, queen), E),
			G = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, rook), F),
			H = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, bishop), G),
			J = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, knight), H);
		ToMove =:= black, Y =:= 1, Type=:=blackPawn ->
			F = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, queen), E),
			G = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, rook), F),
			H = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, bishop), G),
			J = addWhenLegal(makeMove(Sid, Node, FromSquare, Place, knight), H);
		true ->
			J = addWhenLegal(makeMove(Sid, Node, FromSquare, Place), E)
	end,
	expandInner(Sid, Tail, J, Node, FromSquare, P).




%% @doc Returns a sorted list of nodes obtained by inserting
%% the given node into the given list of nodes.
%%
%% TODO: Swap the arguments

-spec addWhenLegal(#node{}, [#node{}]) -> [#node{}].

addWhenLegal(Node, List) ->
	case isCheckedAfterMove(Node) of
		true ->
			List;
		_->
			insert(Node, List)
	end.




-spec isCheckedAfterMove(#node{}) -> boolean().

isCheckedAfterMove(#node{white=White, black=Black, toMove=ToMove}=N) ->
	case ToMove of
		white ->
			#piece{square=BlackKingSquare} = core_material:getKing(Black),
			core_square:isThreated(BlackKingSquare, N, White);
		_ ->
			#piece{square=WhiteKingSquare} = core_material:getKing(White),
			core_square:isThreated(WhiteKingSquare, N, Black)
	end.



%% -spec getJustMovedPiece([#piece{}], #square{}) -> #piece{}.
%% 
%% getJustMovedPiece([#piece{square=To}=P|_], To) ->
%% 	P;
%% 
%% getJustMovedPiece([_|Tail], To) ->
%% 	getJustMovedPiece(Tail, To).



-spec makeMove(sid(), #node{}, #square{}, #square{}) -> #node{}.

makeMove(Sid, #node{toMove=white, white=White, black=Black, isWhiteCastled=WC, isBlackCastled=BC}=N, From, To) ->
	Mown = newOwnMaterial(N, White, From, To),
	Mopp = newOppMaterial(Sid, N, Black, From, To),
	JustMovedWhitePawn = getJustMovedWhitePawn(From, To, Mown),
	
%% 	if
%% 		Mopp =:= Black ->
%% 			% optimized map creation
%% 			MP = getJustMovedPiece(Mown, To),
%% 			%%io:format("o",[]),
%% 			create(Mown, Mopp, black, JustMovedWhitePawn, WC, BC, {N, From, To, MP});
%% 		true ->
%% 			%%io:format(".",[]),
%% 			create(Mown, Mopp, black, JustMovedWhitePawn, WC, BC)
%% 	end;

	create(Mown, Mopp, black, JustMovedWhitePawn, WC, BC);

makeMove(Sid, #node{white=White, black=Black, isWhiteCastled=WC, isBlackCastled=BC}=N, From, To) ->
	Mown = newOwnMaterial(N, Black, From, To),
	Mopp = newOppMaterial(Sid, N, White, From, To),
	JustMovedBlackPawn = getJustMovedBlackPawn(From, To, Mown),
	
%% 	if
%% 		Mopp =:= White ->
%% 			% optimized map creation
%% 			MP = getJustMovedPiece(Mown, To),
%% 			%%io:format("o",[]),
%% 			create(Mopp, Mown, white, JustMovedBlackPawn, WC, BC, {N, From, To, MP});
%% 		true ->
%% 			%%io:format(".",[]),
%% 			create(Mopp, Mown, white, JustMovedBlackPawn, WC, BC)
%% 	end.

	create(Mopp, Mown, white, JustMovedBlackPawn, WC, BC).


%% @doc Promotion context. The 5th argument specifies one of
%% queen, rook, bishop or knight.

-spec makeMove(sid(), #node{}, #square{}, #square{}, atom()) -> #node{}.

makeMove(Sid, #node{toMove=white, white=White, black=Black, isWhiteCastled=WC, isBlackCastled=BC}=N, From, To, Type) ->
	Mown = newOwnMaterial(white, White, From, Type, To),
	Mopp = newOppMaterial(Sid, N, Black, From, To),
	create(Mown, Mopp, black, null, WC, BC);

makeMove(Sid, #node{white=White, black=Black, isWhiteCastled=WC, isBlackCastled=BC}=N, From, To, Type) ->
	Mown = newOwnMaterial(black, Black, From, Type, To),
	Mopp = newOppMaterial(Sid, N, White, From, To),
	create(Mopp, Mown, white, null, WC, BC).



-spec newOwnMaterial(#node{}, [#piece{}], #square{}, #square{}) -> [#piece{}].

newOwnMaterial(#node{pieces=BoardMap}, Material, From, To) ->
	P = bmget(From, BoardMap),
	movePiece(Material, P, To).
	


%% @doc The given Piece at square From is in Material and shall be
%% moved to the To square. This is not a case of promotion of course.
%% This function is optimal in the sense that as few list cells as
%% possible are created.

-spec movePiece([#piece{}], #piece{}, #square{}) -> [#piece{}].

movePiece(Material, Piece, To) ->
	#piece{order=U, square=#square{tupleIndex=J}}=MovedPiece = Piece#piece{square=To, pristine=false},
	movePiece(Material, Piece, MovedPiece, U+J).



-spec movePiece([#piece{}], #piece{}, #piece{}, smallint()) -> [#piece{}].

movePiece([Piece|Tail], Piece, MovedPiece, MM) ->
	insPiece(Tail, MovedPiece, MM);

movePiece([#piece{order=U, square=#square{tupleIndex=J}}=Head|Tail]=Pieces, Piece, MovedPiece, MM) ->
	if
		MM < U+J ->
			[MovedPiece|remPiece(Pieces, Piece)];
		true ->
			[Head|movePiece(Tail, Piece, MovedPiece, MM)]
	end.



-spec insPiece([#piece{}], #piece{}, smallint()) -> [#piece{}].

insPiece([], Piece, _) ->
	[Piece];

insPiece([#piece{order=U, square=#square{tupleIndex=J}}=Head|Tail]=Pieces, Piece, M) ->
	if 
		M < U+J ->
			[Piece|Pieces];
		true ->
			[Head|insPiece(Tail, Piece, M)]
	end.


-spec remPiece([#piece{}], #piece{}) -> [#piece{}].

%% remPiece([], _) ->
%% 	core_util:inconsistencyException("TODO, remove this case altogether");

remPiece([Piece|Tail], Piece) ->
	Tail;

remPiece([Head|Tail], Piece) ->
	[Head|remPiece(Tail, Piece)].








%% @doc In promotion context. The Type argument specifies one of
%% queen, rook, bishop or knight. Note that the piece being
%% removed is of a different type than the one being inserted.

-spec newOwnMaterial(colour(), [#piece{}], #square{}, atom(), #square{}) -> [#piece{}].

newOwnMaterial(ToMove, Own, From, Type, To) ->
	remove(From, insertPiece(Type, ToMove, To, Own)).


-spec newOppMaterial(sid(), #node{}, [#piece{}], #square{}, #square{}) -> [#piece{}].


%% TODO, does not need Sid
newOppMaterial(_Sid,
			   #node{toMove=ToMove, pieces=BoardMap}, 
			   Opp, 
			   #square{enPassantKiller=EPK, x=FromX}=From, 
			   #square{x=ToX, epKillSquare=EPKS}=To) ->
	case bmget(To, BoardMap) of
		#piece{colour=Colour} when Colour =/= ToMove ->   %% regular capture
			remove(To, Opp);
		null when EPK =:= black ->                        %% black makes EP capture
			case bmget(From, BoardMap) of
				#piece{type=blackPawn} ->
					if
						FromX =/= ToX ->
							#board{tuple=Tuple} = get(board),
							%S = array:get(EPKS, Array),
							S = element(EPKS, Tuple),
							remove(S, Opp);
						true ->
							Opp
					end;
				_ ->
					Opp
			end;
		null when EPK =:= white ->                        %% white makes EP capture
			case bmget(From, BoardMap) of
				#piece{type=whitePawn} ->
					if
						FromX =/= ToX ->
							#board{tuple=Tuple} = get(board),
							%S = array:get(EPKS, Array),
							S = element(EPKS, Tuple),
							remove(S, Opp);
						true ->
							Opp
					end;
				_ ->
					Opp
			end;
		_->
			Opp			
	end.
		


%% @doc Remove the piece at the given square from
%% the given material. It is trusted that removal
%% succeeds.

-spec remove(#square{}, [#piece{}]) -> [#piece{}].

remove(Square, [#piece{square=Square}|Tail]) ->
	Tail;

remove(Square, [Piece|Tail]) ->
	[Piece|remove(Square, Tail)].



%% @doc Inserts the indicated piece (it is created
%% non-pristine) into the given material. Note that
%% since the introduction of movePiece/3 this function
%% is no longer time-critical.
%%
%% TODO: Performance: pieceType lookup is done
%% at every recursion.
%%
%% TODO: With the given representation it is just
%% silly to defer piece creation. It can be created
%% outside this function and inserted afterwards.
%% Unclear if this affects performance a lot.
%%
%% TODO: Consider using a function similar to
%% insertPiece/2.

-spec insertPiece(atom(), colour(), #square{}, [#piece{}]) -> [#piece{}].

insertPiece(Type, Colour, Square, []) ->
	[core_material:create(Type, Colour, Square)];

insertPiece(Type, Colour, #square{tupleIndex=IN}=Square, [#piece{order=MO, square=#square{tupleIndex=MN}}=H|Tail]=M) ->
	#pieceType{order=IO} = core_piecetype:pieceType(Type),
	InserteeOrder = IO + IN,
	HeadOrder = MO + MN,
	if
		InserteeOrder < HeadOrder ->
			[core_material:create(Type, Colour, Square)|M];
		true ->
			[H|insertPiece(Type, Colour, Square, Tail)]
	end.
  

-spec insertPiece(#piece{}, [#piece{}]) -> [#piece{}].

insertPiece(Material, []) ->
	[Material];

insertPiece(#piece{type=Type, square=#square{tupleIndex=IN}}=Material, 
			[#piece{order=MO, square=#square{tupleIndex=MN}}=H|Tail]=M) ->
	#pieceType{order=IO} = core_piecetype:pieceType(Type),
	InserteeOrder = IO + IN,
	HeadOrder = MO + MN,
	if
		InserteeOrder < HeadOrder ->
			[Material|M];
		true ->
			[H|insertPiece(Material, Tail)]
	end.



-spec getJustMovedWhitePawn(#square{}, #square{}, [#piece{}]) -> xpiece().

getJustMovedWhitePawn(_From, _To, []) ->
	null;

getJustMovedWhitePawn(#square{y=YF}=From, #square{y=YT}=To, [#piece{type=Type, square=S}=P|Tail]) ->
	if 
		S =:= To, Type =:= whitePawn, YT =:= 4, YF =:= 2 ->
			P;
		true ->
			getJustMovedWhitePawn(From, To, Tail)
	end.


-spec getJustMovedBlackPawn(#square{}, #square{}, [#piece{}]) -> xpiece().

getJustMovedBlackPawn(_From, _To, []) ->
	null;

getJustMovedBlackPawn(#square{y=YF}=From, #square{y=YT}=To, [#piece{type=Type, square=S}=P|Tail]) ->
	if 
		S =:= To, Type =:= blackPawn, YT =:= 5, YF =:= 7 ->
			P;
		true ->
			getJustMovedBlackPawn(From, To, Tail)
	end.




%% @doc insertNode(Node, List), for use when collecting nodes.

-spec insert(#node{}, [#node{}]) -> [#node{}].

insert(Node, []) ->
	[Node];

insert(P, [Q | Tail]=M) ->
	D = compareTo(P, Q),
	if
		D < 0 ->
			[P | M];
		true ->
			[Q | insert(P, Tail)]
	end.


-spec compareTo(#node{}, #node{}) -> smallint().

compareTo(#node{white=WMM, blackAdvantage=AM, black=BMM, toMove=ToMove},
		  #node{white=WMN, blackAdvantage=AN, black=BMN}) ->
	case ToMove of
		black ->
			Score = AM - AN;
		white ->
			Score = AN - AM
	end,
	if
		Score =/= 0 ->
			Score;
		true ->
			U = compareWhite(WMM, WMN),
			if 
				U =/= 0 ->
					U;
				true ->
					compareBlack(BMM, BMN)
			end
	end.
			

-spec compareWhite([#piece{}], [#piece{}]) -> smallint().

compareWhite([], []) ->
	0;

compareWhite([], _) ->
	1;

compareWhite(_, []) ->
	-1;

compareWhite([#piece{type=TM, square=#square{tupleIndex=NM}, order=OM}|TailM],
			 [#piece{type=TN, square=#square{tupleIndex=NN}, order=ON}|TailN]) ->
	if
		TM =/= TN ->
			ON - OM;
		NM =/= NN ->
			NM - NN;
		true ->
			compareWhite(TailM, TailN)
	end.


-spec compareBlack([#piece{}], [#piece{}]) -> smallint().

compareBlack([], []) ->
	0;

compareBlack([], _) ->
	-1;

compareBlack(_, []) ->
	1;

compareBlack([#piece{type=TM, square=#square{tupleIndex=NM}, order=OM}|TailM],
			 [#piece{type=TN, square=#square{tupleIndex=NN}, order=ON}|TailN]) ->
	if
		TM =/= TN ->
			ON - OM;
		NM =/= NN ->
			NM - NN;
		true ->
			compareBlack(TailM, TailN)
	end.


-spec expandAndSelect(sid(), #node{}, smallint()) -> #node{}.

expandAndSelect(Sid, #node{toMove=ToMove}=Node, KingDestFile) ->
	Rank = case ToMove of white -> 1; _ -> 8 end,
	From = core_board:getSquare(Sid, 5, Rank),
	To = core_board:getSquare(Sid, KingDestFile, Rank),
	 
	NewNodes = expand(Sid, Node),
	 
	Result = expandAndSelectHelper(NewNodes, [], Node, From, To),
	 
	case Result of
		[_, _ | _] ->
			core_util:inconsistencyException("expand/1 not consistent, cannot happen");
		[] ->
			core_util:userException("castling not possible");
		[NewNode] ->
			NewNode
	end.
		 


expandAndSelectHelper([], E, _Node, _From, _To) ->
    E;

expandAndSelectHelper([#node{}=NewNode|Tail], E, #node{toMove=ToMove}=Node, From, To) ->
	case isOccupied(Node, From, ToMove, king)
			 andalso isFree(NewNode, From)
			 andalso isFree(Node, To)
			 andalso isOccupied(NewNode, To, ToMove, king) of
		true ->
			expandAndSelectHelper(Tail, [NewNode|E], Node, From, To);
		_ ->
			expandAndSelectHelper(Tail, E, Node, From, To)
	end.


-spec expandAndSelect(sid(), #node{}, #square{}, #square{}, atom()) -> #node{}.

expandAndSelect(Sid, Node, From, To, Prom) ->
	
	NewNodes = expand(Sid, Node),
	 
	Result = expandAndSelectHelper(NewNodes, [], Node, From, To, Prom),

	case Result of
		[_, _ | _] ->
			core_util:inconsistencyException("expand/1 not consistent, cannot happen");
		[] ->
			% core_util:throw("illegal move", []);
			core_util:userException("illegal move");
		[NewNode] ->
			NewNode
	end.



-spec expandAndSelectHelper([#node{}], [#node{}], #node{}, #square{}, #square{}, atom()) -> [#node{}].

expandAndSelectHelper([], E, _, _, _, _) ->
	E;

expandAndSelectHelper([#node{pieces=M2}=NewNode|Tail], E, #node{toMove=ToMove, pieces=BoardMap}=Node, From, To, Prom) ->
	ToNotMove = core_colour:otherColour(ToMove),
	case isOccupied(Node, From, ToMove)
		andalso isFree(NewNode, From)
		andalso (isFree(Node, To) orelse isOccupied(Node, To, ToNotMove)) of
		true ->
			#piece{type=Type} = bmget(From, BoardMap),
			case isOccupied(NewNode, To, ToMove, Type) of
				true ->
					case isRegularMove(Node, NewNode) of
						true ->
							expandAndSelectHelper(Tail, [NewNode|E], Node, From, To, Prom);
						_ ->
							expandAndSelectHelper(Tail, E, Node, From, To, Prom)
					end;
				_ ->
					case isOccupied(NewNode, To, ToMove) 
						andalso (Type =:= whitePawn orelse Type =:= blackPawn) of
						true ->
							case bmget(To, M2) of
								#piece{type=Prom} ->
									expandAndSelectHelper(Tail, [NewNode|E], Node, From, To, Prom);
								_ ->
									expandAndSelectHelper(Tail, E, Node, From, To, Prom)
							end;
						_ ->
							expandAndSelectHelper(Tail, E, Node, From, To, Prom)
					end
			end;
		_ ->
			expandAndSelectHelper(Tail, E, Node, From, To, Prom)
	end.
						
			

-spec isFree(#node{}, #square{}) -> boolean().

isFree(#node{pieces=BoardMap}, Square) ->
	case bmget(Square, BoardMap) of
		null ->
			true;
		_ ->
			false
	end.


-spec isOccupied(#node{}, #square{}, colour()) -> boolean().

isOccupied(#node{pieces=BoardMap}, Square, Colour) ->
	case bmget(Square, BoardMap) of
		null ->
			false;
		#piece{colour=Colour} ->
			true;
		_ ->
			false
	end.

-spec isOccupied(#node{}, #square{}, colour(), atom()) -> boolean().

isOccupied(#node{pieces=BoardMap}, Square, Colour, Type) ->
	case bmget(Square, BoardMap) of
		null ->
			false;
		#piece{colour=Colour, type=Type} ->
			true;
		_ ->
			false
	end.


-spec isRegularMove(#node{}, #node{}) -> boolean().

isRegularMove(#node{toMove=ToMove}=Node, NewNode) ->
	Material = getMaterial(Node, ToMove),
	Count = isRegularMoveHelper(Material, 0, NewNode),
	Count =< 1.


-spec isRegularMoveHelper([#piece{}], smallint(), #node{}) -> smallint().

isRegularMoveHelper([], R, _NewNode) ->
	R;

isRegularMoveHelper([#piece{square=Square}|Tail], R, NewNode) ->
	case isFree(NewNode, Square) of
		true ->
			isRegularMoveHelper(Tail, R+1, NewNode);
		_ ->
			isRegularMoveHelper(Tail, R, NewNode)
	end.
	


-spec getMaterial(#node{}, colour()) -> [#piece{}].

getMaterial(#node{white=White, black=_Black}, white) ->
	White;

getMaterial(#node{white=_White, black=Black}, black) ->
	Black.



-spec key(#node{}) ->  string().

key(Node) ->
	toString(Node).



-spec toString(#node{}) -> string().

toString(#node{white=White, black=Black, toMove=ToMove, movedPawn=MP}) ->
	H = 
		if
			ToMove =:= white ->
				"w|";
			true ->
				"b|"
		end,
	X = H ++ core_material:toString(White) ++ "|" ++ core_material:toString(Black),
	case MP of
		null ->
			X;
		_ ->
			#piece{square=S} = MP,
			X ++ "|" ++ core_square:toString(S)
	end.


%% Inlining causes SLOWDOWN here!!!
%% -compile({inline,[bmget/2]}).

bmget(#square{tupleIndex=TupleIndex}, #boardMap{tuple=Tuple}) ->
	element(TupleIndex, Tuple).


-spec alphaBetaRoot(sid(), #node{}, smallint()) ->  #abResult{}.

alphaBetaRoot(Sid, #node{toMove=ToMove}=CurrentNode, RecursionDepth) ->

	Opponent = core_colour:otherColour(ToMove),

	%% expect an ordered set of nodes
	Nodes = expand(Sid, CurrentNode),
	
	case Nodes of
		[] ->
			case isChecked(CurrentNode) of
				true ->
					core_abresult:create([], atom_to_list(ToMove) ++ " is being checkmated");
				_ ->
					core_abresult:create([], "stalemate")
			end;
		_ ->
			%% we have nodes ... look for immediate checkmate
            case isCheckMate(Sid, Nodes) of
				{true, CMNode} ->
					core_abresult:createSingle(CMNode, atom_to_list(Opponent) ++ " can be checkmated");
				_ ->
					case Nodes of
						[JustOneNode] ->
							core_abresult:createSingle(JustOneNode, atom_to_list(ToMove) ++ " move is forced");
						_ ->
							#abResult{nodeSets=NN} = alphaBetaRootHelper(Sid, Nodes, RecursionDepth),
							core_abresult:create(NN, null)
					end
			end
	end.
							



-spec isCheckMate(sid(), [#node{}]) -> {boolean(), xnode()}.

isCheckMate(_Sid, []) ->
	{false, null};

isCheckMate(Sid, [Child|Tail]) ->
	%% child is checked, and grandchildren empty
    case isChecked(Child) of
		true ->
			case expand(Sid, Child) of
				[] -> 
					{true, Child};
				_ ->
					isCheckMate(Sid, Tail)
			end;
	    _ ->
            isCheckMate(Sid, Tail)
    end.


-spec alphaBetaRootHelper(sid(), [#node{}], smallint()) -> #abResult{}.

alphaBetaRootHelper(Sid, Children, Depth) ->
	MaxThreads = param_parameter:getNumberOfThreads(),
	h(Sid, Children, core_abresult:create([], ""), Depth - 1, 0, MaxThreads, 0, length(Children)).


-spec h(sid(), [#node{}], #abResult{}, smallint(), smallint(), smallint(), smallint(), smallint()) -> #abResult{}.
		  
h(_Sid, [], R, _, PL, _, ResultsDelivered, ResultsExpected) when ResultsDelivered =:= ResultsExpected ->
	if
		PL =/= 0 ->
			core_util:inconsistencyException("inconsistent process count: ~w", [PL]);
		true ->
			ok
	end,
	R;

h(Sid, [], R, D, PL, PMax, RD, RE) ->
	receive
		{ok, Node, ABValue} ->
			h(Sid, [], core_abresult:add(Node, ABValue, R), D, PL - 1, PMax, RD + 1, RE)
	end;
			

%% h([Child|Tail], R, D, PL, PM, RD, RE) when PL < PM ->
%% 	spawn(?MODULE, abw, [self(), Child, D, core_board:instance(Sid)]),
%% 	h([Tail], R, D, PL + 1, PM, RD, RE);
%% 
%% h(Children, R, D, PL, PM, RD, RE) ->
%% 	receive
%% 		{ok, Node, ABValue} ->
%% 			h(Children, abresult:add(Node, ABValue, R), D, PL-1, PM, RD+1, RE)
%% 	end.


h(Sid, [Child|Tail], R, D, PL, PM, RD, RE) when PL < PM ->
	spawn(core_abrunner, run, [Sid, self(), Child, D, get(board)]),
	h(Sid, Tail, R, D, PL + 1, PM, RD, RE);

h(Sid, Children, R, D, PL, PM, RD, RE) ->
	receive
		{ok, Node, ABValue} ->
			h(Sid, Children, core_abresult:add(Node, ABValue, R), D, PL - 1, PM, RD + 1, RE)
	end.





%% @doc Returns true if the player that is to move is also checked.
%% This function is TIME CRITICAL.

-spec isChecked(#node{}) -> boolean().

isChecked(#node{toMove=white, white=Own, black=Opp}=N) ->
	#piece{square=Square} = core_material:getKing(Own),
	core_square:isThreated(Square, N, Opp);

isChecked(#node{toMove=black, white=Opp, black=Own}=N) ->
	#piece{square=Square} = core_material:getKing(Own),
	core_square:isThreated(Square, N, Opp).



%% @doc Not time critical.

-spec isCheckmated(sid(), #node{}) -> boolean().

isCheckmated(Sid, Node) ->
	case isChecked(Node) of
		false ->
			false;
		true ->
			case expand(Sid, Node) of
				[] ->
					true;
				_ ->
					false
			end
	end.


%% @doc Not time critical.

-spec isStalemate(sid(), #node{}) -> boolean().

isStalemate(Sid, Node) ->
	case isChecked(Node) of
		false ->
			case expand(Sid, Node) of
				[] ->
					true;
				_ ->
					false
			end;
		true ->
			false
	end.






-spec abWhite(sid(), #node{}, smallint(), smallint(), smallint()) -> smallint().

abWhite(_Sid, #node{blackAdvantage=A}, 0, _, _) ->
	A;

abWhite(Sid, Node, Depth, A, B) ->
	Nodes = expand(Sid, Node),
	
	case Nodes of
		[] ->
			case isChecked(Node) of
				true ->
					?SMALLINT_MAX_VALUE;
				_ ->
					abWhiteHelper(Sid, Nodes, A, B, B, Depth)
			end;
		_ ->
			abWhiteHelper(Sid, Nodes, A, B, B, Depth)
	end.


-spec abWhiteHelper(sid(), [#node{}], smallint(), smallint(), smallint(), smallint()) -> smallint().

abWhiteHelper(_Sid, [], _, _, ResBeta, _) ->
	ResBeta;

abWhiteHelper(Sid, [Child|Tail], A, B, RB, D) ->
	ABBlack = abBlack(Sid, Child, A, RB, D - 1),
	NewResBeta = mathmin(RB, ABBlack),
	if
		NewResBeta =< A ->
			NewResBeta;
		true ->
			abWhiteHelper(Sid, Tail, A, B, NewResBeta, D)
	end.



-spec abBlack(sid(), #node{}, smallint(), smallint(), smallint()) -> smallint().

abBlack(_Sid, #node{blackAdvantage=A}, _, _, 0) -> 
	A;

abBlack(Sid, Node, A, B, Depth) ->
	Nodes = expand(Sid, Node),
	
	%% nodeTrace(Node, Nodes, Depth),
	
	case Nodes of
		[] ->
            %%% TODO: Bug here .... inherited?
			case isChecked(Node) of
				true ->
					?SMALLINT_MIN_VALUE;
				_ ->
					?SMALLINT_MIN_VALUE
			end;
		_ ->
			alphaBetaBlackHelper(Sid, Nodes, A, B, A, Depth)
	end.



-spec alphaBetaBlackHelper(sid(), [#node{}], smallint(), smallint(), smallint(), smallint()) -> smallint().

alphaBetaBlackHelper(_Sid, [], _, _, ResAlpha, _) ->
	ResAlpha;

alphaBetaBlackHelper(Sid, [Child|Tail], A, B, ResAlpha, D) ->
	ABWhite = abWhite(Sid, Child, D - 1, ResAlpha, B),
	NewResAlpha = mathmax(ResAlpha, ABWhite),
	if
		NewResAlpha >= B ->
			NewResAlpha;
		true ->
			alphaBetaBlackHelper(Sid, Tail, A, B, NewResAlpha, D)
	end.



%% @doc TODO, consider inlining (hardly important)

-spec mathmin(smallint(), smallint()) -> smallint().

mathmin(X, Y) ->
	if 
		X < Y ->
		   X;
	   true ->
		   Y
	end.


-spec mathmax(smallint(), smallint()) -> smallint().

mathmax(X, Y) ->
	if 
		X > Y ->
		   X;
	   true ->
		   Y
	end.



	
	
	
getState(_Node) ->
	init.


