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
%% Created: Jan 17, 2012
%% Description: TODO: Add description to piece_type
-module(core_piecetype).





%%
%% Include files
%%

-include("antonius.hrl").
-include("piecetype.hrl").



%%
%% Exported Functions
%%


-export([pieceType/1]).
-export([pieceType/2]).

%%
%% API Functions
%%



%% @doc Constructor.

-spec pieceType(atomOrString()) -> #pieceType{}.

pieceType(king) ->
	#pieceType{abbreviation="K", name=king, rname="king", order=1000, value=199999};

pieceType(queen) ->
	#pieceType{abbreviation="Q", name=queen, rname="queen", order=2000, value=500};

pieceType(rook) ->
	#pieceType{abbreviation="R", name=rook, rname="rook", order=3000, value=200};

pieceType(bishop) ->
	#pieceType{abbreviation="B", name=bishop, rname="bishop", order=4000, value=120};

pieceType(knight) ->
	#pieceType{abbreviation="N", name=knight, rname="knight", order=5000, value=100};

pieceType(whitePawn) ->
	#pieceType{abbreviation="p", name=whitePawn, rname="pawn", order=6000, value=60};

pieceType(blackPawn) ->
	#pieceType{abbreviation="p", name=blackPawn, rname="pawn", order=6000, value=60};

pieceType("K") -> pieceType(king);
pieceType("Q") -> pieceType(queen);
pieceType("R") -> pieceType(rook);
pieceType("B") -> pieceType(bishop);
pieceType("N") -> pieceType(knight);

pieceType(X) -> core_util:inconsistencyException("unknown piece type: ~p", [X]).


-spec pieceType(atomOrString(), atom()) -> #pieceType{}.

pieceType(pawn, white) -> pieceType(whitePawn);
pieceType("p", white) -> pieceType(whitePawn);
pieceType("P", white) -> pieceType(whitePawn);

pieceType(pawn, black) -> pieceType(blackPawn);
pieceType("p", black) -> pieceType(blackPawn);
pieceType("P", black) -> pieceType(blackPawn);

pieceType("n", _) -> pieceType(knight);
pieceType("N", _) -> pieceType(knight);
pieceType("b", _) -> pieceType(bishop);
pieceType("B", _) -> pieceType(bishop);
pieceType("r", _) -> pieceType(rook);
pieceType("R", _) -> pieceType(rook);
pieceType("q", _) -> pieceType(queen);
pieceType("Q", _) -> pieceType(queen);
pieceType("k", _) -> pieceType(king);
pieceType("K", _) -> pieceType(king);

pieceType(X, _) -> core_util:inconsistencyException("unknown piece type: ~p", [X]).

%%
%% Local Functions
%%

