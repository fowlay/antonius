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


-define(HRL_SQUARE, true).

-ifndef(HRL_COLOUR).
-include("colour.hrl").
-endif.

-type filenum() :: 1..8.
-type ranknum() :: 1..8.
-type sqnum()   :: 1..64.
-type xsqnum()  :: 0..64.

%% NOTE: The order of fields is frozen up to tupleIndex (the exact order
%% is relied upon by NIFs).

-record(square, {
				% fileValue                 :: smallint(),
				 x                         :: filenum(),
				 y                         :: ranknum(),
				 colour                    :: colour(),
				 file                      :: string(),
				 rank                      :: string(),
				 name                      :: string(),
				
				 tupleIndex                :: integer(),
				 enPassantKiller           :: xcolour(), %% for Y = 4: black, for Y = 5: white, else: null
				 whitePawnMoves            :: [sqnum()],
				 whitePawnKills            :: [sqnum()],
				 whitePawnEpSquares        :: [sqnum()], %% for Y = 5, points to <X-1,5> and <X+1,5>
				 whitePawnSkippedSquare    :: xsqnum(),  %% for Y = 4, points to <X,3>, else 0
				 blackPawnMoves            :: [sqnum()],
				 blackPawnKills            :: [sqnum()],
				 blackPawnEpSquares        :: [sqnum()], %% for Y = 4, points to <X-1,5> and <X+1,5>
				 blackPawnSkippedSquare    :: xsqnum(),  %% for Y = 5, points to <X,6>, else 0
				 epKillSquare              :: xsqnum(),  %% for Y = 3, points to <X,4>, for Y = 6 points to <X,5>, else 0
				 knightJumps               :: [sqnum()],
				 bishopBeams               :: [[sqnum()]],
				 rookBeams                 :: [[sqnum()]],
				 kingMoves                 :: [sqnum()],
				 castleShortCorner         :: sqnum(),
				 castleShortPath           :: [sqnum()],
				 castleLongCorner          :: sqnum(),
				 castleLongPath            :: [sqnum()]
				}).

-type xsquare() :: #square{} | null.




