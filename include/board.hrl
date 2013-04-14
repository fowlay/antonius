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


-define(HRL_BOARD, true).


%% @doc All tuples are indexed 1..64.

-record(board, {
				tuple           :: tuple(),
				dict            :: dict(),
				rookLanes       :: tuple(),
				bishopLanes     :: tuple(),
				queenLanes      :: tuple(),
				knightJumps     :: tuple(),
				kingMoves       :: tuple()
  }).
