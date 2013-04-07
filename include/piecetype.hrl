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




-define(HRL_PIECETYPE, true).

-ifndef(HRL_ANTONIUS).
-include("antonius.hrl").
-endif.



%% doc Types are: king, queen, rook, bishop, knight, whitePawn, blackPawn.




-record(pieceType, {
					name            :: piecetypeid(),
					rname           :: string(),
					abbreviation    :: string(),
					value           :: smallint(),
					order           :: smallint()
					}).



-type piecetypeid() ::  king | queen | rook | bishop | knight | whitePawn | blackPawn.

-type xpieceType() :: #pieceType{} | null.

% TODO: More global?
-type atomOrString() :: atom() | string().
