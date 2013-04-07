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


-define(HRL_MATERIAL, true).

-ifndef(HRL_PIECETYPE).
-include("piecetype.hrl").
-endif.


-ifndef(HRL_SQUARE).
-include("square.hrl").
-endif.



%% TODO: Having both value and order as explicit fields is wasteful. Currently
%% 'value' could be used for ordering too.

-record(piece, {
				type         :: piecetypeid(),
				colour       :: colour(),
				square       :: #square{},
				value        :: smallint(),
				order        :: smallint(),
				pristine     :: boolean()
  }).


-type ppiece() :: #piece{} | null.
