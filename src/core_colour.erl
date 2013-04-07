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
%% Created: Jan 23, 2012
%% Description: TODO: Add description to colour
-module(core_colour).

%%
%% Include files
%%
-include("colour.hrl").
%%
%% Exported Functions
%%
-export([colour/1]).
-export([otherColour/1]).
-export([shortName/1]).

%%
%% API Functions
%%


-spec colour(string()) -> colour().

colour("w") -> white;
colour("W") -> white;

colour("b") -> black;
colour("B") -> black.


-spec otherColour(colour()) -> colour().

otherColour(white) ->
	black;

otherColour(black) ->
	white.



shortName(white) ->
	"w";

shortName(black) ->
	"b".

%%
%% Local Functions
%%

