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
%% Created: Oct 29, 2012
%% Description: TODO: Add description to cli_rectangle
-module(cli_rectangle).

%%
%% Include files
%%

-include("rectangle.hrl").

%%
%% Exported Functions
%%
-export([create/3]).
-export([compareTo/2]).

%%
%% API Functions
%%

-spec create(smallint(), smallint(), [string()]) -> #rectangle{}.

create(X, Y, Text) ->
	#rectangle{x=X, y=Y, text=Text, upperY=(Y + length(Text) - 1)}.

compareTo(#rectangle{x=X1}, #rectangle{x=X2}) when X1 =/= X2 ->
	X1 - X2;

compareTo(#rectangle{y=Y1}, #rectangle{y=Y2}) when Y1 =/= Y2 ->
	Y1 - Y2;

compareTo(_, _) ->
	core_util:inconsistencyException("rectangle lower-left corners coincide").


%%
%% Local Functions
%%
