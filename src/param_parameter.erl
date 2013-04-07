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
%% Created: Oct 20, 2012
%% Description: TODO: Add description to param_parameter
-module(param_parameter).

%%
%% Include files
%%

-include("antonius.hrl").

%%
%% Exported Functions
%%

-export([setRecursionDepth/1]).
-export([getRecursionDepth/0]).
-export([setNumberOfThreads/1]).
-export([getNumberOfThreads/0]).

%%
%% API Functions
%%



%%
%% Local Functions
%%




setRecursionDepth(Depth) ->
	core_state:sput(recursionDepth, Depth).

getRecursionDepth() ->
	case core_state:sget(recursionDepth) of
		{ok, Value} ->
			Value;
		_ ->
			?RECURSION_DEPTH_DEFAULT
	end.


-spec setNumberOfThreads(smallint()) -> ok.

setNumberOfThreads(N) ->
	core_state:sput(maxThreads, N).


-spec getNumberOfThreads() -> smallint().

getNumberOfThreads() ->
	{ok, Result} = core_state:sget(maxThreads),
	Result.
