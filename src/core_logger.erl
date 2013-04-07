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
%% Created: Apr 1, 2012
%% Description: TODO: Add description to logger
-module(core_logger).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).
-export([stop/0]).
-export([log/2]).

-export([prelude/2]).

%%
%% API Functions
%%

start(Pathname) ->
	case file:open(Pathname, [write]) of
		{ok, Stream} ->
			Pid = spawn(?MODULE, prelude, [self(), Stream]),
			register(?MODULE, Pid),
			receive
				ok ->
					ok
			end;
		{error, Reason} ->
			core_util:inconsistencyException("failed to open log file, reason: ~p", [Reason])
	end.
	
	
	

stop() ->
	?MODULE ! {stop, self()},
	receive
		ok ->
			ok
	end.
	

log(Format, Data) ->
	?MODULE ! {message, Format, Data}.

prelude(Master, Stream) ->
	Master ! ok,
	loop(Stream).

%%
%% Local Functions
%%

loop(Stream) ->
	receive
		{message, Format, Data} ->
			io:fwrite(Stream, Format, Data),
			loop(Stream);
		{stop, Master} ->
			file:close(Stream),
			Master ! ok
	end.

			

