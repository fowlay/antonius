%% @author erarafo
%% @doc @todo Add description to ics_lib.

%% TODO, xbi_listener uses functions in here, so reconsider module name?

-module(ics_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_listener_socket/3, add_to_buffer/2]).

-spec get_listener_socket(integer(), integer(), [any()]) ->
		  {ok, inet:socket(), integer()} |
			  {error, any()} |
			  no_port.

get_listener_socket(Port, PortMax, _Options) when Port > PortMax ->
	nok;

get_listener_socket(Port, PortMax, Options) ->
	case gen_tcp:listen(Port, Options) of
		{error, eaddrinuse} ->
			get_listener_socket(Port+1, PortMax, Options);
		{error, _Reason} ->
			%% logging please, TODO
			get_listener_socket(Port+1, PortMax, Options);
		{ok, Socket} ->
			{ok, Socket, Port}
	end.



-spec add_to_buffer(string(), [string()]) -> {string(), [string()]}.

add_to_buffer(Data, Buffer) ->
	case leader_line(Data, "") of
		null ->
			{Data, Buffer};
		{Command, More} ->
			add_to_buffer(More, Buffer ++ [Command])
	end.


%% @doc Splits off the smallest leading newline-terminated line. The returned
%% value is {null, _} if no newline is seen. If splitting succeeds,
%% the split-off string and the remainder is returned. The split-off
%% string is trimmed.

-spec leader_line(string(), string()) -> {string(), string()}|null.


leader_line([], _Acc) ->
	null;

leader_line([$\n|More], Acc) ->
	{lists:reverse(Acc), More};

leader_line([A|More], Acc) ->
	leader_line(More, [A|Acc]).
