%% @author erarafo
%% @doc @todo Add description to xbi_socket_listener.


-module(xbi_socket_listener).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2]).


start(Master, Socket) ->
	{ok, Pid} = gen_server:start(?MODULE, [Master, Socket], []),
	gen_tcp:controlling_process(Socket, Pid),
	Master ! {ok}.   %% TODO ........ this was forgotten for a long time!!!!!!!



-record(state,
		{master,
		 socket,
		 buffer = ""}).

init([Master, Socket]) ->
	inet:setopts(Socket, [{active, true}]),
    {ok, #state{master = Master, socket = Socket}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info({tcp, _Socket, Data}, #state{master = Master} = State) ->
	core_logger:logLine("socket received data: ~p", [Data]),
	NewBuffer = State#state.buffer ++ Data,
	NewNewBuffer = eager_add(Master, NewBuffer),
	{noreply, State#state{buffer = NewNewBuffer}};

handle_info({tcp_closed, _Socket}, State) ->
	core_logger:logLine("socket closed by xboard backend proxy", []),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


eager_add(Pid, Buffer) ->
	case leader_line(Buffer, "") of
		{null, _} ->
			Buffer;
		{Line, Remainder} ->
			core_logger:logLine("~p ! {add, ~p}", [Pid, Line]),
			Pid ! {add, Line},
			eager_add(Pid, Remainder)
	end.



%% duplicated, TODO

%% @doc Splits off the leading newline-terminated line. The returned
%% value is {null, _} if no newline is seen. If splitting succeeds,
%% the split-off string and the remainder is returned. The split-off
%% string is trimmed.

-spec leader_line(string(), string()) -> {string()|null, string()}.


leader_line([], Acc) ->
	{null, lists:reverse(Acc)};

leader_line([$\n|More], Acc) ->
	{lists:reverse(Acc), More};

leader_line([A|More], Acc) ->
	leader_line(More, [A|Acc]).

