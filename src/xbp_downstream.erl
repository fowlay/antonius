%% @author erarafo
%% @doc @todo Add description to xbp_downstream.


-module(xbp_downstream).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([start/1, say/2]).




%% @doc Invoked from wrapper script

start([LogFile, ServerHost, ServerPortS]) ->
	persistent_term:put(logFile, LogFile),
	ServerPort = list_to_integer(ServerPortS),
	case gen_server:start({local, ?MODULE}, ?MODULE, [ServerHost, ServerPort], []) of
		{error, Reason} ->
			say("stopping, reason: ~p", [Reason]),
			init:stop();
		{ok, Pid} ->
			say("downstream started: ~p", [Pid]),
			receive stop -> ok end
	end.

say(Format, Data) ->
	{ok, Stream} = file:open(persistent_term:get(logFile), [append]),
	io:fwrite(Stream, Format++"~n", Data),
	file:close(Stream).





%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {socket, upstream, buffer}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([ServerHost, ServerPort]) ->
	case gen_tcp:connect(ServerHost, ServerPort, [], 30000) of
		{error, timeout} ->
			{stop, cannot_connect};
		{ok, Socket} ->
			inet:setopts(Socket, [{active, true}]),
			Pid = xbp_upstream:start(self(), Socket),
			{ok, #state{socket = Socket, upstream = Pid, buffer = ""}}
	end.

	




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


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_info({tcp, _Socket, Data}, #state{buffer = Buffer} = State) ->
	say("received from server: ~p", [Data]),
	NewData = Buffer ++ Data,
	Remainder = eager_write(NewData),
    {noreply, State#state{buffer = Remainder}};

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

eager_write(Buffer) ->
	case leader_line(Buffer, []) of
		{null, _} ->
			Buffer;
		{Line, More} ->
			io:fwrite(standard_io, "~s~n", [Line]),
			say("written to xboard: ~p", [Line]),
			eager_write(More)
	end.

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


