%% @author erarafo
%% @doc @todo Add description to xbi_listener.


-module(xbi_listener).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).


start(Socket) ->
	%% the socket is not active
	{ok, Pid} = gen_server:start(?MODULE, [Socket], []),
	if
		Socket =/= "null" ->
		   gen_tcp:controlling_process(Socket, Pid);
	   true ->
		   core_logger:logLine("no socket ..", []),
		   ok
	end,
	{ok, Pid}.



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
		{commands = [],
		 databuffer = "",
		 deferredrequest = false,
		 controller_pid,
		 socket
		 }).

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
init([Socket]) when Socket =:= "null" ->
	core_logger:logLine("no socket ...", []),
	xbi_stdin_listener:start(self()),
    {ok, #state{socket = Socket}};

init([Socket]) ->
	core_logger:logLine("[L] have socket", []),
	inet:setopts(Socket, [{active, true}]),
    {ok, #state{socket = Socket}}.


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

handle_cast({input, Line}, #state{controller_pid = Cpid,
							commands = Commands,
							deferredrequest = DefReq} = State) ->
	
	core_logger:logLine("[L] got a stdin line: ~p", [Line]),
	
	NewCommands = Commands ++ [Line],
	if
		DefReq ->
			[First|Rest] = NewCommands,
			core_logger:logLine("deliver deferred request: ~p", [First]),
			gen_server:cast(Cpid, {request, First}),
			{noreply, State#state{deferredrequest = false, commands = Rest}};
		true ->
			core_logger:logLine("enqueue request", []),
			{noreply, State#state{commands = NewCommands}}
	end;

handle_cast({get_request, ControllerPid},
			#state{commands = [First|More]} = State) ->
	core_logger:logLine("[L] deliver request promptly", []),
	gen_server:cast(ControllerPid, {request, First}),
	{noreply, State#state{commands = More}};

handle_cast({get_request, ControllerPid}, #state{commands = []} = State) ->
	core_logger:logLine("[L] empty queue", []),
	{noreply, State#state{deferredrequest = true, controller_pid = ControllerPid}};

handle_cast(Msg, State) ->
	core_logger:logLine("[L] handle_cast, unexpected: ~p", [Msg]),
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




handle_info({tcp, _Socket, Data},
			#state{controller_pid = ControllerPid,
				   commands = Commands,
				   databuffer = DataBuffer,
				   deferredrequest = R} = State) ->
	core_logger:logLine("socket received data: ~p", [Data]),
	NewDataBuffer = DataBuffer ++ Data,
	
	{NewNewDataBuffer, NewCommands} = ics_lib:add_to_buffer(NewDataBuffer, Commands),
	
	if
		R andalso NewCommands =/= [] ->
			%% deliver deferred response now
			[First|MoreCommands] = NewCommands,
			gen_server:cast(ControllerPid, {request, First}),
			{noreply, State#state{databuffer = NewNewDataBuffer,
								  commands = MoreCommands,
								  deferredrequest = false}};
		true ->
			{noreply, State#state{databuffer = NewNewDataBuffer, commands = NewCommands}}
	end;

handle_info(Info, State) ->
	core_logger:logLine("[L] handle_info, unexpected: ~p", [Info]),
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









