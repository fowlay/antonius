%% @author erarafo
%% @doc @todo Add description to xbi_controller.


-module(xbi_controller).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1]).
-export([allowLogging/0]).
-export([log/2]).
-export([log/1]).
-export([stdout/1]).
-export([stdout/2]).


-include("exception.hrl").

%% Mode is "xboard" or "ics"
%% "xboard" implies Socket == "null"


start([LibPath, Mode, Socket, Master]) ->
	if
		Socket =/= "null" ->
			%% metal_log("running, using socket: ~p", []),
			Master ! started;
		true ->
			%% metal_log("running, using standard I/O", []),
			{ok, _} = core_logger:start("/tmp/single.log")   %% TODO, where should this file be?

	       , core_logger:logLine("the log is operational", [])
	end,
	
	persistent_term:put(theSocket, Socket),   %% TODO, really dirty

	case core_util:initVm(LibPath, list_to_atom(Mode)) of
		{ok} ->
			ok;
		_ ->
			init:stop()
	end,
	
	core_logger:logLine("done init VM", []),

	{ok, ListenerPid} = xbi_listener:start(Socket),

	core_logger:logLine("done start listener", []),

	{ok, _Pid} = gen_server:start(?MODULE, [Mode, ListenerPid], []),
	
	core_logger:logLine("done start controller loop", []),
	
	ok.

-spec stdout(string()) -> ok.

stdout(String) ->
	try
		core_logger:logLine("xbi_controller, send data to xboard: ~p", [String]),
		case core_state:sget(mode) of
			{ok, xboard} ->
				core_logger:logLine("[C] mode: ~p", [xboard]),
				io:fwrite("~s~n", [String]);
			{ok, ics} ->
				core_logger:logLine("[C] mode: ~p", [ics]),
				Socket = persistent_term:get(theSocket),
				ok = gen_tcp:send(Socket, String ++ "\n")
		end
	catch
		A:B:C ->
			core_logger:logLine("caught: ~p, ~p, stack:~n~p", [A, B, C])
	end.


-spec stdout(string(), [term()]) -> ok.

stdout(Format, Data) ->
  stdout(lists:flatten(io_lib:format(Format, Data))).


allowLogging() ->
	core_state:sput(loggingAllowed, true),
	[log(String) || String <- core_state:sget(deferredLog)].

log(Format, Arguments) ->
	log(io_lib:format(Format, Arguments)).
	

log(String) ->
	case core_state:sget(loggingAllowed) of
		false ->
			core_state:sput(deferredLog, lists:append(core_state:sget(deferredLog), [String]));
		_ ->
			io:fwrite("# ~s~n", [String])
	end.



-record(state,
		{listener :: pid(),
		 dict
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
init([Mode, ListenerPid]) ->
	
	core_logger:logLine("[C] mode: ~p", [Mode]),
	
    %% log("mode is: ~p", [Mode]),

	cmd_dict:setupCommands(),
		
	cli_game:setupBoard(),

	core_state:sput(loggingAllowed, false),
	core_state:sput(deferredLog, []),
	
	core_gamestate:create(),
	
	
	Dict = xbi_command:setup(),
	
    {ok, #state{dict = Dict, listener = ListenerPid}, 0}.


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
handle_cast({request, Line}, #state{dict = Dict} = State) ->
	
	%% execute the request
	%% maybe recognize "quit" especially
    %% write to socket, or to stdout

	case string:tokens(Line, " ") of
		[] ->
			log("unexpected empty line from xboard");
		[Keyword|Arguments] ->
			case dict:is_key(Keyword, Dict) of
				false ->
					log(" ignore: ~s", [Line]);
				true ->
					core_logger:logLine("about to execute: ~p ~p", [Keyword, Arguments]),
					try
						xbi_command:execute(Keyword, Arguments, Dict)
					catch
						_A:#exception{type=Type, message=Message, reason=Reason}:Stack ->
							log(io_lib:format("caught: ~p, reason: ~p, message: ~p", [Type, Message, Reason])),
							log(io_lib:format("stack trace: ~p~n", [Stack]));
						A:B:Stack ->
							log(io_lib:format("caught: ~p: ~p~n", [A, B])),
							log(io_lib:format("stack trace: ~p~n", [Stack]))
					end
			end
	end,
	
	case Line of
		"quit" ->
			log("xbi_controller stops"),
			{stop, normal, State};             %% TODO, differently, stop this server?
		_ ->
			{noreply, State, 0}
	end;

handle_cast(_Msg, State) ->
	%% not expected
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

handle_info(timeout, #state{listener = Listener} = State) ->
	core_logger:logLine("[C] timeout, listener: ~p", [Listener]),
	%% dequeue one command
	gen_server:cast(Listener, {get_request, self()}),
	%% the response arrives as a cast
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


