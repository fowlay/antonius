%% @author erarafo
%% @doc @todo Add description to xbi_controller.


-module(xbi_controller).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1]).
-export([allowLogging/1]).
-export([log/3]).
-export([log/2]).
-export([stdout/2]).
-export([stdout/3]).


-include("exception.hrl").
-include("antonius.hrl").

%% Mode is "xboard" or "ics"
%% "xboard" implies Socket == "null"


start([LibPath, Mode, Socket, Master]) ->
	
	Sid =
		if
			Socket =/= "null" ->
				{ok, {Addr, Port}} = inet:peername(Socket),
				{Port, Addr};
			true ->
				xboard
		end,
	
	if
		Socket =/= "null" ->
			core_state:sput({Sid, socket}, Socket),
			%% metal_log("running, using socket: ~p", []),
			Master ! started;
		true ->
			%% metal_log("running, using standard I/O", []),
			{ok, _} = core_logger:start("/tmp/single.log")   %% TODO, where should this file be?

	       , core_logger:logLine("the log is operational", [])
	end,

	if
		Socket =:= "null" ->
			case core_util:initVm(LibPath, list_to_atom(Mode)) of
				{ok} ->
					ok;
				_ ->
					init:stop()
			end;
		true ->
			ok
	end,
	
	core_logger:logLine("done init VM", []),

	{ok, ListenerPid} = xbi_listener:start(Socket),

	core_logger:logLine("done start listener", []),

	{ok, _Pid} = gen_server:start(?MODULE, [Sid, Mode, ListenerPid], []),
	
	core_logger:logLine("done start controller loop", []),
	
	ok.

-spec stdout(sid(), string()) -> ok.

stdout(Sid, String) ->
	try
		core_logger:logLine("xbi_controller, send data to xboard: ~p", [String]),
		if
			not(is_tuple(Sid)) ->
				core_logger:logLine("[C] mode: ~p", [xboard]),
				io:fwrite("~s~n", [String]);
			true ->
				core_logger:logLine("[C] mode: ~p", [ics]),
				{ok, Socket} = core_state:sget({Sid, socket}),
				ok = gen_tcp:send(Socket, String ++ "\n")
		end
	catch
		A:B:C ->
			core_logger:logLine("caught: ~p, ~p, stack:~n~p", [A, B, C])
	end.


-spec stdout(sid(), string(), [term()]) -> ok.

stdout(Sid, Format, Data) ->
  stdout(Sid, lists:flatten(io_lib:format(Format, Data))).


allowLogging(Sid) ->
	core_state:sput({Sid, loggingAllowed}, true),
	[log(Sid, String) || String <- core_state:sget({Sid, deferredLog})].

log(Sid, Format, Arguments) ->
	log(Sid, io_lib:format(Format, Arguments)).
	

log(Sid, String) ->
	case core_state:sget({Sid, loggingAllowed}) of
		false ->
			core_state:sput({Sid, deferredLog}, lists:append(core_state:sget({Sid, deferredLog}), [String]));
		_ ->
			io:fwrite("# ~s~n", [String])
	end.



-record(state,
		{sid :: sid(),
		 listener :: pid(),
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
init([Sid, Mode, ListenerPid]) ->
	
	core_logger:logLine("[C] mode: ~p", [Mode]),

	if ?IS_XBOARD(Sid) -> cmd_dict:setupCommands(); true -> ok end,
		
	cli_game:setupBoard(Sid),

	core_state:sput({Sid, loggingAllowed}, false),
	core_state:sput({Sid, deferredLog}, []),
	
	core_gamestate:create(Sid),
	
	
	Dict = xbi_command:setup(),
	
    {ok, #state{sid = Sid, dict = Dict, listener = ListenerPid}, 0}.


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
handle_cast({request, Line}, #state{sid = Sid, dict = Dict} = State) ->
	
	%% execute the request
	%% maybe recognize "quit" especially
    %% write to socket, or to stdout

	case string:tokens(Line, " ") of
		[] ->
			log(Sid, "unexpected empty line from xboard");
		[Keyword|Arguments] ->
			case dict:is_key(Keyword, Dict) of
				false ->
					log(Sid, " ignore: ~s", [Line]);
				true ->
					core_logger:logLine("about to execute: ~p ~p", [Keyword, Arguments]),
					try
						xbi_command:execute(Sid, Keyword, Arguments, Dict)
					catch
						_A:#exception{type=Type, message=Message, reason=Reason}:Stack ->
							log(Sid, io_lib:format("caught: ~p, reason: ~p, message: ~p", [Type, Message, Reason])),
							log(Sid, io_lib:format("stack trace: ~p~n", [Stack]));
						A:B:Stack ->
							log(Sid, io_lib:format("caught: ~p: ~p~n", [A, B])),
							log(Sid, io_lib:format("stack trace: ~p~n", [Stack]))
					end
			end
	end,
	
	case Line of
		"quit" ->
			log(Sid, "xbi_controller stops"),
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


