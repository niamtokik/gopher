%%%-------------------------------------------------------------------
%%%
%%%
%%%-------------------------------------------------------------------
-module(gopher_listener).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(state_listener, { accepted = 0 :: integer()
			, refused = 0 :: integer()
			, active = 0 :: integer()
			, active_acceptor = [] :: list(pid())
			}).

%%--------------------------------------------------------------------
%% start our listener gopher service
%%--------------------------------------------------------------------
-spec start() -> pid().
start() ->
    start(7777).

-spec start(Port :: non_neg_integer()) -> pid().
start(Port) ->
    start("localhost", Port).

-spec start(Host :: string(), Port :: non_neg_integer()) -> pid().
start(_Host, Port) ->
    spawn(fun() -> init(_Host, Port) end).    

%%--------------------------------------------------------------------
%% start our listener gopher service with linked to a pid
%%--------------------------------------------------------------------
-spec start_link() -> pid().
start_link() ->
    start_link(7777).

-spec start_link(Port :: non_neg_integer()) -> pid().
start_link(Port) ->
    start_link("localhost", Port).

-spec start_link(Host :: string(), Port :: non_neg_integer()) -> pid().
start_link(_Host, Port) ->
    spawn_link(fun() -> init(_Host, Port) end).

%%--------------------------------------------------------------------
%% stop our listener
%%--------------------------------------------------------------------
stop(Pid) ->
    exit(Pid).

%%--------------------------------------------------------------------
%% We will initialize our connection and get a fd descriptor from
%% operating system. This is managed by an erlang port.
%%--------------------------------------------------------------------
-spec init(Hostname :: string(), Port :: non_neg_integer()) -> none().
init(_Host, Port) ->
    try gen_tcp:listen(Port, [{port, Port}, binary]) of
	{ok, Socket} -> loop(Socket);
	_Else -> io:format("error: ~p~n", [_Else])
    catch
	Error:Value -> io:format("error: ~p,~p~n", [Error,Value])
    end.
	
%%--------------------------------------------------------------------
%% In this step, we have now an initialized ports based on network
%% socket descriptor. We are waiting for some client connection.
%% When a client connects to defined tcp ports, we spawn an acceptor
%% process, and this process will keep the connection active from
%% the client to the server.
%%--------------------------------------------------------------------
-spec loop(Socket :: port()) -> none().
loop(Socket) ->
    loop(Socket, #state_listener{}).

-spec loop(Socket :: port(), State :: #state_listener{}) -> none().
loop(Socket, State) ->
    try gen_tcp:accept(Socket) of
	{ok, AcceptSocket} -> Pid = spawn_link(fun() -> gopher_acceptor:init(AcceptSocket) end),
			      io:format("~p~n", [Pid]),
			      loop(Socket, accepted(State));
	_Else -> loop(Socket, refused(State))
    catch
	Error:Value -> io:format("error: ~p, ~p~n",[Error, Value])
    end.

%%--------------------------------------------------------------------
%% increment accepted value in listener state
%%--------------------------------------------------------------------
-spec accepted(#state_listener{}) -> #state_listener{}.
accepted(State) ->
    #state_listener{ accepted = Accepted } = State,
    State#state_listener{ accepted = Accepted+1 }.

accepted_0001_test() ->
    IN = #state_listener{},
    OUT = #state_listener{ accepted = 1 },
    ?assertEqual(OUT, accepted(IN)).

%%--------------------------------------------------------------------
%% increment refused counter in listener state
%%--------------------------------------------------------------------
-spec refused(#state_listener{}) -> #state_listener{}.
refused(State) ->
    #state_listener{ refused = Refused } = State,
    State#state_listener{ refused = Refused+1 }.

refused_0001_test() ->
    IN = #state_listener{},
    OUT = #state_listener{ refused = 1 },
    ?assertEqual(OUT, refused(IN)).
