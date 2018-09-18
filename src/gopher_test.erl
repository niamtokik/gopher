%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(gopher_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec listener() -> none().
listener() ->
    listener(7777).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec listener(Port :: non_neg_integer()) -> none().
listener(Port) ->
    listener("localhost", Port).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec listener(Hostname :: string(), Port :: non_neg_integer()) -> none().
listener(_Host, Port) ->
    spawn( fun() -> listener_init(_Host, Port) end).

%%--------------------------------------------------------------------
%% We will initialize our connection and get a fd descriptor from
%% operating system. This is managed by an erlang port.
%%--------------------------------------------------------------------
-spec listener_init(Hostname :: string(), Port :: non_neg_integer()) -> none().
listener_init(_Host, Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{port, Port}, binary]),
    listener_loop(Socket).

%%--------------------------------------------------------------------
%% In this step, we have now an initialized ports based on network
%% socket descriptor. We are waiting for some client connection.
%% When a client connects to defined tcp ports, we spawn an acceptor
%% process, and this process will keep the connection active from
%% the client to the server.
%%--------------------------------------------------------------------
-spec listener_loop(Socket :: port()) -> none().
listener_loop(Socket) ->
    {ok, AcceptSocket} = gen_tcp:accept(Socket),
    Pid = spawn(fun() -> acceptor_init(AcceptSocket) end),
    io:format("~p~n", [Pid]),
    listener_loop(Socket).

%%--------------------------------------------------------------------
%% We got the socket from gen_tcp:accept(Socket) and connect our
%% spawned process to current managed socket. We can now start our
%% acceptor loop.
%%--------------------------------------------------------------------
acceptor_init(Socket) ->
    erlang:port_connect(Socket, self()),
    accept_loop(Socket).

%%--------------------------------------------------------------------
%% This is our main loop! We can do whatever we want here, and we
%% will receive all connection information (data from the client) 
%% directly from our mailbox.
%%--------------------------------------------------------------------
accept_loop(Socket) ->
    receive
	{tcp, _Port, Binary} -> 
	    accept_route(Socket, Binary);
	{tcp_closed, Port} -> 
	    io:format("Closing connection from ~p~n",[Port]),
	    gen_tcp:close(Socket);
	_Else -> io:format("~n~p", [_Else]),
		 accept_loop(Socket)
    end.


%%--------------------------------------------------------------------
%% Now we can create our own router
%%--------------------------------------------------------------------
accept_route(Socket, Binary) ->
    io:format("received: ~p~n", [Binary]),
    accept_loop(Socket).
