%%%-------------------------------------------------------------------
%%%
%%%
%%%-------------------------------------------------------------------
-module(gopher_acceptor).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% We got the socket from gen_tcp:accept(Socket) and connect our
%% spawned process to current managed socket. We can now start our
%% acceptor loop.
%%--------------------------------------------------------------------
init(Socket) ->
    erlang:port_connect(Socket, self()),
    loop(Socket).

%%--------------------------------------------------------------------
%% This is our main loop! We can do whatever we want here, and we
%% will receive all connection information (data from the client) 
%% directly from our mailbox.
%%--------------------------------------------------------------------
loop(Socket) ->
    receive
	{tcp, Port, Binary} -> 
	    io:format("raw request: ~p~n", [Binary]),
	    route(Port, Binary);
	{tcp_closed, Port} -> 
	    io:format("Closing connection from ~p~n",[Port]),
	    gen_tcp:close(Socket);
	_Else -> io:format("~n~p", [_Else]),
		 loop(Socket)
    end.


%%--------------------------------------------------------------------
%% Now we can create our own router
%%--------------------------------------------------------------------
route(Socket, <<>>) ->
    loop(Socket);
route(Socket, <<"\r\n">>) ->
    gen_tcp:send(Socket,listing()),
    gen_tcp:close(Socket);
route(Socket, <<Route/bitstring>>) ->
    io:format("received: ~p~n", [Route]),
    Clean = gopher_handler:clean_route(Route),
    case gopher_handler:info(Clean) of
	directory -> gopher_handler:directory(Socket, Clean);
	file -> gopher_handler:file(Socket, Clean);
	regular -> gopher_handler:file(Socket, Clean);
	_Else -> io:format("got: ~p~p~n", [Clean, _Else])
    end.


-spec listing() -> iodata().
listing() ->
    List = [<<"0About internet Gopher\tStuff:About us\trawBits.micro.umn.edu\t70\r\n">>
    ,<<"1Around University of Minnesota\tZ,5692,AUM\tunderdog.micro.umn.edu\t70\r\n">>
    ,<<"1Microcomputer News & Prices\tPrices/\tpserver.bookstore.umn.edu\t70\r\n">>
    ,<<"1Courses, Schedules, Calendars\t\tevents.ais.umn.edu\t9120\r\n">>
    ,<<"1Student-Staff Directories\t\tFuinfo.ais.umn.edu\t70\r\n">>
    ,<<"1Departmental Publications\tStuff:DP:\trawBits.micro.umn.edu\t70\r\n">>
    ],
    << X || X <- List >>.
