-module(gopher_handler).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-record(state, { path = "." :: string() }).

start() ->
    start([]).

start(Args) ->
    Pid = spawn(fun() -> init(Args) end),
    erlang:register(gopher_handler, Pid),
    {ok, Pid}.

init(_Args) ->
    loop(#state{}).

loop(State) ->
    receive 
	{From, {path, Path}} -> check_path(From, Path),
				loop(State);	
	_Else -> io:format("~p~n", [_Else]),
		 loop(State)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
check_path(From, Path) ->
    From ! {self(), <<>>}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec sanitize(Path :: string()) -> {ok, string()}.
sanitize(Path) ->
    sanitize(Path, <<>>).

sanitize(<<>>, Acc) ->
    Acc;
sanitize(<<"../", Rest/bitstring>>, Acc) ->
    sanitize(Rest, <<Acc/bitstring, "/">>);
sanitize(<<A:8/bitstring, Rest/bitstring>>, Acc) ->
    sanitize(Rest, <<Acc/bitstring, A:8/bitstring>>).

sanitize_0001_test() ->
    IN = <<"/../">>,
    OUT = <<"//">>,
    ?assertEqual(OUT, sanitize(IN)).
sanitize_0002_test() ->
    IN = <<"/../../">>,
    OUT = <<"///">>,
    ?assertEqual(OUT, sanitize(IN)).


%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec file(Path :: string()) -> {ok, iodata()}.
file(Path) ->
    file:read_file(Path).


    

