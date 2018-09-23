%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(gopher_parser).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(DATA(TT, V), 
	data({TT, Display, DescriptorString, Server, Port}) ->
	       Data = lists:join(<<"\t">>, [Display, DescriptorString, Server, Port]),
	       Bitstring = << X || X <- Data >>,
	       <<V, Bitstring/bitstring, "\r\n">> ).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
?DATA(file, "0");
?DATA(directory, "1");
?DATA(phonebook, "2");
?DATA(error, "3");
?DATA(mac, "4");
?DATA(dos, "5");
?DATA(uuencode, "6");
?DATA(search, "7");
?DATA(telnet, "8");
?DATA(binary, "9");
?DATA(redundant, "+");
?DATA(tn3270,  "T");
?DATA(gif, "g");
?DATA(image, "I").

data_0001_test() ->    
    IN = {file, <<"About internet Gopher">>, <<"Stuff:About us">>, <<"rawBits.micro.umn.edu">>, <<"70">>},
    OUT = <<"0About internet Gopher\tStuff:About us\trawBits.micro.umn.edu\t70\r\n">>,
    ?assertEqual(OUT, data(IN)).

data_0002_test() ->
    IN = {directory, <<"My test">>, <<"Nothing">>, <<"localhost">>, <<"70">>},
    OUT = <<"1My test\tNothing\tlocalhost\t70\r\n">>,
    ?assertEqual(OUT, data(IN)).

directory(Socket, Path) ->
    gen_tcp:send(Socket, <<"test">>),
    gen_tcp:close(Socket).

file(Socket, Path) ->
    {ok, Data} = file:read_file(Path),
    gen_tcp:send(Socket, Data),
    gen_tcp:close(Socket).


info(Path) ->
    {ok, {file_info, _Size, Type, read_write
	 , _ ,_,_ 
	 , _,_,_,_,_,_,_}} = file:read_file_info(Path),
     Type.
    

-spec clean_route(Route :: bitstring()) -> bitstring().
clean_route(Route) ->
    clean_route(Route, <<>>).

clean_route(<<"\n">>, Acc) ->
    Acc;
clean_route(<<"\r\n">>, Acc) ->
    Acc;
clean_route(<<A:8/bitstring, Rest/bitstring>>, Acc) ->
    clean_route(Rest, <<Acc/bitstring, A:8/bitstring>>).

clean_route_0001_test() ->
    IN = <<"thisisatest\r\n">>,
    OUT = <<"thisisatest">>,
    ?assertEqual(OUT, clean_route(IN)).
