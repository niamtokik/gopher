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
