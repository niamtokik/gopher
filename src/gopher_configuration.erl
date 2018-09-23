-module(gopher_configuration).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-behavior(gen_server).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(_Args) ->
    Ets = ets:new(?MODULE, [protected]),
    {ok, Ets}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call({get, port}, _From, State) ->
    [[Port]] = ets:match(State, {'port', '$1'}),
    {reply, {ok, Port}, State};
handle_call(Data, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast({set, port, Port}, State) ->
    true = ets:insert(State, {port, Port}),
    {noreply, State};
handle_cast(Data, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(Data, State) ->
    {noreply, State}.


cast(Message) ->
    gen_server:cast(?MODULE, Message).

call(Message) ->
    gen_server:call(?MODULE, Message).

%%--------------------------------------------------------------------
%% API to retrieve configuration variables.
%%--------------------------------------------------------------------
get(port) ->
    call({get, port}).

%%--------------------------------------------------------------------
%% API to set configuration variable
%%--------------------------------------------------------------------
set(port, Port) ->
    cast({set, port, Port}).
