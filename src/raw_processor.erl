-module(raw_processor).
-vsn(1.0).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([start/0, start/1, stop/0]).
-export([init/1, terminate/2, handle_cast/2]).

-behavior(gen_server).



%%%
%% Client Functions (All exported)
%

%% Start server
start() ->
	start(?MODULE).
start(_ServerName) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, _ServerName, []).

%% Stop server
stop() ->
	gen_server:cast(?MODULE, stop).



%%%
%% Callback Functions
%

%% Initialize
init(_Pname) ->
	inets:start(),
	{ok, null}.

%% Terminating
terminate(_Reason, _LoopData) ->
	{_LoopData}.

%% Stop server
handle_cast(stop, _LoopData) ->
	{stop, normal, _LoopData};

%% Receive Streaming Data
handle_cast({data, {headers, Data}}, _LoopData) ->
	io:format("Received Header: ~n~p~n", [Data]),
	{noreply, _LoopData};

handle_cast({data, {part, Data}}, _LoopData) ->
	io:format("Received Data: ~p~n", [Data]),
	{noreply, _LoopData}.