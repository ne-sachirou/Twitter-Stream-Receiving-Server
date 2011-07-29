-module(couchdb_processor).
-vsn(1.01).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([start/0, start/1, stop/0]).
-export([process_data/1]).
-export([init/1, terminate/2, handle_cast/2]).

-behavior(gen_server).

-include("../include/erlang_couchdb.hrl").



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
handle_cast({data, {_, _}, {headers, _}} = Data, _LoopData) ->
	{noreply, _LoopData};

handle_cast({data, {_, _}, {part, _}} = Data, _LoopData) ->
	spawn(?MODULE, process_data, [Data]),
	{noreply, _LoopData}.



%%%
%% Data Processing
%

%% Process received streaming
process_data(Data) ->
	{data, {including_word, Word}, {part, Part}} = Data,
	DocumentData = [{including_word, unicode:characters_to_binary(Word)}, {part, Part}],
	
	case Part of
		{_, [_, {<<"text">>, Text} | _]} ->
			case binary:match(Text, unicode:characters_to_binary(Word)) of
				{Start, Length} ->
					case erlang_couchdb:create_document(?DB_HOST, ?DB_DATABASE, DocumentData) of
						_Res ->
							io:format("matched and inserted: ~p~n", [_Res])
					end;
				nomatch ->
					io:format("no_matched~n")
			end;
		_Other ->
			io:format("garbage stream~n")
	end.




