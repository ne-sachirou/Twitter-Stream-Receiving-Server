-module(couchdb_processor).
-vsn(1.12).
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
	{data, {filtering_words, Words}, {part, Part}} = Data,
	{_, Fields} = Part,

	case lists:keyfind(<<"text">>, 1, Fields) of
		{<<"text">>, Text} ->
			%io:format("Text: ~p~n", [Text]),
			case list_elements_in_binary(Text, Words) of
				{match, MatchedWords} ->
					%io:format("matched: ~p~n", [MatchedWords]),
					{DateY, DateM, DateD} = date(),
					{TimeH, TimeI, TimeS} = time(),
					DayOfTheWeek = calendar:day_of_the_week({DateY, DateM, DateD}),
					DocumentData = [
						{including_words, unilib:utf8list_to_binary(MatchedWords)},
						{saved_datetime, [
							{date_y, DateY},
							{date_m, DateM},
							{date_d, DateD},
							{time_h, TimeH},
							{time_i, TimeI},
							{time_s, TimeS},
							{day_of_the_week, DayOfTheWeek}
						]},
						{part, Part}
					],
					io:format("before insert: ~n"),
					case erlang_couchdb:create_document(?DB_HOST, ?DB_DATABASE, DocumentData) of
						_Res ->
							io:format("matched and inserted: ~p~n", [_Res])
					end,
					io:format("inserted: ~n");
				nomatch ->
					%io:format("no_matched~n")
					io:format("")
			end;
		false ->
			io:format("garbage stream: ~n~p~n", [Fields])
	end.

%% Binary is including the element in List
list_elements_in_binary(Binary, Words) ->
	list_elements_in_binary(Binary, Words, []).

list_elements_in_binary(_, [], []) ->
	nomatch;
list_elements_in_binary(_, [], Matches) ->
	{match, Matches};
list_elements_in_binary(Binary, [Word | T], Matches) ->
	%io:format("   Match try: ~p~n", [unicode:characters_to_binary(Word)]),
	case binary:match(Binary, unicode:characters_to_binary(Word)) of
		{Start, Length} ->
			NewMatches = [Word | Matches],
			list_elements_in_binary(Binary, T, NewMatches);
		nomatch ->
			list_elements_in_binary(Binary, T, Matches)
	end.
