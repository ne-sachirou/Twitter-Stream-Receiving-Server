-module(couchdb_processor).
-vsn(1.02).
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
	
	case Part of
		{_, [_, {<<"text">>, Text} | _]} ->
			case list_elements_in_binary(Text, Words) of
				{match, MatchedWords} ->
					io:format("matched: ~p~n", [MatchedWords]),
					DocumentData = [{including_words, all_convert_to_binary_from_unicode(MatchedWords)}, {part, Part}],
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

%% Binary is including the element in List
list_elements_in_binary(Binary, Words) ->
	list_elements_in_binary(Binary, Words, []).

list_elements_in_binary(_, [], []) ->
	nomatch;
list_elements_in_binary(_, [], Matches) ->
	{match, Matches};
list_elements_in_binary(Binary, [Word | T], Matches) ->
	case binary:match(Binary, unicode:characters_to_binary(Word)) of
		{Start, Length} ->
			NewMatches = [Word | Matches],
			list_elements_in_binary(Binary, T, NewMatches);
		nomatch ->
			list_elements_in_binary(Binary, T, Matches)
	end.

%% Convert from unicode character to binary in each element of List
all_convert_to_binary_from_unicode(Unicodes) ->
	all_convert_to_binary_from_unicode(Unicodes, []).

all_convert_to_binary_from_unicode([], Converted) ->
	Converted;
all_convert_to_binary_from_unicode([Unicode | T], Converted) ->
	ConvertedUnicode = unicode:characters_to_binary(Unicode),
	NewConverted = [ConvertedUnicode | Converted],
	all_convert_to_binary_from_unicode(T, NewConverted).
