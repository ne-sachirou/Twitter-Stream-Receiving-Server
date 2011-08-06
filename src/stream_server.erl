-module(stream_server).
-vsn(1.01).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([start/0, start/1, stop/0]).
-export([request_stream/2]).
-export([init/1, terminate/2, handle_cast/2]).

-behavior(gen_server).

%% Constants for Twitter
-include("../include/twitter_access_keys.hrl").



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


%% Start getting the streaming by several ways
%% Method = raw | couchdb
request_stream(_Method, FilterWords) ->
	Processor = case _Method of
		raw ->
			raw_processor:start(),
			fun(Data) -> gen_server:cast(raw_processor, {data, Data}) end;
		couchdb ->
			couchdb_processor:start(),
			fun(Data) ->
				gen_server:cast(
					couchdb_processor,
					{data, {filtering_words, FilterWords}, Data}
				)
			end
	end,
	
	%Request = {?FILTER_URL, [], "application/x-www-form-urlencoded", unicode:characters_to_binary(["track=", FilterWord])},
	Request = {?SAMPLE_URL, [], "application/x-www-form-urlencoded", []},
	HTTPOptions = [],
	Options = [{sync, false}, {stream, self}],
	
	case httpc:request(post, Request, HTTPOptions, Options) of
		{ok, RequestId} -> stream_receiver(RequestId, Processor);
		{error, Reason} -> io:format("HTTP Request Error: ~n~p~n", [Reason])
	end.

stream_receiver(RequestId, Processor) ->
	receive
		{http, {RequestId, stream_start, Headers}} ->
			Processor({headers, Headers}),
			stream_receiver(RequestId, Processor);
		{http, {RequestId, stream, Part}} ->
			case Part of
				<<"\r\n">> -> noop;
				_ ->
					Decoded = mochijson2:decode(Part),
					Processor({part, Decoded})
			end,
			stream_receiver(RequestId, Processor);
		{http, {RequestId, {error, Reason}}} ->
			httpc:cancel_request(RequestId),
			io:format("HTTP Streaming Error: ~n~p~n", [Reason])
	end.


%%%
%% Callback Functions
%

%% Initialize
init(_Pname) ->
	{ok, null}.

%% Terminating
terminate(_Reason, _LoopData) ->
	{_LoopData}.

%% Stop server
handle_cast(stop, _LoopData) ->
	{stop, normal, _LoopData}.