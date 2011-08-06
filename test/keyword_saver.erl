-module(keyword_saver).
-vsn(1.01).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([start/0]).
-export([init/1]).

-behavior(supervisor).

-define(SEARCH_KEYWORDS, unilib:utf8binary_to_list(unilib:utf8_to_binary([
	"UNICODE_keyword01",
	"UNICODE_keyword02",
	"UNICODE_keyword03",
	"UNICODE_keyword04",
	"UNICODE_keyword05",
]))).


start() ->
	application:start(twitter),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%%%
%% Callback Function on OTP/supervisor
%

init(_Args) ->
	UsrChildren = [
		{
			request_stream,
			{stream_server, request_stream, [couchdb, ?SEARCH_KEYWORDS]},
			permanent,
			1000,
			worker,
			dynamic
		}
	],
	{ok, {{one_for_one, 3, 10}, UsrChildren}}.
