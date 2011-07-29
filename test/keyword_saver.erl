-module(start).
-vsn(1.0).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([start/0]).


start() ->
	application:start(twitter),
	spawn(stream_server, request_stream, [
		couchdb,
		[
			"keyword01",
			"keyword02",
			"keyword03",
			"keyword04",
			"keyword05",
			"keyword06",
			"keyword07",
			"keyword08",
			"keyword09",
			"keyword10"
		]
	]).