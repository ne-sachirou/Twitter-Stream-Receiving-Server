-module(twitter).
-vsn(1.01).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([start/2, stop/1]).
-export([init/1]).

%% This server behave as a Application and also a Supervisor
-behavior(application).
-behavior(supervisor).



%%%
%% Callback Functions on OTP/application
%

start(_Type, _Args) ->
	inets:start(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
	ok.

%restart() ->
%delete() ->



%%%
%% Callback Function on OTP/supervisor
%

init(_Args) ->
	UsrChildren = [
		{
			stream_server,
			{stream_server, start, []},
			permanent,
			1000,
			worker,
			dynamic
		}
	],
	{ok, {{one_for_one, 3, 10}, UsrChildren}}.

%terminate() ->