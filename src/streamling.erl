-module(streamling).

-export([start/0]).

start() ->
	{ok, _Pid} = streaming_server:start_link(),

	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(bullet),
	ok = application:start(streamling).
