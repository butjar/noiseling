-module(listen_handler).
-export([init/3]).
-export([handle/2,terminate/3]).
-behaviour(cowboy_http_handler).

init({tcp,http}, Req, []) ->
	{ok, Req, undefined_state}.

handle(Req, St) ->
	{ok, [[WsPort]]} = init:get_argument(port),

	{ok,Body} = listen_dtl:render([
		{ws_port, WsPort}
	]),

	{ok,Reply} = cowboy_req:reply(200,[], Body, Req),

	{ok,Reply,St}.

terminate(_What, _Req, _St) ->
	ok.