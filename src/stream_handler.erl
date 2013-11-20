%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%%-------------------------------------------------------------------
-module(stream_handler).
-export([init/3]).
-export([handle/2,terminate/3]).
-behaviour(cowboy_http_handler).
%% initalizes http handler
init({tcp,http}, Req, []) ->
	{ok, Req, undefined_state}.
%% serves stream_dtl template
handle(Req, St) ->
	{ok, [[WsPort]]} = init:get_argument(port),

	{ok,Body} = stream_dtl:render([
		{ws_port, WsPort}
	]),

	{ok,Reply} = cowboy_req:reply(200,[], Body, Req),

	{ok,Reply,St}.
%% handles termination of the handler
terminate(_What, _Req, _St) ->
	ok.