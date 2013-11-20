%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%%-------------------------------------------------------------------
-module(home_handler).
-export([init/3]).
-export([handle/2,terminate/3]).
-behaviour(cowboy_http_handler).
%% initalizes http handler
init({tcp,http}, Req, []) ->
	{ok, Req, undefined_state}.
%% serves home_dtl template
handle(Req, St) ->
	{ok,Body} = home_dtl:render(),

	{ok,Reply} = cowboy_req:reply(200,[], Body, Req),

	{ok,Reply,St}.	%% never reached
%% handles termination of the handler
terminate(_What, _Req, _St) ->
	ok.