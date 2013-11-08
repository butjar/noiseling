-module(listen_handler).
-export([init/3]).
-export([handle/2,terminate/3]).
-behaviour(cowboy_http_handler).

init({tcp,http}, Req, []) ->
	{ok, Req, undefined_state}.

handle(Req, St) ->
	{ok, [{addr, WsAddress}]} = inet:ifget("wlan0", [addr]),
	{ok, WsAddressList} = ip4_address_to_binary(WsAddress),
	erlydtl:compile("./priv/templates/listen.dtl", stream_template),

	{ok,Body} = stream_template:render([
		{ws_address, WsAddressList}
	]),

	{ok,Reply} = cowboy_req:reply(200,[], Body, Req),

	{ok,Reply,St}.	%% never reached

terminate(_What, _Req, _St) ->
	ok.

%% internal
ip4_address_to_binary(Address)->
	[H|T] = tuple_to_list(Address),
	parse_address(T, integer_to_list(H)).

parse_address([], List)->
	{ok, List};
parse_address(Address, List) ->
	[H|T] = Address,
	NewList = List ++ "." ++ integer_to_list(H),
	parse_address(T, NewList).
%%EOF