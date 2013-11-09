-module(listen_handler).
-export([init/3]).
-export([handle/2,terminate/3]).
-behaviour(cowboy_http_handler).

init({tcp,http}, Req, []) ->
	{ok, Req, undefined_state}.

handle(Req, St) ->
	{ok, [[Port]]} = init:get_argument(port),
	{ok, [[NetworkDevice]]} = init:get_argument(network_device),
	{ok, [{addr, WsAddress}]} = inet:ifget(NetworkDevice, [addr]),
	{ok, WsAddressList} = ip4_address_to_binary(WsAddress),
	erlydtl:compile("./priv/templates/listen.dtl", listen_template),

	{ok,Body} = listen_template:render([
		{ws_address, WsAddressList},
		{port, Port}
	]),

	{ok,Reply} = cowboy_req:reply(200,[], Body, Req),

	{ok,Reply,St}.

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