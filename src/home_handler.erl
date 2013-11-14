-module(home_handler).
-export([init/3]).
-export([handle/2,terminate/3]).
-behaviour(cowboy_http_handler).

init({tcp,http}, Req, []) ->
	{ok, Req, undefined_state}.

handle(Req, St) ->
	{ok, [[NetworkDevice]]} = init:get_argument(network_device),
	IpAddress = get_ip_address(NetworkDevice),
	{ok, WsAddressList} = ip4_address_to_binary(IpAddress),
	erlydtl:compile("./priv/templates/home.dtl", home_template),

	{ok,Body} = home_template:render([
		{ws_address, WsAddressList}
	]),

	{ok,Reply} = cowboy_req:reply(200,[], Body, Req),

	{ok,Reply,St}.	%% never reached

terminate(_What, _Req, _St) ->
	ok.

%% internal
get_ip_address(Dev) ->
    {ok,IfAddrs} = inet:getifaddrs(),
    {_,IfInfo} = lists:keyfind(Dev, 1, IfAddrs),
    {_,IpAddr} = lists:keyfind(addr, 1, IfInfo),
    IpAddr.

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