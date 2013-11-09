-module(noiseling_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	%found at http://gumzo.de/post/108/
	Static = fun(Filetype) ->
	    {lists:append(["/", Filetype, "/[...]"]), cowboy_static, [
	        {directory, {priv_dir, noiseling, [list_to_binary(lists:append(["static/",Filetype]))]}},
	        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
	    ]}
	end,

	Dispatch = cowboy_router:compile([
		{'_', [
			Static("css"),
			Static("js"),
			Static("img"),
    		{"/", home_handler, []},
    		{"/stream", stream_handler, []},
    		{"/listen", listen_handler, []},
    		{"/record", stream_ws_handler, []},
    		{"/receive", listen_ws_handler, []}		
    	]}
	]),
	
	{ok, [[PortString]]} = init:get_argument(port),
	{Port, _} = string:to_integer(PortString),
	{ok,_} = cowboy:start_http(http, 100,
		[{port,Port}],
		[{env,[{dispatch,Dispatch}]}]),

    noiseling_sup:start_link().

stop(_State) ->
    ok.

