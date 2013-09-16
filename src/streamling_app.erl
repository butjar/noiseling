-module(streamling_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	% found at http://gumzo.de/post/108/
	Static = fun(Filetype) ->
	    {lists:append(["/", Filetype, "/[...]"]), cowboy_static, [
	        {directory, {priv_dir, streamling, [list_to_binary(lists:append(["static/",Filetype]))]}},
	        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
	    ]}
	end,

	Dispatch = cowboy_router:compile([
		{'_', [
        	{"/js/bullet.js", cowboy_static, [
            	{directory, {priv_dir, bullet, []}},
            	{file, "bullet.js"},
            	{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        	]},
			Static("css"),
			Static("js"),
			Static("img"),
    		{"/", home_handler, []},
    		{"/record", record_handler, []},
    		{"/receive", receive_handler, []},
    		{"/stream", stream_handler,[]},
    		{"/listen", listen_handler, []}
    	]}
	]),

	{ok,_} = cowboy:start_http(http, 100,
		[{port,8080}],
		[{env,[{dispatch,Dispatch}]}]),

    streamling_sup:start_link().

stop(_State) ->
    ok.

