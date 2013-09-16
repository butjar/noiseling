-module(stream_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket}.
 
websocket_init(TransportName, Req, _Opts) ->
    streaming_server:register_streamer(self()),
	io:format("~nStream_server started PID:~p~n",[self()]),
    {ok, Req, undefined_state}.
 
websocket_handle({binary, Data}, Req, State) ->
	global:send('stream', Data),
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.
 
websocket_terminate(_Reason, _Req, _State) ->
    ok.