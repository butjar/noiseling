%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%%-------------------------------------------------------------------
-module(stream_ws_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).
%% handler state
-record(state, {
			streamer = none
		}).
%% streamer record defines an output-stream
-record(streamer, {
			pid,
			connected_listeners = [],
			stream_name,
			lat,
			long,
			desc
		}).

%% upgrades the protocol to websockets
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% sets the state to a state-Record with streamer = none
websocket_init(_TransportName, Req, _Opts) ->
	State = #state{},
	{ok, Req, State}.
%% requests the server-backen to stop the streamer when stop stream messages receives
websocket_handle({text, <<"stop_streamer">>}, Req, State) ->
	NewState = State#state{streamer = none},
	noiseling_server:kill_streamer(State#state.streamer#streamer.pid),
	{reply, {text, <<"streamer_stopped">>}, Req, NewState};
%% handles JSON messages, currently only the start streamer message is handled
websocket_handle({text, Msg}, Req, State) ->
	Struct = mochijson2:decode(Msg),
	{struct, JsonData} = Struct,
	case JsonData of
		[{<<"start_streamer">>, {struct,StreamerIoList}}] ->
			StreamName = binary_to_list(proplists:get_value(<<"stream_name">>, StreamerIoList)),
			Lat = binary_to_list(proplists:get_value(<<"lat">>, StreamerIoList)),
			Long = binary_to_list(proplists:get_value(<<"lon">>, StreamerIoList)),
			Desc = binary_to_list(proplists:get_value(<<"desc">>, StreamerIoList)),
			{ok, {streamer, Streamer}} = noiseling_server:start_streamer(StreamName, Lat, Long, Desc),
			NewState = State#state{streamer = Streamer},
		{reply, {text, <<"streamer_started">>}, Req, NewState}
	end;
%% receives the audio blobs to stream from the client and sends it to the according streaming-process
websocket_handle({binary, Data}, Req, State) ->
	case State#state.streamer of
		none -> true;
		Streamer ->
			Streamer#streamer.pid ! Data
	end,
	{ok, Req, State}.
%% handles info messages
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
%% requests the server-backend to kill the streaming process when websocket connection closes
websocket_terminate(_Reason, _Req, State) ->
	case State#state.streamer of
		none -> true;
		_Any -> ok = noiseling_server:kill_streamer(State#state.streamer#streamer.pid)
	end,
	ok.