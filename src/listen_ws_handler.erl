%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%%-------------------------------------------------------------------
-module(listen_ws_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).
%% handler state
-record(state, {
            streamer_pid = none
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
    ok = noiseling_server:add_listener(self()),
    {ok, Req, State}.
%% requests all streamers from the server
websocket_handle({text, <<"get_streamers">>}, Req, State) ->
    {ok, {streamers, Streamers}} = noiseling_server:get_streamers(),
    Struct = streamers_to_mochijson_struct(Streamers),
    Msg = iolist_to_binary(mochijson2:encode(Struct)),    
    {reply, {text, Msg}, Req, State};
%% disconnects the listner from the current stream
websocket_handle({text, <<"disconnect">>}, Req, State) ->
    ok = noiseling_server:disconnect_listener(State#state.streamer_pid, self()),
    NewState = State#state{streamer_pid = none},   
    {ok, Req, NewState};
%% handles messages in JSON string format currently its only used when connecting a client
websocket_handle({text, Msg}, Req, State) ->
    Struct = mochijson2:decode(Msg),
    {struct, JsonData} = Struct,
    case JsonData of
        [{<<"connect">>, StreamPidIoList}] ->
            if 
                State#state.streamer_pid /= none ->
                    noiseling_server:disconnect_listener(State#state.streamer_pid, self());
                true -> true
            end,
            StreamPid = list_to_pid(binary_to_list(StreamPidIoList)),
            ok = noiseling_server:connect_listener(StreamPid, self()),
            NewState = State#state{streamer_pid = StreamPid}
    end,        
    {ok, Req, NewState}.
%% sends events when output stream was added/removed or serves audio chunk to the listener client
websocket_info(Data, Req, State) ->
    case Data of
        {streamer_removed_event, StreamerPid} ->
            StreamerPidAsList = list_to_binary(pid_to_list(StreamerPid)),
            Struct = {struct, [{streamer_removed_event, StreamerPidAsList}]},
            Msg = iolist_to_binary(mochijson2:encode(Struct)),
            {reply, {text, Msg}, Req, State};
        {streamer_added_event, Streamer} ->
            {struct, Struct} = streamer_to_mochijson_struct(Streamer),
            EventStruct = {struct, [{streamer_added_event, Struct}]},
            Msg = iolist_to_binary(mochijson2:encode(EventStruct)),
            {reply, {text, Msg}, Req, State};
        {audio_chunk, Chunk} ->
            {reply, {binary, Chunk}, Req, State}
    end.

%% requests server to disconnect the listener when websocket connection terminates
websocket_terminate(_Reason, _Req, State) ->
    case State#state.streamer_pid of
        none ->
            ok = noiseling_server:remove_listener(self());
        StreamPid ->
            ok = noiseling_server:disconnect_listener(StreamPid, self()),
            ok = noiseling_server:remove_listener(self())
    end,
    ok.

%% internal
%% builds streamer list as structs for mochijson as preparation for sending as json string
streamers_to_mochijson_struct(Streamers) ->
    StreamerAsStructs = lists:map(fun(Streamer) -> streamer_to_mochijson_struct(Streamer) end, Streamers),
    {struct, [{streamers, StreamerAsStructs}]}.

%% builds a mochijson struct from a streamer record
streamer_to_mochijson_struct(Streamer) ->
    Pid      = list_to_binary(pid_to_list(Streamer#streamer.pid)),
    Name     = list_to_binary(Streamer#streamer.stream_name),
    Lat      = list_to_binary(Streamer#streamer.lat),
    Long     = list_to_binary(Streamer#streamer.long),
    Desc     = list_to_binary(Streamer#streamer.desc),
    {struct, [{pid, Pid}, {name, Name},{lat, Lat},{long, Long},{desc, Desc}]}.