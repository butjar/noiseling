-module(receive_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).

-record(state, {
            streamer_pid = none
        }).

-record(streamer, {
            pid,
            connected_listners = [],
            stream_name,
            lat,
            long,
            desc
        }).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.
 
websocket_init(_TransportName, Req, _Opts) ->
    State = #state{},
    ok = streaming_server:add_listner(self()),
    {ok, Req, State}.

websocket_handle({text, <<"get_streamers">>}, Req, State) ->
    {ok, {streamers, Streamers}} = streaming_server:get_streamers(),
    Struct = streamers_to_mochijson_struct(Streamers),
    Msg = iolist_to_binary(mochijson2:encode(Struct)),    
    {reply, {text, Msg}, Req, State};
websocket_handle({text, <<"disconnect">>}, Req, State) ->
    ok = streaming_server:disconnect_listner(State#state.streamer_pid, self()),
    NewState = State#state{streamer_pid = none},   
    {ok, Req, NewState};
websocket_handle({text, Msg}, Req, State) ->
    Struct = mochijson2:decode(Msg),
    {struct, JsonData} = Struct,
    case JsonData of
        [{<<"connect">>, StreamPidIoList}] ->
            if 
                State#state.streamer_pid /= none ->
                    streaming_server:disconnect_listner(State#state.streamer_pid, self());
                true -> true
            end,
            StreamPid = list_to_pid(binary_to_list(StreamPidIoList)),
            ok = streaming_server:connect_listner(StreamPid, self()),
            NewState = State#state{streamer_pid = StreamPid}
    end,        
    {ok, Req, NewState}.

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
 
websocket_terminate(_Reason, _Req, State) ->
    case State#state.streamer_pid of
        none ->
            ok = streaming_server:remove_listner(self());
        StreamPid ->
            ok = streaming_server:disconnect_listner(StreamPid, self()),
            ok = streaming_server:remove_listner(self())
    end,
    ok.

%% internal
streamers_to_mochijson_struct(Streamers) ->
    StreamerAsStructs = lists:map(fun(Streamer) -> streamer_to_mochijson_struct(Streamer) end, Streamers),
    {struct, [{streamers, StreamerAsStructs}]}.

streamer_to_mochijson_struct(Streamer) ->
    Pid      = list_to_binary(pid_to_list(Streamer#streamer.pid)),
    Name     = list_to_binary(Streamer#streamer.stream_name),
    Lat      = list_to_binary(Streamer#streamer.lat),
    Long     = list_to_binary(Streamer#streamer.long),
    Desc     = list_to_binary(Streamer#streamer.desc),
    {struct, [{pid, Pid}, {name, Name},{lat, Lat},{long, Long},{desc, Desc}]}.