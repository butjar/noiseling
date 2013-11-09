%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2013 by Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%%-------------------------------------------------------------------
-module(noiseling_server).

-behaviour(gen_server).

%% API
-export([start_link/0, get_connected_listeners/1, get_streamers/0, 
    connect_listener/2, disconnect_listener/2, start_streamer/4,
     kill_streamer/1, add_listener/1, remove_listener/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(streamer, {
            pid,
            connected_listeners = [],
            stream_name,
            lat,
            long,
            desc
        }).

-record(state, {
            streamers = [],
            listeners = []
        }).

%%%===================================================================
%%% API
%%%===================================================================
get_connected_listeners(StreamPid) ->
    {ok, {listeners, Listeners}} = gen_server:call(?MODULE, {get_connected_listeners, StreamPid}),
    {ok, {connected_listeners, Listeners}}.

get_streamers() ->
    {ok, {streamers, Streamers}} = gen_server:call(?MODULE, {get_streamers}),
    {ok, {streamers, Streamers}}.

connect_listener(StreamPid, ListenerPid) -> 
    ok = gen_server:call(?MODULE, {connect_listener, StreamPid, ListenerPid}),
    ok.

disconnect_listener(StreamPid, ListenerPid) ->
    ok = gen_server:call(?MODULE, {disconnect_listener, StreamPid, ListenerPid}),
    ok.

start_streamer(Stream_name, Lat, Long, Desc) -> 
    {ok, {streamer, Streamer}} = gen_server:call(?MODULE, {start_streamer, Stream_name, Lat, Long, Desc}),
    {ok, {listeners, Listeners}} = gen_server:call(?MODULE, {get_all_listeners}),
    send_data_to_listeners({streamer_added_event, Streamer}, Listeners),
    {ok, {streamer, Streamer}}.

kill_streamer(StreamPid) -> 
    ok = gen_server:call(?MODULE, {kill_streamer, StreamPid}),
    {ok, {listeners, Listeners}} = gen_server:call(?MODULE, {get_all_listeners}),
    send_data_to_listeners({streamer_removed_event, StreamPid}, Listeners),
    ok.

add_listener(ListenerPid) -> 
    ok = gen_server:call(?MODULE, {add_listener, ListenerPid}),
    ok.

remove_listener(ListenerPid) ->
    ok = gen_server:call(?MODULE, {remove_listener, ListenerPid}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> 
    Server = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    Server.
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    State = #state{},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({start_streamer, StreamName, Lat, Long, Desc}, _From, State) ->
    StreamPid = spawn(fun() -> stream_data_loop() end),
    Streamer = #streamer{pid=StreamPid, stream_name=StreamName, lat=Lat, long=Long, desc=Desc},
    NewState = State#state{streamers = State#state.streamers ++ [ Streamer ]},
    {reply, {ok, {streamer, Streamer}}, NewState};

handle_call({kill_streamer, StreamerPid}, _From, State) ->
    case lists:keyfind(StreamerPid, 2, State#state.streamers) of
        false -> {reply, ok, State};
        _Streamer ->
            NewStreamers = lists:keydelete(StreamerPid, 2, State#state.streamers),
            NewState = State#state{streamers = NewStreamers},
            StreamerPid ! shutdown,
            {reply, ok, NewState}
    end;
handle_call({add_listener, ListenerPid}, _From, State) ->
    NewState = State#state{listeners = State#state.listeners ++ [ ListenerPid ]},
    {reply, ok, NewState};

handle_call({remove_listener, ListenerPid}, _From, State) ->
    NewState = State#state{listeners = State#state.listeners -- [ ListenerPid ]},
    {reply, ok, NewState};

handle_call({connect_listener, StreamerPid, ListenerPid}, _From, State) ->
    case lists:keyfind(StreamerPid, 2, State#state.streamers) of
        false -> {reply, ok, State};
        Streamer ->
            Streamer = lists:keyfind(StreamerPid, 2, State#state.streamers),
            NewListeners = Streamer#streamer.connected_listeners ++ [ListenerPid],
            NewStreamers = lists:keyreplace(StreamerPid, 2, State#state.streamers, Streamer#streamer{connected_listeners=NewListeners}),
            NewState = State#state{streamers = NewStreamers},
            {reply, ok, NewState}
    end;
handle_call({disconnect_listener, StreamerPid, ListenerPid}, _From, State) ->
    case lists:keyfind(StreamerPid, 2, State#state.streamers) of
        false -> {reply, ok, State};
        Streamer ->
            NewListeners = Streamer#streamer.connected_listeners -- [ListenerPid],
            NewStreamers = lists:keyreplace(StreamerPid, 2, State#state.streamers, Streamer#streamer{connected_listeners=NewListeners}), 
            NewState = State#state{streamers = NewStreamers},
            {reply, ok, NewState}
    end;
handle_call({get_connected_listeners, StreamerPid}, _From, State) ->
    case lists:keyfind(StreamerPid, 2, State#state.streamers) of
        false ->
            {reply, {ok, {listeners, []}}, State};
        Streamer ->
            Listeners = Streamer#streamer.connected_listeners,
            {reply, {ok, {listeners, Listeners}}, State}
    end;
handle_call({get_all_listeners}, _From, State) ->
    Listeners = State#state.listeners,
    {reply, {ok, {listeners, Listeners}}, State};

handle_call({get_streamers}, _From, State) ->
    Streamers = State#state.streamers,
    {reply, {ok, {streamers, Streamers}}, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_data_to_listeners(Data, Listeners) ->
    lists:foreach(fun(Pid)-> Pid ! Data end, Listeners).

stream_data_loop() ->
    receive
        shutdown ->
            exit(normal);
        Chunk ->
            {ok, {connected_listeners, Listeners}} = get_connected_listeners(self()),
            case Listeners of
                [] -> 
                    stream_data_loop();
                _Any -> 
                    send_data_to_listeners({audio_chunk, Chunk}, Listeners),
                    stream_data_loop()
            end
    end.