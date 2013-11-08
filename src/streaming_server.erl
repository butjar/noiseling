%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2013 by Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%%-------------------------------------------------------------------
-module(streaming_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE). 

-record(streamer, {
            pid,
            connected_listners = [],
            stream_name,
            lat,
            long,
            desc
        }).

-record(state, {
            streamers = [],
            listners = []
        }).

%%%===================================================================
%%% API
%%%===================================================================

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
    {reply, {ok, {stream_pid, Streamer}}, NewState};

handle_call({kill_streamer, StreamerPid}, _From, State) ->
    NewStreamers = lists:keydelete(StreamerPid, 2, State#state.streamers),
    NewState = State#state{streamers = NewStreamers},
    StreamerPid ! shutdown,
    {reply, ok, NewState};

handle_call({add_listner, ListnerPid}, _From, State) ->
    NewState = State#state{listners = State#state.listners ++ [ ListnerPid ]},
    {reply, ok, NewState};

handle_call({remove_listner, ListnerPid}, _From, State) ->
    NewState = State#state{listners = State#state.listners -- [ ListnerPid ]},
    {reply, ok, NewState};

handle_call({connect_listner, StreamerPid, ListnerPid}, _From, State) ->
    Streamer = lists:keyfind(StreamerPid, 2, State#state.streamers),
    NewListners = Streamer#streamer.connected_listners ++ [ListnerPid],
    NewStreamers = lists:keyreplace(StreamerPid, 2, State#state.streamers, Streamer#streamer{connected_listners=NewListners}),
    NewState = State#state{streamers = NewStreamers},
    {reply, ok, NewState};

handle_call({disconnect_listner, StreamerPid, ListnerPid}, _From, State) ->
    Streamer = lists:keyfind(StreamerPid, 2, State#state.streamers),
    NewListners = Streamer#streamer.connected_listners -- [ListnerPid],
    NewStreamers = lists:keyreplace(StreamerPid, 2, State#state.streamers, Streamer#streamer{connected_listners=NewListners}), 
    NewState = State#state{streamers = NewStreamers},
    {reply, ok, NewState};

handle_call({get_connected_listners, StreamerPid}, _From, State) ->
    case lists:keyfind(StreamerPid, 2, State#state.streamers) of
        false ->
            {reply, {ok, {listners, []}}, State};
        Streamer ->
            Listners = Streamer#streamer.connected_listners,
            {reply, {ok, {listners, Listners}}, State}
    end;
handle_call({get_all_listners}, _From, State) ->
    Listners = State#state.listners,
    {reply, {ok, {listners, Listners}}, State};

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
send_data_to_listners(Data, Listners) ->
    lists:foreach(fun(Pid)-> Pid ! Data end, Listners).

get_connected_listners(StreamPid) ->
    {ok, {listners, Listners}} = gen_server:call(?MODULE, {get_connected_listners, StreamPid}),
    {ok, {connected_listners, Listners}}.

get_streamers() ->
    {ok, {streamers, Streamers}} = gen_server:call(?MODULE, {get_streamers}),
    {ok, {streamers, Streamers}}.

connect_listner(StreamPid, ListnerPid) -> 
    ok = gen_server:call(?MODULE, {connect_listner, StreamPid, ListnerPid}),
    ok.

disconnect_listner(StreamPid, ListnerPid) -> 
    ok = gen_server:call(?MODULE, {disconnect_listner, StreamPid, ListnerPid}),
    ok.

start_streamer(Stream_name, Lat, Long, Desc) -> 
    {ok, {stream_pid, Streamer}} = gen_server:call(?MODULE, {start_streamer, Stream_name, Lat, Long, Desc}),
    {ok, {listners, Listners}} = gen_server:call(?MODULE, {get_all_listners}),
    send_data_to_listners({streamer_added_event, Streamer}, Listners),
    {ok, {stream_pid, Streamer}}.

kill_streamer(StreamPid) -> 
    ok = gen_server:call(?MODULE, {kill_streamer, StreamPid}),
    {ok, {listners, Listners}} = gen_server:call(?MODULE, {get_all_listners}),
    send_data_to_listners({streamer_removed_event, StreamPid}, Listners),
    ok.

add_listner(ListnerPid) -> 
    ok = gen_server:call(?MODULE, {add_listner, ListnerPid}),
    ok.

remove_listner(ListnerPid) ->
    ok = gen_server:call(?MODULE, {remove_listner, ListnerPid}),
    ok.

stream_data_loop() ->
    receive
        shutdown ->
            exit(normal);
        Chunk ->
            {ok, {connected_listners, Listners}} = get_connected_listners(self()),
            case Listners of
                [] -> 
                    stream_data_loop();
                _Any -> 
                    send_data_to_listners({audio_chunk, Chunk}, Listners),
                    stream_data_loop()
            end
    end.