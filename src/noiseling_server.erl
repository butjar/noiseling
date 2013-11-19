%%%-------------------------------------------------------------------
%%% @author Martin Fleischer <butjar@butjar-ThinkPad-X1-Carbon>
%%% @copyright (C) 2013, Martin Fleischer
%%%-------------------------------------------------------------------
-module(noiseling_server).

-behaviour(gen_server).

%% API
-export([start_link/0, get_streamers/0, connect_listener/2, 
	disconnect_listener/2, start_streamer/4, kill_streamer/1, 
	add_listener/1, remove_listener/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
%% streamer record defines an output-stream
-record(streamer, {
			pid,
			connected_listeners = [],
			stream_name,
			lat,
			long,
			desc
		}).
%% state of the server
-record(state, {
			streamers = [],
			listeners = []
		}).

%%%===================================================================
%%% API
%%%===================================================================
%% Returns all streamers of the application bound to Stramers in the tuple {ok, {streamers, Streamers}}.
get_streamers() ->
	{ok, {streamers, Streamers}} = gen_server:call(?MODULE, {get_streamers}),
	{ok, {streamers, Streamers}}.

%% Connects the listener with ListenerPid to the stream with StreamPid. Returns ok.
connect_listener(StreamPid, ListenerPid) -> 
	ok = gen_server:call(?MODULE, {connect_listener, StreamPid, ListenerPid}),
	ok.

%% Disconnects the listener with pid ListenerPid from the stream with pid StreamPid. Returns ok.
disconnect_listener(StreamPid, ListenerPid) ->
	ok = gen_server:call(?MODULE, {disconnect_listener, StreamPid, ListenerPid}),
	ok.

%% Starts a stream with the given Attributs and returns the created stream 
%% Streamer as streamer-Record in the tuple {ok, {streamer, Streamer}}.
start_streamer(StreamName, Lat, Long, Desc) -> 
	{ok, {streamer, Streamer}} = gen_server:call(?MODULE, {start_streamer, StreamName, Lat, Long, Desc}),
	{ok, {listeners, Listeners}} = gen_server:call(?MODULE, {get_all_listeners}),
	send_data_to_listeners({streamer_added_event, Streamer}, Listeners),
	{ok, {streamer, Streamer}}.

%% Ends stream with pid StreamPid.
kill_streamer(StreamPid) -> 
	ok = gen_server:call(?MODULE, {kill_streamer, StreamPid}),
	{ok, {listeners, Listeners}} = gen_server:call(?MODULE, {get_all_listeners}),
	send_data_to_listeners({streamer_removed_event, StreamPid}, Listeners),
	ok.

%% Adds the listener with pid ListenerPid to the server state.
add_listener(ListenerPid) -> 
	ok = gen_server:call(?MODULE, {add_listener, ListenerPid}),
	ok.

%% Removes the listener with pid ListenerPid from the server state.
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
%% Adds a new stream with given attributes to the state.
%% Replys the stream Streamer as streamer-Record in the tuple {ok, {streamer, Streamer}.
handle_call({start_streamer, StreamName, Lat, Long, Desc}, _From, State) ->
	StreamPid = spawn(fun() -> stream_data_loop() end),
	Streamer = #streamer{pid=StreamPid, stream_name=StreamName, lat=Lat, long=Long, desc=Desc},
	NewState = State#state{streamers = State#state.streamers ++ [ Streamer ]},
	{reply, {ok, {streamer, Streamer}}, NewState};

%% Removes the streamer with pid StreamerPid from the state.
%% Replys ok.
handle_call({kill_streamer, StreamerPid}, _From, State) ->
	case lists:keyfind(StreamerPid, 2, State#state.streamers) of
		false -> {reply, ok, State};
		_Streamer ->
			NewStreamers = lists:keydelete(StreamerPid, 2, State#state.streamers),
			NewState = State#state{streamers = NewStreamers},
			StreamerPid ! shutdown,
			{reply, ok, NewState}
	end;
%% Adds the listener with pid ListenerPid to the state.
%% Replys ok.
handle_call({add_listener, ListenerPid}, _From, State) ->
	NewState = State#state{listeners = State#state.listeners ++ [ ListenerPid ]},
	{reply, ok, NewState};
%% Removes the listener with pid ListenerPid from the state.
%% Replys ok.
handle_call({remove_listener, ListenerPid}, _From, State) ->
	NewState = State#state{listeners = State#state.listeners -- [ ListenerPid ]},
	{reply, ok, NewState};
%% Connects the listener with pid ListenerPid to the stream with pid StreamerPid.
%% Replys ok.
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
%% Disconnects the listener with pid ListenerPid from the stream with pid StreamerPid.
%% Replys ok.
handle_call({disconnect_listener, StreamerPid, ListenerPid}, _From, State) ->
    case lists:keyfind(StreamerPid, 2, State#state.streamers) of
        false -> {reply, ok, State};
        Streamer ->
            NewListeners = Streamer#streamer.connected_listeners -- [ListenerPid],
            NewStreamers = lists:keyreplace(StreamerPid, 2, State#state.streamers, Streamer#streamer{connected_listeners=NewListeners}), 
            NewState = State#state{streamers = NewStreamers},
            {reply, ok, NewState}
    end;
%% Returns the list Listeners containing all listeners of the stream with pid StreamerPid.
%% Replys {ok, {listeners, Listeners}.
handle_call({get_connected_listeners, StreamerPid}, _From, State) ->
	case lists:keyfind(StreamerPid, 2, State#state.streamers) of
		false ->
			{reply, {ok, {listeners, []}}, State};
		Streamer ->
			Listeners = Streamer#streamer.connected_listeners,
			{reply, {ok, {listeners, Listeners}}, State}
	end;
%% Returns the list Listeners containing all listeners of the server state.
%% Replys {ok, {listeners, Listeners}}.
handle_call({get_all_listeners}, _From, State) ->
	Listeners = State#state.listeners,
	{reply, {ok, {listeners, Listeners}}, State};
%% Returns the list Streamers containing all streamers of the server state.
%% Replys {ok, {streamers, Streamers}}.
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
%% sends Data to all Prozesses of the list Listeners
send_data_to_listeners(Data, Listeners) ->
	lists:foreach(fun(Pid)-> Pid ! Data end, Listeners).

%% infinite streaming loop takes audio chunks and sends it to the connected listeners of its stream 
%% or a shutdown message for killing itself
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

%% returns the list listeners containing all listeners of the stream with pid StreamPid 
%% in the tuple {ok, {connected_listeners, Listeners}}
get_connected_listeners(StreamPid) ->
	{ok, {listeners, Listeners}} = gen_server:call(?MODULE, {get_connected_listeners, StreamPid}),
	{ok, {connected_listeners, Listeners}}.