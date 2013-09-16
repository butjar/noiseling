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

-record(state, {}).

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
    {ok, #state{}}.

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
handle_call({register_streamer, Pid}, From, State) ->
    io:format("~nStreamer has been registered:[~p]~n",[Pid]),
    StreamPid = spawn(fun() -> stream_data_loop() end),
    global:register_name('stream', StreamPid),
    io:format("~nStream started~n"),
    {reply, ok, State};
handle_call({register_listner, Pid}, From, State) ->
    global:register_name(new_listner(Pid), Pid),
    io:format("~nListner has been registered:[~p]~n",[Pid]),
    {reply, ok, State}.


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

send_to_connected_listners(Data) ->
    lists:foreach(fun(Name)-> global:send(Name, Data) end, connected_listners()).

connected_listners() ->
    lists:filter(fun(Elem) -> 
        case re:run(atom_to_list(Elem), "listner_*") of 
            {match, _} -> true;
            nomatch -> false
        end
    end, global:registered_names()).

register_streamer(Pid) -> gen_server:call(?MODULE, {register_streamer, Pid}).

register_listner(Pid) -> gen_server:call(?MODULE, {register_listner, Pid}).

stream_data_loop() ->
    receive
        Data ->
            send_to_connected_listners(Data)
    end,
    stream_data_loop().

new_listner(Pid) ->
    list_to_atom(string:concat("listner_", pid_to_list(Pid))).
