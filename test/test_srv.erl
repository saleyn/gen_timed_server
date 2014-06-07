%%%----------------------------------------------------------------------------
%%% @doc 
%%% @author  <saleyn@gmail.com>
%%% @copyright 2014 
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2014-06-06
%%%----------------------------------------------------------------------------
-module(test_srv).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
}).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @spec () -> {ok, Pid} | ignore | {error, Reason} 
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns `{error,Reason}'.
%%      If init/1 returns `{stop,Reason}' or ignore, the process is
%%      terminated and the function returns `{error,Reason}' or ignore,
%%      respectively.
%% @end
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @spec () -> {ok, Pid} | {error, Reason} 
%% @doc Start the server outside of supervision tree.
%% @end
%%-----------------------------------------------------------------------------
start() ->
    gen_server:start_link(?MODULE, [], []).

%%%----------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Args) -> {ok, State} | {ok, State, Timeout} |
%%                 ignore | {stop, Reason}
%% @doc Initiates the server
%% @end
%%-----------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    try
        io:format("Process ~p started!\n", [self()]),
        {ok, #state{}}
    catch _:What ->
        {stop, What}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Request, From, State) ->
%%            {reply, Reply, State} |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State} |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, Reply, State} |
%%            {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%-----------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Msg, State) -> {noreply, State} |
%%                       {noreply, State, Timeout} |
%%                       {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%-----------------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Info, State) -> {noreply, State} |
%%                        {noreply, State, Timeout} |
%%                        {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%-----------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
terminate(Reason, #state{}) ->
    io:format("Process ~p ended (~p)!\n", [self(), Reason]),
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

