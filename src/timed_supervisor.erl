%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc A supervisor implementing gen_timed_server behavior.
%%%
%%% @author  Serge Aleynikov <saleyn@gmail.com>
%%% @version $Revision$
%%%          $Date$
%%% @end
%%%------------------------------------------------------------------------
%%% Created 2014-0605 Serge Aleynikov <saleyn@gmail.com>
%%%------------------------------------------------------------------------
-module(timed_supervisor).
-author('saleyn@gmail.com').

-behaviour(gen_timed_server).

%% External exports
-export([
    start_link/3, start_link/4
]).

%% Internal exports
-export([
      init/1, handle_call/4, handle_cast/3, handle_info/3
    , code_change/3, terminate/2
    , handle_start/2, handle_run/1, handle_stop/4
]).

-record(state, {
      name
    , mfa
}).

%%-------------------------------------------------------------------------
%% @doc Creates a server process that will execute the MFA
%%      callback according to the given `schedule' option.
%% @see //stdlib/supervisor
%% @end
%%-------------------------------------------------------------------------
-spec start_link(atom(), {atom(), atom(), list()},
    gen_timed_server:sup_options(), list()) ->
        {ok, pid()} | ignore | {error, any()}.
start_link(SupName, ChildMFA, SupOpts, DbgOpts)
  when is_atom(SupName), tuple_size(ChildMFA) =:= 3
     , is_list(SupOpts), is_list(DbgOpts) ->
    gen_timed_server:start_link(
        {local, SupName}, ?MODULE, [ChildMFA],
        [no_spawn, {monitor_type, child_link} | filter_sup_opts(SupOpts)],
        DbgOpts).

%%-------------------------------------------------------------------------
%% @spec (ChildMFA::mfa(), SupOpts::sup_options()) ->
%%              {ok, Pid::pid()} | ignore | {error, Error}
%% @doc Creates a timed supervisor as part of supervision tree without
%%      a registered name.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start_link(ChildMFA, SupOpts, DbgOpts)
  when tuple_size(ChildMFA) =:= 3
     , is_list(SupOpts), is_list(DbgOpts) ->
    gen_timed_server:start_link(
        ?MODULE, ChildMFA,
        [no_spawn, {monitor_type, child_link} | filter_sup_opts(SupOpts)],
        DbgOpts).

%%-------------------------------------------------------------------------
%% Callback functions from gen_timed_server
%%-------------------------------------------------------------------------

%% @private
init([MFA]) ->
    {ok, #state{mfa=MFA}}.

%% @private
handle_call(_Req, _From, State, _Opaque) ->
    {reply, {error, not_implemented}, State}.

%% @private
handle_cast(Req, State, _Opaque) ->
    {stop, {unhandled_cast, Req}, State}.

handle_info(Msg, State, _Opaque) ->
    error_logger:warning_msg("Unhandled msg: ~p\n", [Msg]),
    {noreply, State}.

handle_start(#state{} = State, _Opaque) ->
    % This callback is executed just before a call to handle_run/2.
    % Since this supervisor has `no_spawn' option, the process will be
    % spawned in the call to `handle_run/2'
    {start, State}.

handle_run(#state{mfa={M,F,A}}) ->
    % This call must spawn a child process linked to self().
    erlang:apply(M,F,A).

handle_stop(Action, Reason, #state{} = State, _Opaque) ->
    % This callback is executed after the child process is
    % exited by `stop_child' call.
    {Action, Reason, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

filter_sup_opts(SupOpts) ->
    lists:filter(fun
        (no_spawn) -> false;
        ({I,_})    -> not lists:member(I, [spawn, monitor_type]);
        (_Other)   -> true
    end, SupOpts).
