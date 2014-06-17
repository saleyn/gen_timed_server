%%%----------------------------------------------------------------------------
%%% @doc 
%%% @author  <saleyn@gmail.com>
%%% @copyright 2014 
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2014-06-06
%%%----------------------------------------------------------------------------
-module(test_app).
-author('saleyn@gmail.com').

-behaviour(application).
-behaviour(supervisor).

%% API
-export([]).

%% Application callbacks
-export([start/0, start/2, stop/1, config_change/3]).

%% Supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

start() ->
    application:start(test_app).

%%%----------------------------------------------------------------------------
%%% Application callbacks
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @spec (StartType, StartArgs) -> 
%%              {ok, Pid} | {ok, Pid, State} | {error, Reason}
%%          StartType = normal | {takeover,Node} | {failover,Node}
%% @doc Called by application behavior to start application by creating 
%%      top-level supervisor. `StartArgs' are taken from `{mod, StartArgs}' 
%%      option in the `*.app' file.
%% @see application
%% @end
%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    application:start(exec),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%-----------------------------------------------------------------------------
%% @private
%% @spec (State) -> void()
%% @doc Called by application behavior to stop application.
%% @see application
%% @end
%%-----------------------------------------------------------------------------
stop(_State) ->
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Changed, New, Removed) -> ok
%%          Changed = [{atom(),Value}]
%%          New     = [{atom(),Value}]
%%          Removed = [atom()]
%% @doc Called by application behavior on change in application's environment.
%% @see application
%% @end
%%-----------------------------------------------------------------------------
config_change(_Changed, _New, _Removed) ->
    ok.

%%%----------------------------------------------------------------------------
%%% Supervisor callbacks
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Args) -> {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% @see supervisor:init/1
%% @end
%%-----------------------------------------------------------------------------
init([]) ->
    SupOpt = application:get_env(test_app, sup, []),
    Exec   = application:get_env(test_app, cmd, "sleep 2"),
    CmdOpt = application:get_env(test_app, cmd_opts, []),
    CmdSup = application:get_env(test_app, cmd_sup,  []),
    DbgOpt = application:get_env(test_app, dbg, []),
    SrvOpt = application:get_env(test_app, srv, []),
    RestartStrategy = {one_for_one, 0, 1},

    SupSpec = {
          test_sup                      % ID
        , {timed_supervisor, start_link,% Start function {M,F,A}
            [test_sup, {test_srv, start_link, SrvOpt}, SupOpt, DbgOpt]
          }
        , permanent                     % Restart =
                                        %   permanent | transient | temporary
        , 5000                          % Shutdown =
                                        %   brutal_kill | int()>=0 | infinity
        , worker                        % Type = worker | supervisor
        , [test_app]                    % Modules = [Module] | dynamic
    },

    CmdSpec = {
          test_os                       % ID
        , {timed_supervisor, start_link,% Start function {M,F,A}
            [test_os, {test_srv, start_oscmd, [Exec, CmdOpt]}, CmdSup, DbgOpt]
          }
        , permanent                     % Restart =
                                        %   permanent | transient | temporary
        , 5000                          % Shutdown =
                                        %   brutal_kill | int()>=0 | infinity
        , worker                        % Type = worker | supervisor
        , [test_srv]                    % Modules = [Module] | dynamic
    },

    {ok, {RestartStrategy, [SupSpec, CmdSpec]}}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

