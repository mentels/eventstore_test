%%%-------------------------------------------------------------------
%% @doc es top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(es_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
                #{id => es_user_events_manager,
                  start => {es_user_events_manager, start_link, []}},
                #{id => es_user_srv,
                  start => {es_user_srv, start_link, []}},
                #{id => es_events_db,
                  start => {es_events_db, start_link, []}},
                #{id => es_api_srv,
                  start => {es_api_srv, start_link, []}}
               ],
    {ok, { {rest_for_one, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
