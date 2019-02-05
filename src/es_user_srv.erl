-module(es_user_srv).

-behaviour(gen_server).

-include("es.hrl").

-export([start/0, start_link/0, stop/1]).
-export([new/3, new/2, update/3, get_incomplete/1, get_complete/1]).
-export([init/1, terminate/2, handle_call/3]).

-define(RAM_DB, users).
-define(DISK_DB_NAME, users).
-define(DISK_DB_FILE, "users.dat").

%%% Server Lifecycle API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

%%% Server API

-spec new(pid(), [proplists:property()], on_comleted_callback()) -> {ok, es_user:id()}.
new(Pid, Params, OnCompleted) ->
    gen_server:call(Pid, {create_user, Params, OnCompleted}).

-spec new(pid(), [proplists:property()]) -> {ok, es_user:id()}.
new(Pid, Params) ->
    new(Pid, Params, fun(_) -> ok end).

-spec update(pid(), es_user:id(), [proplists:property()]) -> R when
      R :: {ok, [atom()]}
         | {error, already_completed}
         | {error, not_exists}.
update(Pid, UserId, Params) ->
    gen_server:call(Pid, {update_user, UserId, Params}).

-spec get_incomplete(pid()) -> {ok, [es_user:id()]}.
get_incomplete(Pid) ->
    gen_server:call(Pid, get_incomplete).

-spec get_complete(pid()) -> {ok, [es_user:id()]}.
get_complete(Pid) ->
    gen_server:call(Pid, get_complete).

%%% GEN_SERVER CALLBACKS

init([]) ->
    subscribe_to_user_events(es_user_srv_events_handler, self()),
    RamDb = init_ram_store(?RAM_DB),
    DiskDb = init_disk_store(?DISK_DB_NAME, ?DISK_DB_FILE),
    {ok, {RamDb, DiskDb, #{}}}.

terminate(_, {_, DiskDb, #{}}) ->
    unsubscribe_from_user_events(es_user_srv_events_handler),
    close_disk_store(DiskDb).

handle_call({create_user, Params, OnCompleted},
            _From,
            {RamDb, DiskDb, OnCompletedCallbacks0} = State) ->
    User = handle_create_user(Params, RamDb),
    maybe_persist_user(User, RamDb, DiskDb),
    OnCompletedCallbacks =
        maybe_complete(User, OnCompletedCallbacks0#{User#es_user.id => OnCompleted}),
    {reply, {ok, User#es_user.id}, setelement(3, State, OnCompletedCallbacks)};
handle_call({update_user, UserId, Params},
            _From,
            {RamDb, DiskDb, OnCompletedCallbacks0} = State) ->
    case handle_update_user(UserId, Params, RamDb) of
        already_completed = Err ->
            {reply, {error, Err}, State};
        not_exists = Err ->
            {reply, {error, Err}, State};
        {User, UpdatedFields} ->
            maybe_persist_user(User, RamDb, DiskDb),
            OnCompletedCallbacks = maybe_complete(User, OnCompletedCallbacks0),
            {reply, {ok, UpdatedFields}, setelement(3, State, OnCompletedCallbacks)}
    end;
handle_call(get_incomplete, _From, {RamDb, _, _} = State) ->
    Incomplete = handle_get_incomplete(RamDb),
    {reply, {ok, Incomplete}, State};
handle_call(get_complete, _From, {_, DiskDb, _} = State) ->
    Complete = handle_get_complete(DiskDb),
    {reply, {ok, Complete}, State}.

%%% HELPERS

init_ram_store(Tab) ->
    ets:new(Tab, [named_table, {keypos, #es_user.id}]).

init_disk_store(Tab, File) ->
    {ok, Name} = dets:open_file(Tab, [{file, File}, {keypos, #es_user.id}]),
    Name.

close_disk_store(Tab) ->
    dets:close(Tab).

handle_create_user(Params, Tab) ->
    User = es_user:new(Params),
    true = ets:insert(Tab, User),
    User.

handle_update_user(UserId, Params, Tab) ->
    [User] = ets:lookup(Tab, UserId),
    {UpdatedUser, UpdatedFields} =
        lists:foldl(fun(Param, {Usr, Updates}) ->
                            maybe_update_user_field(Param, Usr, Updates)
                    end, {User, []}, Params),
    true = ets:insert(Tab, UpdatedUser),
    {UpdatedUser, UpdatedFields}.

handle_get_incomplete(Tab) ->
    Matches = ets:match(Tab, #es_user{id = '$1', is_complete = false,  _ = '_'}),
    lists:flatten(Matches).

handle_get_complete(Tab) ->
    Matches = dets:match(Tab, #es_user{id = '$1', _ = '_'}),
    lists:flatten(Matches).

maybe_persist_user(User, RamTab, DiskTab) when User#es_user.is_complete ->
    ok = dets:insert(DiskTab, User),
    true = ets:delete(RamTab, User#es_user.id),
    persisted;
maybe_persist_user(_, _, _) ->
    incomplete_user.

maybe_complete(User, OnCompletedCallbacks0) when User#es_user.is_complete ->
    UserId = User#es_user.id,
    {OnCompleted, OnCompletedCallbacks} = maps:take(UserId, OnCompletedCallbacks0),
    OnCompleted(UserId),
    OnCompletedCallbacks;
maybe_complete(_, Callbacks) ->
    Callbacks.

maybe_update_user_field({Field, Value}, User, UpdatedFields) ->
    SetFun = list_to_existing_atom("set_" ++ atom_to_list(Field)),
    case erlang:function_exported(es_user, SetFun, 2) of
        true ->
            {es_user:SetFun(Value, User), [Field | UpdatedFields]};
        false ->
            {User, UpdatedFields}
    end.

subscribe_to_user_events(UserEventsHandler, UserServerPid) ->
    ok = es_user_events_manager:subscribe(UserEventsHandler, UserServerPid).

unsubscribe_from_user_events(UserEventsHandler) ->
    ok = es_user_events_manager:unsubscribe(UserEventsHandler, []).
