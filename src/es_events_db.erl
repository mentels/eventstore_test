-module(es_events_db).

-behaviour(gen_server).

-include_lib("erles/include/erles.hrl").

-define(NAME, ?MODULE).

-export([start/0, start_link/0, stop/0]).
-export([store/3, read_backward_from_last/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-record(state, {
                conn :: pid(),
                exp_vers = #{} ::
                  #{Stream :: binary() | string() => Ver :: non_neg_integer()}
               }).

%% API

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?NAME}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?NAME).


store(Stream, Event, Type) when is_atom(Type) ->
    store(Stream, Event, atom_to_binary(Type, utf8));
store(Stream, Event, Type) when is_atom(Stream) ->
    store(atom_to_binary(Stream, utf8), Event, Type);
store(Stream, Event, Type) when is_binary(Stream) and is_binary(Type) ->
    gen_server:call(?NAME, {store, Stream, Event, Type}).

read_backward_from_last(Stream, MaxCount) ->
    gen_server:call(?NAME, {read_backward, Stream, last, MaxCount}).

%% Callbacks

init([]) ->
    {ok, #state{conn = connect("localhost", 1113)}}.

terminate(_, #state{conn = Pid}) ->
    ok = erles:close(Pid).

handle_call({store, Stream, Event, Type},
            _From,
            #state{conn = Conn, exp_vers = ExpVers} = State) ->
    {ok, NewExpVer} =
        append(
          Conn, Stream, maps:get(Stream, ExpVers, any), format_event(Event, Type)),
    {reply, ok, State#state{exp_vers = #{Stream => NewExpVer}}};
handle_call({read_backward, Stream, FromVer, MaxCount},
            From,
            #state{conn = Conn} = State) ->
    {reply, read_backward(Conn, Stream, FromVer, MaxCount), State}.


handle_cast(_, _) ->
    error(not_implemented).

handle_info(_, _) ->
    error(not_implemented).

code_change(_, _, _) ->
    error(not_implemented).

%% Helpers

connect(Host, Port) ->
    {ok, Pid} = erles:connect(node, {Host, Port}),
    Pid.

append(Conn, Stream, ExpVer, FormattedEvent) ->
    erles:append(Conn, Stream, ExpVer, [FormattedEvent]).

read_backward(Conn, Stream, From, MaxCount) ->
    erles:read_stream(Conn, Stream, From, MaxCount, backward).

format_event(Event, Type) ->
    #event_data{
       event_type = Type,
       data = term_to_binary(Event)
      }.
