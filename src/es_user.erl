-module(es_user).

-include("es.hrl").

-export([get_id/1, new/1, set_name/2, set_surname/2, set_age/2, is_complete/1]).

-export_type([id/0]).

%%% API

get_id(User) ->
    User#es_user.id.

new([]) ->
    new();
new(Params) ->
    lists:foldl(
      fun(Field, User) ->
              Fun = list_to_existing_atom("set_" ++ atom_to_list(Field)),
              ?MODULE:Fun(proplists:get_value(Field, Params), User)
      end, new(), proplists:get_keys(Params)).

set_name(_, #es_user{id = undefined}) -> no_id_error();
set_name(Name, User) when is_list(Name) orelse is_binary(Name) ->
    maybe_set_complete(User#es_user{name = Name}).

set_surname(_, #es_user{id = undefined}) -> no_id_error();
set_surname(Surname, User) when is_list(Surname) orelse is_binary(Surname) ->
    maybe_set_complete(User#es_user{surname = Surname}).

set_age(_, #es_user{id = undefined}) -> no_id_error();
set_age(Age, User) when Age >= 0 ->
    maybe_set_complete(User#es_user{age = Age}).

is_complete(#es_user{id = undefined}) -> no_id_error();
is_complete(User) ->
    User#es_user.is_complete.


%%% Helpers

new() ->
    #es_user{id = es_utils:uuid()}.

maybe_set_complete(#es_user{is_complete = true} = U) ->  U;
maybe_set_complete(#es_user{is_complete = false} = U) ->
    IsComplete = not lists:any(fun(Elem) ->
                                       element(Elem, U) == undefined
                               end, lists:seq(1, record_info(size, es_user))),
    U#es_user{is_complete = IsComplete}.

no_id_error() ->
    error({badarg, no_id}).
