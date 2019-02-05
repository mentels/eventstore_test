-type id() :: binary().

-type on_comleted_callback() :: fun((UserId :: id()) -> ok).

-record(es_user, {
                  id = erlang:error({required, id}) :: id(),
                  name :: string() | binary(),
                  surname :: string() | binary(),
                  age :: non_neg_integer(),
                  is_complete = false :: boolean()
                 }).
