-module(option).

%% API
-export([new/1, map/1, get_or_else/1, or_else/1, flat_map/1, lift/1, map2/2, traverse/2, sequence/1]).

-type option(A) :: {some, A} | none.
-type op(A, B) :: fun((option(A)) -> B).

-import(fp, [compose/2]).
-compile({parse_transform, uncurry_pt}).

-uncurry([map/1, get_or_else/1, or_else/1, flat_map/1, map2/2]).


-spec new(A) -> option(A).
new(undefined) -> none;
new(A) -> {some, A}.

-spec map(fun((A) -> B)) -> op(A, option(B)).
map(F) -> fun
  ({some, A}) -> {some, F(A)};
  (none) -> none
end.

-spec get_or_else(Else :: A) -> op(A, A).
get_or_else(Else) -> fun
  ({some, A}) -> A;
  (none) -> Else
end.

-spec or_else(Else :: option(A)) -> op(A, option(A)).
or_else(Else) -> compose(get_or_else(Else), map(fun(O) -> new(O) end)).

-spec flat_map(fun((A) -> option(B))) -> op(A, option(B)).
flat_map(F) -> compose(get_or_else(none), map(F)).

-spec lift(fun((A) -> B)) -> op(A, option(B)).
lift(F) -> map(F).

-spec map2(fun((A, B) -> C), option(B)) -> op(A, option(C)).
map2(F, Other) -> flat_map(fun(A) -> map(fun(B) -> F(A, B) end, Other) end).

-spec traverse(fun((A) -> option(B)), list(A)) -> option(list(B)).
traverse(F, List) ->
  lists:foldr(fun(A, Acc) ->
    map2(fun(OptionB, L) -> [OptionB | L] end, Acc, F(A))
  end, {some, []}, List).

-spec sequence(list(option(A))) -> option(list(A)).
sequence(List) ->
  traverse(fun fp:identity/1, List).