-module(fp).

-export([compose/2, partial/2, curry/1, identity/1, uncurry/1]).

compose(F1, F2) -> fun(X) -> F1(F2(X)) end.

identity(X) -> X.

partial(F, AppliedArgs) ->
  {arity, Arity} = erlang:fun_info(F, arity),
  fun_of_arity(
    fun(Args) -> erlang:apply(F, AppliedArgs ++ Args) end,
    Arity - length(AppliedArgs)
  ).

curry(F) ->
  curry_next(F, 0, []).

uncurry(F) ->
  {arity, Arity} = erlang:fun_info(F, arity),
  fun_of_arity(
    fun(Args) -> (F(lists:droplast(Args)))(lists:last(Args)) end,
    Arity + 1
  ).


curry_next(F, Count, Applied) -> fun(Arg) ->
  case erlang:fun_info(F, arity) of
    {arity, Arity} when Count + 1 == Arity ->
      erlang:apply(F, Applied ++ [Arg]);
    _ ->
      curry_next(F, Count + 1, Applied ++ [Arg])
  end
end.

fun_of_arity(Call, 1) -> fun(X) -> Call([X]) end;
fun_of_arity(Call, 2) -> fun(X1, X2) -> Call([X1, X2]) end;
fun_of_arity(Call, 2) -> fun(X1, X2, X3) -> Call([X1, X2, X3]) end;
fun_of_arity(Call, 2) -> fun(X1, X2, X3, X4) -> Call([X1, X2, X3, X4]) end.
