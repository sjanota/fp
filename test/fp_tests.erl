-module(fp_tests).

-include_lib("eunit/include/eunit.hrl").

compose_test() ->
  F1 = fun(X) -> X * 2 end,
  F2 = fun(X) -> X + 2 end,
  ?assert((fp:compose(F1, F2))(2) == 8),
  ?assert((fp:compose(F2, F1))(2) == 6).

partial_test_() -> [
  {"two arguments", fun() ->
    F = fun(X, Y) -> X + Y end,
    Partial = fp:partial(F, [1]),
    ?assert(Partial(2) == 3)
  end},
  {"leave out only last argument", fun() ->
    F = fun(X, Y, Z) -> X + Y + Z end,
    Partial = fp:partial(F, [1, 2]),
    ?assert(Partial(3) == 6)
  end},
  {"fill in only first argument", fun() ->
    F = fun(X, Y, Z) -> X + Y + Z end,
    Partial = fp:partial(F, [1]),
    ?assert(Partial(2, 3) == 6)
  end}
].
