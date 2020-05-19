-module(option_tests).

-include_lib("eunit/include/eunit.hrl").

-import(option, [map/2, get_or_else/2]).

map_test_() -> [
  {"maps over some", ?_assert(map(fun(X) -> X*2 end, {some, 2}) == {some, 4})},
  {"ignores none", ?_assert(map(fun(X) -> X*2 end,none) == none)}
].

get_or_else_test_() -> [
  {"gets from some", ?_assert(get_or_else(3, {some, 2}) == 2)},
  {"defaults on none", ?_assert(get_or_else(3, none) == 3)}
].
