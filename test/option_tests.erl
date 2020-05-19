-module(option_tests).

-include_lib("eunit/include/eunit.hrl").

map_test_() -> [
  {"maps over some", ?_assert(option:map(fun(X) -> X * 2 end, {some, 2}) == {some, 4})},
  {"ignores none", ?_assert(option:map(fun(X) -> X * 2 end, none) == none)}
].

get_or_else_test_() -> [
  {"gets from some", ?_assert(option:get_or_else(3, {some, 2}) == 2)},
  {"defaults on none", ?_assert(option:get_or_else(3, none) == 3)}
].

or_else_test_() -> [
  {"gets from some", ?_assert(option:or_else({some, 3}, {some, 2}) == {some, 2})},
  {"defaults on none", ?_assert(option:or_else({some, 3}, none) == {some, 3})},
  {"passes none through none", ?_assert(option:or_else(none, none) == none)}
].
