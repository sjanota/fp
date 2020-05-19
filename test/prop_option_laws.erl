-module(prop_option_laws).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_map_composition() ->
  ?FORALL(F1, function1(any()),
    ?FORALL(F2, function1(any()),
      ?FORALL(X, any(),
        (fp:compose(option:map(F1), option:map(F2)))({some, X}) == option:map(fp:compose(F1, F2), {some, X})
      )
    )
  ).

prop_map_identity() ->
  ?FORALL(X, any(),
    option:map(fun fp:identity/1, option:new(X)) == option:new(X)
  ).
