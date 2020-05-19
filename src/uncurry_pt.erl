-module(uncurry_pt).

-export([parse_transform/2]).

parse_transform(Ast, _Opt) ->
  case lists:search(is_attribute(uncurry), Ast) of
    false -> Ast;
    {value, {_, _, _, ToUncurry}} ->
      {value, {attribute, _, _, Exports}} = lists:search(is_attribute(export), Ast),
      {Functions, NewExports} = process(ToUncurry, [], Exports),
      WithFunctions = lists:droplast(Ast) ++ Functions ++ [lists:last(Ast)],
      lists:map(fun
        ({attribute, Line, export, _}) -> {attribute, Line, export, NewExports};
        (Other) -> Other
      end, WithFunctions)
  end.

is_attribute(Name) -> fun({attribute, _, AName, _}) -> Name == AName end.

process([], Functions, Exports) -> {Functions, Exports};
process([{Name, Arity} = F | T], Functions, Exports) ->
  process(T, [generate_function(F) | Functions], [{Name, Arity + 1} | Exports]).

generate_function({Name, Arity}) ->
  Variables = lists:map(fun(N) -> {var, 100, list_to_atom("X" ++ integer_to_list(N))} end, lists:seq(0, Arity)),
  {function, 100, Name, Arity + 1, [
    {clause, 100, Variables, [], [
      {call, 100, {call, 100, {atom, 100, Name}, lists:droplast(Variables)}, [lists:last(Variables)]}]
    }]
  }.
