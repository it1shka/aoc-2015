-module(task1).
-export([main/0]).

-spec get_floor(binary(), integer()) -> integer().
get_floor(<<"(", Tail/binary>>, Floor) ->
  get_floor(Tail, Floor + 1);
get_floor(<<")", Tail/binary>>, Floor) ->
  get_floor(Tail, Floor - 1);
get_floor(<<_>>, Floor) -> Floor.

-spec main() -> none().
main() -> 
  {ok, Binary} = file:read_file("input.txt"),
  Floor = get_floor(Binary, 0),
  io:format("The floor is ~p.\n", [Floor]).
