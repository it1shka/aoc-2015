-module(task2).
-export([main/0]).

-spec basement(binary(), integer(), integer()) -> integer().
basement(<<"(", Tail/binary>>, Floor, Position) ->
  basement(Tail, Floor + 1, Position + 1);
basement(<<")", Tail/binary>>, Floor, Position) 
  when Floor > 0 -> basement(Tail, Floor - 1, Position + 1);
basement(<<")", _/binary>>, Floor, Position) 
  when Floor =:= 0 -> Position.

-spec main() -> none().
main() -> 
  {ok, Binary} = file:read_file("input.txt"),
  Floor = basement(Binary, 0, 1),
  io:format("The position is ~p.\n", [Floor]).
