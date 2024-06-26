-module(task2).
-export([main/0]).

count(Line) -> 
  Parts = string:split(Line, "x", all),
  Ints = lists:map(fun (X) -> 
    {Int, _} = string:to_integer(X),
    Int
  end, Parts),
  [L, W, H] = Ints,
  Perimeters = [2*(L+W), 2*(W+H), 2*(L+H)],
  lists:min(Perimeters) + (L * W * H).

paper(Device, Acc) -> 
  case io:get_line(Device, "") of
    eof -> Acc;
    Line -> paper(Device, Acc + count(Line))
  end.

-spec main() -> none().
main() ->
  {ok, Device} = file:open("input.txt", [read]),
  Answer = paper(Device, 0),
  io:format("Answer is ~p\n", [Answer]).
