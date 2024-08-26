-module(task1).
-export([main/0]).

-spec powerset(list(T)) -> list(list(T)).
powerset([Head | Tail]) ->
  Without = powerset(Tail),
  With = lists:map(fun (Elem) -> [Head | Elem] end, Without),
  Without ++ With;
powerset([]) -> [[]].

-spec solution(integer(), list(integer())) -> integer().
solution(Liters, Containers) ->
  S = powerset(Containers),
  Sums = lists:map(fun lists:sum/1, S),
  Proper = lists:filter(fun (X) -> X =:= Liters end, Sums),
  length(Proper).

-spec read_containers() -> list(integer()).
read_containers() ->
  {ok, Binary} = file:read_file("input.txt"),
  Parts = binary:split(string:trim(Binary), <<"\n">>, [global]),
  lists:map(fun binary_to_integer/1, Parts).

-spec main() -> none().
main() ->
  {ok, Liters} = io:read("Liters: "),
  Containers = read_containers(),
  Answer = solution(Liters, Containers),
  io:format("Answer is: ~p\n", [Answer]).
