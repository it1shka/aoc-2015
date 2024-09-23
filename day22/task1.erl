-module(task1).
-export([solve/0]).

-define(ENEMY_DAMAGE, 9).
-define(EFFECTS, [
  missile,
  drain,
  shield,
  poison,
  recharge
]).
-define(TIMEOUT, 10000).
-record(battle_state, {
  turn :: player | enemy,
  player_hp :: integer(),
  enemy_hp :: integer(),
  mana :: integer(),
  effects :: [{ atom(), integer() }]
}).

-spec accumulator(MinSpent :: integer()) -> none().
-spec effect_cost(Effect :: atom()) -> integer().
-spec available_effects(Mana :: integer()) -> [atom()].
-spec 
  battle (
    AccPID :: pid(), 
    State :: #battle_state{}, 
    Spent :: integer()
  ) -> none().
-spec solve() -> none().

accumulator(MinSpent) ->
  receive 
    Spent ->
      accumulator(if
        MinSpent > Spent ->
          io:format("Min spent: ~p\n", [Spent]),
          Spent;
        true ->
          MinSpent
      end)
  after 
    ?TIMEOUT ->
      io:write("Accumulator timeouted\n")
  end.

effect_cost(Effect) ->
  case Effect of
    missile -> 53;
    drain -> 73;
    shield -> 113;
    poison -> 173;
    recharge -> 229;
    IllegalEffect ->
      EffectName = atom_to_list(IllegalEffect),
      io:format("Unknown effect: ~s\n", [EffectName])
  end.

available_effects(Mana) ->
  lists:filter(fun (Effect) ->
    effect_cost(Effect) =< Mana
  end, ?EFFECTS).

battle(_, State, _)
  when State#battle_state.player_hp =< 0 ->
  battle_lost;
battle(AccPID, State, Spent)
  when State#battle_state.enemy_hp =< 0 ->
  AccPID ! Spent;
battle(AccPID, State, Spent) ->
  todo.

solve() ->
  AccPID = spawn(fun() ->
    accumulator(infinity)
  end),
  InitialState = 
    #battle_state{
      turn = player,
      player_hp = 50,
      enemy_hp = 51,
      mana = 500,
      effects = []
    },
  battle(AccPID, InitialState, 0).
