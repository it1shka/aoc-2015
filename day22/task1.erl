-module(task1).
-export([solve/0]).

%% Definitions:

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

%% Type definitions:

-spec accumulator(MinSpent :: integer()) -> none().
-spec effect_cost(Effect :: atom()) -> integer().
-spec effect_duration(Effect :: atom()) -> integer().
-spec available_effects(Mana :: integer(), ExistingEffects :: [atom()]) -> [atom()].
-spec 
  battle (
    AccPID :: pid(), 
    State :: #battle_state{}, 
    Spent :: integer()
  ) -> none().
-spec solve() -> none().

%% Functions:

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
      io:format("Accumulator timeouted, min value is ~p\n", [MinSpent])
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
      io:format("Unknown effect: ~s\n", [EffectName]),
      infinity
  end.

effect_duration(Effect) ->
  case Effect of
    shield -> 6;
    poison -> 6;
    recharge -> 5;
    IllegalEffect ->
      EffectName = atom_to_list(IllegalEffect),
      io:format("Unknown effect: ~s\n", [EffectName]),
      infinity
  end.

available_effects(Mana, ExistingEffects) ->
  lists:filter(fun (Effect) ->
    Cheap = effect_cost(Effect) =< Mana,
    Enabled = lists:member(Effect, ExistingEffects),
    Cheap and not Enabled
  end, ?EFFECTS).

battle(_, State, _)
  when State#battle_state.player_hp =< 0 ->
  battle_lost;
battle(AccPID, State, Spent)
  when State#battle_state.enemy_hp =< 0 ->
  AccPID ! Spent;
battle(AccPID, State, Spent) ->
  EffectAtoms = lists:map(fun({ Atom, _ }) -> 
    Atom 
  end, State#battle_state.effects),
  AffectedState = lists:foldl(fun(Effect, PrevState) ->
    case Effect of
      poison -> PrevState#battle_state{
        enemy_hp = PrevState#battle_state.player_hp - 6
      };
      recharge -> PrevState#battle_state{
        mana = PrevState#battle_state.mana + 101
      };
      _ -> PrevState
    end
  end, State, EffectAtoms),
  PastState = AffectedState#battle_state{
    effects = lists:filtermap(fun({Atom, Count}) ->
      NewCount = Count - 1,
      if 
        NewCount =< 0 ->
          false;
        true ->
          {true, {Atom, NewCount}}
      end
    end, AffectedState#battle_state.effects)
  },
  if
    PastState#battle_state.enemy_hp =< 0 ->
      AccPID ! Spent;
    PastState#battle_state.turn =:= player ->
      PastEffects = lists:map(fun({ Atom, _ }) ->
        Atom
      end, PastState#battle_state.effects),
      AvailableEffects = available_effects(PastState#battle_state.mana, PastEffects),
      lists:foreach(fun(Effect) ->
        NextState = 
          case Effect of
            missile -> 
              NextEnemyHP = PastState#battle_state.enemy_hp - 4,
              PastState#battle_state{ enemy_hp = NextEnemyHP };
            drain -> 
              NextEnemyHP = PastState#battle_state.enemy_hp - 2,
              NextPlayerHP = PastState#battle_state.player_hp + 2,
              PastState#battle_state{ 
                enemy_hp = NextEnemyHP, 
                player_hp = NextPlayerHP 
              };
            LongEffect ->
              NextEffects = lists:append(
                PastState#battle_state.effects,
                [{LongEffect, effect_duration(LongEffect)}]
              ),
              PastState#battle_state{ effects = NextEffects }
          end,
        spawn(fun() ->
          battle(AccPID, NextState#battle_state{
            turn = enemy
          }, Spent + effect_cost(Effect))
        end)
      end, AvailableEffects);
    true ->
      NextPlayerHP = 
        case lists:member(shield, EffectAtoms) of
          true ->
            Damage = max(1, ?ENEMY_DAMAGE - 7),
            PastState#battle_state.player_hp - Damage;
          _ ->
            PastState#battle_state.player_hp - ?ENEMY_DAMAGE
        end,
      NextState = PastState#battle_state{
        turn = player,
        player_hp = NextPlayerHP
      },
      spawn(fun() ->
        battle(AccPID, NextState, Spent)
      end)
  end,
  finished.

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
