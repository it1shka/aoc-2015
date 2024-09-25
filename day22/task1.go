package main

import (
	"errors"
	"fmt"
	"sync"
)

const (
  PoisonDamage = 3
  RechargeMana = 101
  ShieldProtection = 7
  MagicMissileDamage = 4
  DrainDamage = 2
  DrainHeal = 2
)

type Turn bool
const (
  Player Turn = true
  Enemy Turn = false
)

type Effect int
const (
  Shield Effect = iota
  Poison
  Recharge
  MagicMissile
  Drain
)

func Effects() []Effect {
  return []Effect{
    Shield,
    Poison,
    Recharge,
    MagicMissile,
    Drain,
  }
}

func (e Effect) Price() (int, error) {
  switch e {
  case MagicMissile:
    return 53, nil
  case Drain:
    return 73, nil
  case Shield:
    return 113, nil
  case Poison:
    return 173, nil
  case Recharge:
    return 229, nil
  default:
    return 0, errors.New("Unknown effect")
  }
}

func (e Effect) Duration() (int, error) {
  switch e {
  case Recharge:
    return 5, nil
  case Shield:
    return 6, nil
  case Poison:
    return 6, nil
  default:
    return 0, errors.New("This effect doesn't have duration")
  }
}

type BattleState struct {
  Turn Turn
  Effects [3]int
  PlayerHP int
  EnemyHP int
  EnemyDamage int
  Mana int
  SpentMana int
}

func (bs BattleState) Next() []BattleState {
  if bs.PlayerHP <= 0 || bs.EnemyHP <= 0 {
    return []BattleState{}
  }
  if bs.Effects[Poison] > 0 {
    bs.EnemyHP -= PoisonDamage
    bs.Effects[Poison]--
    if bs.EnemyHP <= 0 {
      return []BattleState{bs}
    }
  }
  if bs.Effects[Recharge] > 0 {
    bs.Mana += RechargeMana
    bs.Effects[Recharge]--
  }
  if bs.Turn == Enemy {
    var damage int
    if bs.Effects[Shield] > 0 {
      damage = max(1, bs.EnemyDamage - ShieldProtection)
      bs.Effects[Shield]--
    } else {
      damage = bs.EnemyDamage
    }
    bs.PlayerHP -= damage
    bs.Turn = Player
    return []BattleState{bs}
  }
  next := []BattleState{}
  for _, effect := range Effects() {
    price, err := effect.Price()
    if err != nil {
      fmt.Println(err)
      continue
    }
    if price > bs.Mana {
      continue
    }
    state := bs
    switch effect {
    case MagicMissile:
      state.EnemyHP -= MagicMissileDamage
    case Drain:
      state.EnemyHP -= DrainDamage
      state.PlayerHP += DrainHeal
    case Shield, Poison, Recharge:
      duration, err := effect.Duration()
      if err != nil {
        fmt.Println(err)
        continue
      }
      state.Effects[effect] = duration
    }
    state.Mana -= price
    state.SpentMana += price
    state.Turn = Enemy
    next = append(next, state)
  }
  return next
}

func main() {
  configuration := BattleState{
    Turn: Player,
    Effects: [3]int{0, 0, 0},
    PlayerHP: 50,
    EnemyHP: 51,
    EnemyDamage: 9,
    Mana: 500,
    SpentMana: 0,
  }

  manaSpent := make(chan int)
  done := make(chan struct{})
  go func() {
    minValue := int(^uint(0) >> 1)
    for {
      select {
      case value := <- manaSpent:
        minValue = min(minValue, value)
        fmt.Printf("Received: %d, min: %d\n", value, minValue)
      case <-done:
        fmt.Printf("Finished with min mana: %d\n", minValue)
      }
    }
  }()

  states := []BattleState{configuration}
  for len(states) > 0 {
    nextStates := []BattleState{}
    var wg sync.WaitGroup
    wg.Add(len(states))
    results := make(chan BattleState)
    for _, state := range states {
      go func(state BattleState) {
        defer wg.Done()
        if state.EnemyHP <= 0 {
          manaSpent <- state.SpentMana
        }
        for _, nextState := range state.Next() {
          results <- nextState
        }
      } (state)
    }
    go func() {
      defer close(results)
      wg.Wait()
    }()
    for nextState := range results {
      nextStates = append(nextStates, nextState)
    }
    states = nextStates
  }
  
  done <- struct{}{}
}
