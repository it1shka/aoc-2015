import fs from 'node:fs/promises'

async function getShopItems(filename) {
  const content = await fs.readFile(filename, { encoding: 'utf-8' })
  return Object.fromEntries(content.trim().split('\n\n').map(section => {
    const [header, ...itemLines] = section.split('\n')
    const [sectionName, ...columns] = header.toLowerCase().split(/[^a-z]+/gi)
    const items = itemLines.map(line => {
      const [itemName, ...parameters] = line.split(/\s+/gi)
      return Object.fromEntries([
        ['name', itemName],
        ...parameters.map((parameter, index) => {
          return [columns[index], Number(parameter)]
        })
      ])
    })
    return [sectionName, items]
  }))
}

function choose(array, n) {
  if (n <= 0) return [[]]
  const output = []
  for (let i = 0; i < array.length; i++) {
    const elem = array[i]
    for (const each of choose(array.slice(i + 1), n - 1)) {
      output.push([elem, ...each])
    }
  }
  return output
}

function chooseWithInterval(array, min, max) {
  const output = []
  for (let i = min; i <= max; i++) {
    output.push(...choose(array, i))
  }
  return output
}

function allConfigurations(...[first, ...rest]) {
  if (rest.length <= 0) return first
  const output = []
  for (const conf of allConfigurations(...rest)) {
    for (const each of first) {
      output.push([...each, ...conf])
    }
  }
  return output
}

function buildPlayer(configuration, health = 100) {
  return configuration.reduce((acc, { damage, armor }) => {
    return Object.freeze({
      ...acc,
      damage: acc.damage + damage,
      armor: acc.armor + armor,
    })
  }, { damage: 0, armor: 0, health })
}

function fight(player, enemy) {
  let attack = true
  while (player.health > 0 && enemy.health > 0) {
    const [attacker, defender] = attack
      ? [player, enemy]
      : [enemy, player]
    const damage = Math.max(1, attacker.damage - defender.armor)
    defender.health -= damage
    attack = !attack
  }
  return player.health > 0
}

async function main() {
  const { weapons, armor, rings } = await getShopItems('input.txt')
  const enemy = Object.freeze({
    health: 103,
    damage: 9,
    armor: 2,
  })
  const configurations = allConfigurations (
    chooseWithInterval(weapons, 1, 1),
    chooseWithInterval(armor, 0, 1),
    chooseWithInterval(rings, 0, 2),
  )
  const failures = configurations.filter(config => {
    return !fight({ ...buildPlayer(config) }, { ...enemy })
  })
  const costs = failures.map(config => {
    return config
      .map(({ cost }) => cost)
      .reduce((a, b) => a + b, 0)
  })
  const maxCost = Math.max(...costs)
  console.log(maxCost)
}

await main()
