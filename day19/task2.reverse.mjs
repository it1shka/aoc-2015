import fs from 'node:fs/promises'

async function readInput() {
  const fileContent = await fs.readFile('input.txt', { encoding: 'utf8' })
  const [ruleList, targetMolecule] = fileContent.trim().split('\n\n')
  const reversedRules = ruleList.split('\n').map(rule => {
    return rule
      .trim()
      .split(' => ')
      .reverse()
  })
  return [reversedRules, targetMolecule]
}

function* getPossibleEvolutions(molecule, rules) {
  for (const [original, replacement] of rules) {
    const pattern = new RegExp(original, 'g')
    for (const { index } of molecule.matchAll(pattern)) {
      const evolution = 
        molecule.slice(0, index) + 
        replacement +
        molecule.slice(index + original.length)
      yield evolution
    }
  }
}

function* reduceAlgorithm(start, rules, desired) {
  const queue = [{ molecule: start, count: 0 }]
  while (queue.length > 0) {
    const { molecule, count } = queue.pop()
    if (molecule === desired) {
      yield count
    }
    for (const evolution of getPossibleEvolutions(molecule, rules)) {
      queue.push({ molecule: evolution, count: count + 1 })
    }
  }
}

const [rules, molecule] = await readInput()
for (const answer of reduceAlgorithm(molecule, rules, 'e')) {
  console.log(answer)
}
