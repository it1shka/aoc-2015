import fs from 'node:fs/promises'

async function getInput() {
  const contents = await fs.readFile('input.txt', {
    encoding: 'utf-8',
  })
  const [rawRules, expression] = contents.split('\n\n')
  const rules = rawRules
    .split('\n')
    .map(e => e.split(' => '))
  return Object.freeze({ rules, expression })
}

function nextSteps(rules, current) {
  const output = []
  for (const [original, replacement] of rules) {
    let index, tail = current
    while ((index = tail.indexOf(original)) > -1) {
      const step = [
        current.slice(0, current.length - tail.length),
        tail.slice(0, index),
        replacement,
        tail.slice(index + original.length),
      ].join('')
      output.push(step)
      tail = tail.slice(index + original.length)
    }
  }
  return output
}

// I have to add heuristics here,
// otherwise it's too long
function bfs(rules, start, desired, {
  dfs = false,
  logging = false,
}) {
  const visited = new Set([start])
  const queue = [{ current: start, steps: 0 }]
  while (queue.length > 0) {
    const { current, steps } = dfs
      ? queue.pop()
      : queue.shift()
    if (logging) {
      console.log(`${steps}: ${current}`)
    }
    if (current === desired) {
      return steps
    }
    const forward = nextSteps(rules, current)
      .filter(e => {
        return !visited.has(e) && e.length <= desired.length
      })
    forward.forEach(e => visited.add(e))
    queue.push(...forward.map(current => {
      return Object.freeze({
        current,
        steps: steps + 1,
      })
    }))
  }
  return null
}

(async function main() {
  const { rules, expression } = await getInput()
  const steps = bfs(rules, 'e', expression, {
    dfs: true,
    logging: true,
  })
  console.log(steps)
})()
