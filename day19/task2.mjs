import fs from 'node:fs/promises'

const cached = fn => {
  const cache = {}
  return (...args) => {
    const key = JSON.stringify(args)
    if (key in cache) return cache[key]
    return (cache[key] = fn(...args))
  }
}

Array.prototype.sortBy = function(evaluate) {
  return this
    .map(value => Object.freeze({
      value,
      score: evaluate(value),
    }))
    .sort(({ score: score1 }, { score: score2 }) => score1 - score2)
    .map(({ value }) => value)
}

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

function algorithm({ entry, desired, next, heuristics, logging }) {
  const visited = new Set([entry])
  let queue = [{ current: entry, steps: 0 }]
  while (queue.length > 0) {
    const { current, steps } = queue.shift()
    if (logging) {
      const metric = heuristics ? heuristics(current) : 'unknown'
      console.log(`${steps} (${metric}): ${current}`)
    }
    if (current === desired) {
      return steps
    }
    const forward = next(current)
      .filter(e => !visited.has(e))
    for (const each of forward) {
      visited.add(each)
      queue.push(Object.freeze({
        current: each,
        steps: steps + 1,
      }))
    }
    if (heuristics) {
      queue = queue.sortBy(({ current }) => {
        return heuristics(current)
      })
    }
  }
  return null
}

const next = rules => current => {
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

const levenshteinDistance = s => t => {
  if (!s.length) return t.length
  if (!t.length) return s.length
  const arr = []
  for (let i = 0; i <= t.length; i++) {
    arr[i] = [i]
    for (let j = 1; j <= s.length; j++) {
      arr[i][j] =
        i === 0
          ? j
          : Math.min(
              arr[i - 1][j] + 1,
              arr[i][j - 1] + 1,
              arr[i - 1][j - 1] + (s[j - 1] === t[i - 1] ? 0 : 1)
            )
    }
  }
  return arr[t.length][s.length]
}

(async function main() {
  const { rules, expression } = await getInput()
  const steps = algorithm({
    entry: 'e',
    desired: expression,
    next: cached(next(rules)),
    heuristics: cached(levenshteinDistance(expression)),
    logging: true,
  })
  console.log(steps)
})()
