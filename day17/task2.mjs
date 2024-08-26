import { readFile } from 'node:fs/promises'
import { createInterface } from 'node:readline/promises'
import { stdin, stdout } from 'node:process'

Array.prototype.sum = function() {
  let output = 0
  for (const each of this.valueOf()) {
    output += each
  }
  return output
}

Array.prototype.min = function() {
  return Math.min(...this.valueOf())
}

const powerset = (original) => {
  if (original.length <= 0) return [[]]
  const [head, ...tail] = original
  const nextPowerset = powerset(tail)
  return [
    ...nextPowerset, 
    ...nextPowerset.map(elem => [head, ...elem])
  ]
}

const solution = (liters, containers) => {
  const possibilities = powerset(containers)
    .filter(ctx => ctx.sum() === liters)
  const minLength = possibilities
    .map(elem => elem.length)
    .min()
  return possibilities
    .filter(elem => elem.length === minLength)
    .length
}

const main = async () => {
  const readline = createInterface({
    input: stdin,
    output: stdout,
  })

  const liters = 
    Number(await readline.question('Liters: '))
  const containers = 
    (await readFile('input.txt', { encoding: 'utf8' }))
    .trim()
    .split('\n')
    .map(e => Number(e.trim()))
    .sort((a, b) => a - b)

  console.log(`Containers: ${containers.join(', ')} (${containers.length} total)`)

  const answer = solution(liters, containers)
  console.log(`Answer: ${answer}`)

  readline.close()
}

main()
