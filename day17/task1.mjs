import { readFile } from 'node:fs/promises'
import { createInterface } from 'node:readline/promises'
import { stdin, stdout } from 'node:process'

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
  return powerset(containers)
    .map(ctx => ctx.reduce((a, b) => a + b, 0))
    .filter(sum => sum === liters)
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
  console.log(`Possibilities: ${answer}`)

  readline.close()
}

main()
