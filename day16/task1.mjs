import * as fs from 'fs'

Object.prototype.pipe = function(action) {
  return action(this.valueOf())
}

const requirements = Object.freeze({
  children: 3,
  cats: 7,
  samoyeds: 2,
  pomeranians: 3,
  akitas: 0,
  vizslas: 0,
  goldfish: 5,
  trees: 3,
  cars: 2,
  perfumes: 1,
})

const parseDescription = raw => {
  const headerPattern = /^Sue\s+(\d+):\s+/
  const id = headerPattern
    .exec(raw)
    .at(1)
    .pipe(Number)
  const properties = raw
    .replace(headerPattern, '')
    .split(',')
    .map(part => {
      return part
        .split(':')
        .map(e => e.trim())
        .pipe(([a, b]) => [a, Number(b)])
    })
    .pipe(Object.fromEntries)
  return Object.freeze({ id, ...properties })
}

const fulfillsRequirements = description => {
  for (const [requirement, value] of Object.entries(requirements)) {
    if (requirement in description && description[requirement] !== value) {
      return false
    }
  }
  return true
}

fs.readFileSync('input.txt', { encoding: 'utf-8' })
  .trim()
  .split('\n')
  .map(parseDescription)
  .filter(fulfillsRequirements)
  .map(({ id }) => `Sue ${id}`)
  .join(', ')
  .pipe(console.log)
