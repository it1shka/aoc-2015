import * as fs from 'fs'

Object.prototype.pipe = function(action) {
  return action(this.valueOf())
}

const Constraint = Object.freeze({
  greater: 'greater',
  fewer: 'fewer',
  exact: 'exact',

  mapping: {
    greater: (a, b) => a > b,
    fewer:   (a, b) => a < b,
    exact:   (a, b) => a === b,
  }
})

const greaterThan = value => Object.freeze({
  limit: value,
  constraint: Constraint.greater,
})

const fewerThan = value => Object.freeze({
  limit: value,
  constraint: Constraint.fewer,
})

const exactly = value => Object.freeze({
  limit: value,
  constraint: Constraint.exact,
})

const requirements = Object.freeze({
  children:    exactly(3),
  cats:        greaterThan(7),
  samoyeds:    exactly(2),
  pomeranians: fewerThan(3),
  akitas:      exactly(0),
  vizslas:     exactly(0),
  goldfish:    fewerThan(5),
  trees:       greaterThan(3),
  cars:        exactly(2),
  perfumes:    exactly(1),
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
  for (const [requirement, { limit, constraint }] of Object.entries(requirements)) {
    const fulfills = Constraint.mapping[constraint]
    if (requirement in description && !fulfills(description[requirement], limit)) {
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
