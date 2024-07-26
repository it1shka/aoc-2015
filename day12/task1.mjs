import { readFileSync } from 'fs'

Object.prototype.apply = function(fn) {
  return fn(this.valueOf())
}

Array.prototype.total = function() {
  return this.reduce((a, b) => a + b, 0)
}

Array.prototype.flatten = function() {
  return this.reduce((acc, elem) => {
    return elem instanceof Array 
      ? [...acc, ...elem.flatten()]
      : [...acc, elem]
  }, [])
}

const sum = obj => {
  switch (true) {
    case typeof obj === 'number':
      return obj
    case typeof obj !== 'object':
      return 0
    case obj instanceof Array:
      return obj.map(sum).total()
    default:
      return Object
        .entries(obj)
        .flatten()
        .map(sum)
        .total()
  }
}

readFileSync('input.txt', { encoding: 'utf-8' })
  .apply(JSON.parse)
  .apply(sum)
  .apply(console.log)
