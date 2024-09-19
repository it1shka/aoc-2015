import {readFile} from 'node:fs/promises'

function sumOf(values) {
  let output = 0
  for (const each of values) {
    output += each
  }
  return output
}

function prodOf(values) {
  let output = 1
  for (const each of values) {
    output *= each
  }
  return output
}

async function readWeights(filename) {
  const content = await readFile(filename, { encoding: 'utf-8' })
  return content
    .trim()
    .split('\n')
    .map(Number)
}

function* powerset(values) {
  if (values.length <= 0) {
    yield new Set()
    return
  }
  const [head, ...tail] = values
  for (const each of powerset(tail)) {
    yield each
    yield new Set([head, ...each])
  }
}

function* partitions(values, partitionWeight) {
  for (const group of powerset(values)) {
    if (sumOf(group) === partitionWeight) {
      yield group
    }
  }
}

function partitionExists(values, partitionWeight) {
  for (const group of powerset(values)) {
    if (sumOf(group) === partitionWeight) {
      return true
    }
  }
  return false
}

const weights = await readWeights('input.txt')
const weightSet = new Set(weights)
const partitionWeight = sumOf(weights) / 3
let minSize = Infinity, minEntanglement = Infinity
for (const firstPartition of partitions(weights, partitionWeight)) {
  const rest = Array.from(weightSet.difference(firstPartition))
  if (partitionExists(rest, partitionWeight) && firstPartition.size <= minSize) {
    minSize = firstPartition.size
    minEntanglement = Math.min(minEntanglement, prodOf(firstPartition))
    console.log(minEntanglement)
  }
}
