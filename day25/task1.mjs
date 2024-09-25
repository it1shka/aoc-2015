function position(r, c) {
  let index = 0
  for (let diagonalSize = 1; ; diagonalSize++) {
    for (let row = diagonalSize; row >= 1; row--) {
      const column = diagonalSize - row + 1
      if (r === row && c === column) {
        return index
      }
      index++
    }
  }
}

const linearIndex = position(3010, 3019)
let acc = 20151125
for (let i = 0; i < linearIndex; i++) {
  acc *= 252533
  acc %= 33554393
}
console.log(acc)
