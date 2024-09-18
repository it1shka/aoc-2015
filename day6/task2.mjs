import {createReadStream} from 'node:fs'
import {createInterface} from 'node:readline'
import {from, map, mergeMap, reduce, tap} from 'rxjs'

const FIELD_SIZE = 1000
const field = Array.from(Array(FIELD_SIZE), () => {
  return Array(FIELD_SIZE).fill(0)
})

const Commands = Object.freeze({
  'turn on': x => x + 1,
  'turn off': x => Math.max(0, x - 1),
  'toggle': x => x + 2,
})

const inputInterface = createInterface({
  input: createReadStream('input.txt'),
  crlfDelay: Infinity,
})

const flow$ = from(inputInterface).pipe(
  tap(line => {
    console.log(`Executing ${line}`)
  }),
  map(line => {
    const pattern = /^(turn on|turn off|toggle) (\d+)\,(\d+) through (\d+)\,(\d+)$/
    return line.match(pattern)
  }),
  reduce((acc, [_, command, x1, y1, x2, y2]) => {
    for (let x = Number(x1); x <= Number(x2); x++) {
      for (let y = Number(y1); y <= Number(y2); y++) {
        acc[y][x] = Commands[command](acc[y][x])
      }
    }
    return acc
  }, field),
  mergeMap(matrix => from(matrix)),
  mergeMap(row => from(row)),
  reduce((acc, elem) => acc + elem, 0),
)

flow$.subscribe(answer => {
  console.log(`The answer is: ${answer}`)
})
