import type { MapLike } from 'typescript'
import { is, shallowEqual } from './equality-utils'
import { clamp } from './math-utils'
import { fastForEach } from './utils'

export function stripNulls<T>(array: Array<T | null | undefined>): Array<T> {
  var workingArray: Array<T> = []
  for (const value of array) {
    if (value !== null && value !== undefined) {
      workingArray.push(value)
    }
  }
  return workingArray
}

export function filterDuplicates<T>(array: Array<T>): Array<T> {
  var workingArray: Array<T> = []
  for (const value of array) {
    if (workingArray.indexOf(value) < 0) {
      workingArray.push(value)
    }
  }
  return workingArray
}

export function flatMapArray<T, U>(
  fn: (t: T, index: number) => Array<U>,
  arr: ReadonlyArray<T>,
): Array<U> {
  var workingArray: Array<U> = []
  fastForEach(arr, (arrayEntry, index) => {
    workingArray.push(...fn(arrayEntry, index))
  })
  return workingArray
}

export function mapDropNulls<T, U>(
  fn: (t: T, i: number) => U | null | undefined,
  a: ReadonlyArray<T>,
): Array<U> {
  let result: Array<U> = []
  fastForEach(a, (t, i) => {
    const u = fn(t, i)
    if (u != null) {
      result.push(u)
    }
  })
  return result
}

export function dropNulls<T>(a: ReadonlyArray<T | null | undefined>): Array<T> {
  return mapDropNulls((t) => t, a)
}

export function mapAndFilter<T, U>(
  mapFn: (t: T, i: number) => U,
  filter: (u: U) => boolean,
  a: ReadonlyArray<T>,
): Array<U> {
  return mapDropNulls((t: T, i: number) => {
    const mapResult = mapFn(t, i)
    if (filter(mapResult)) {
      return mapResult
    } else {
      return null
    }
  }, a)
}

export function mapFirstApplicable<T, U>(
  array: Iterable<T>,
  mapFn: (t: T, i: number) => U | null,
): U | null {
  for (const value of array) {
    const mapped = mapFn(value, 0)
    if (mapped != null) {
      return mapped
    }
  }
  return null
}

// Dumb version of:
// traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
export function traverseArray<T, U>(
  fn: (t: T, index: number) => U | null,
  arr: Array<T>,
): Array<U> | null {
  var workingArray: Array<U> = []
  var arrayIndex = 0
  for (const arrayEntry of arr) {
    const result = fn(arrayEntry, arrayIndex)
    if (result == null) {
      return null
    } else {
      workingArray.push(result)
    }
    arrayIndex += 1
  }
  return workingArray
}

export function pluck<T, K extends keyof T>(list: T[], key: K): T[K][] {
  return list.map((listItem) => listItem[key])
}

export function arrayToObject<T>(
  arr: Array<T>,
  keyFn: (t: T) => string,
): {
  [key: string]: T
} {
  const result: {
    [key: string]: T
  } = {}
  for (const t of arr) {
    result[keyFn(t)] = t
  }
  return result
}

export function mapArrayToDictionary<From, Values, Keys extends string | number>(
  arr: ReadonlyArray<From>,
  keyFn: (t: From, index: number) => Keys,
  mapFn: (t: From, index: number) => Values,
): {
  [key in Keys]: Values
} {
  return arr.reduce(
    (working, next, index) => {
      const key = keyFn(next, index)
      working[key] = mapFn(next, index)
      return working
    },
    {} as {
      [key in Keys]: Values
    },
  )
}
// taken from Ramda.move: https://github.com/ramda/ramda/blob/v0.26.1/source/move.js

export function move<T>(from: number, to: number, list: Array<T>): Array<T> {
  var length = list.length
  var result = [...list]
  var positiveFrom = from < 0 ? length + from : from
  var positiveTo = to < 0 ? length + to : to
  var item = result.splice(positiveFrom, 1)
  if (
    positiveFrom < 0 ||
    positiveFrom >= list.length ||
    positiveTo < 0 ||
    positiveTo >= list.length
  ) {
    return list
  } else {
    return result.slice(0, positiveTo).concat(item).concat(result.slice(positiveTo, list.length))
  }
}

export function uniq<T extends string | number | boolean>(array: Array<T>): Array<T> {
  // this is ~2x faster than R.uniq
  // https://runkit.com/bayjorix/5aec102a814a000012474a6e
  return Array.from(new Set(array))
}

export function uniqBy<T>(array: ReadonlyArray<T>, eq: (l: T, r: T) => boolean): Array<T> {
  let result: Array<T> = []
  fastForEach(array, (elem) => {
    if (result.find((e) => eq(e, elem)) == null) {
      result.push(elem)
    }
  })
  return result
}

export function sortBy<T>(array: ReadonlyArray<T>, compare: (l: T, r: T) => number): Array<T> {
  let result = [...array]
  result.sort(compare)
  return result
}

export function drop<T>(n: number, array: Array<T>): Array<T> {
  return array.slice(n)
}

export function dropLast<T>(array: Array<T>): Array<T> {
  return dropLastN(1, array)
}

export function dropLastN<T>(n: number, array: Array<T>): Array<T> {
  return array.slice(0, -n)
}

export function take<T>(n: number, array: Array<T>): Array<T> {
  return array.slice(0, n)
}

export function last<T>(array: Array<T>): T | undefined {
  return array[array.length - 1]
}

export function lastOfNonEmptyArray<T>(array: NonEmptyArray<T>): T {
  return array[array.length - 1]
}

export function splitAt<T>(n: number, array: Array<T>): [Array<T>, Array<T>] {
  return [take(n, array), drop(n, array)]
}

export function removeIndexFromArray<T>(index: number, arr: Array<T>): Array<T> {
  let result: Array<T> = [...arr]
  result.splice(index, 1)
  return result
}

export function flattenArray<T>(array: Array<Array<T>>): Array<T> {
  let result: Array<T> = []
  fastForEach(array, (elem) => result.push(...elem))
  return result
}

export function addToMapOfArraysUnique<V, M extends MapLike<Array<V>>>(
  map: M,
  key: string,
  value: V,
): M {
  if (key in map) {
    const existing: Array<V> = map[key]
    if (existing.includes(value)) {
      return map
    } else {
      return {
        ...map,
        [key]: [...existing, value],
      }
    }
  } else {
    return {
      ...map,
      [key]: [value],
    }
  }
}

export function groupBy<T>(
  toString: (value: T) => string,
  array: Array<T>,
): { [key: string]: Array<T> } {
  let result: { [key: string]: Array<T> } = {}

  for (const value of array) {
    const key = toString(value)
    if (key in result) {
      let existing: Array<T> = result[key]
      existing.push(value)
    } else {
      result[key] = [value]
    }
  }

  return result
}

export function addUniquely<T extends string | number | boolean | null | undefined>(
  array: Array<T>,
  value: T,
): Array<T> {
  if (array.includes(value)) {
    return array
  } else {
    return [...array, value]
  }
}

export function pushUniquelyBy<T>(array: Array<T>, value: T, eq: (l: T, r: T) => boolean): void {
  if (array.findIndex((a) => eq(a, value)) === -1) {
    array.push(value)
  }
}

export function addAllUniquely<T extends string | number | boolean | null | undefined>(
  array: Array<T>,
  values: Array<T>,
): Array<T> {
  return values.reduce(addUniquely, array)
}

export function addAllUniquelyBy<T>(
  array: Array<T>,
  values: Array<T>,
  eq: (l: T, r: T) => boolean,
): Array<T> {
  let workingArray = [...array]
  fastForEach(values, (value) => {
    if (!workingArray.some((a) => eq(a, value))) {
      workingArray.push(value)
    }
  })
  return workingArray
}

export function findLastIndex<T>(predicate: (t: T) => boolean, array: ReadonlyArray<T>): number {
  // Assumes non-sparse arrays starting at zero.
  for (let index: number = array.length - 1; index >= 0; index--) {
    const elem = array[index]
    if (elem != null && predicate(elem)) {
      return index
    }
  }
  return -1
}

// For those times when you want to join but use a different separator for the last value
export function joinSpecial(
  arr: Array<string>,
  normalSeparator: string,
  lastSeparator: string,
): string {
  let result: string = ''
  const length = arr.length
  fastForEach(arr, (next, index) => {
    if (index === 0) {
      result = next
    } else if (index === length - 1) {
      result = `${result}${lastSeparator}${next}`
    } else {
      result = `${result}${normalSeparator}${next}`
    }
  })
  return result
}

export function removeAll<T>(
  target: ReadonlyArray<T>,
  toRemove: ReadonlyArray<T>,
  eqFn: (l: T, R: T) => boolean = is,
): Array<T> {
  let result: Array<T> = []
  fastForEach(target, (nextA) => {
    if (toRemove.findIndex((nextB) => eqFn(nextA, nextB)) < 0) {
      result.push(nextA)
    }
  })
  return result
}

export function immutablyUpdateArrayIndex<T>(
  oldValue: Array<T>,
  newValue: T,
  indexToReplace: number,
): Array<T> {
  let working = [...oldValue]
  working[indexToReplace] = newValue
  return working
}

export function safeIndex<T>(array: ReadonlyArray<T>, index: number): T | undefined {
  if (index in array) {
    return array[index]
  } else {
    return undefined
  }
}

export function intersection<T>(
  first: Array<T>,
  second: Array<T>,
  eqFn: (l: T, r: T) => boolean = is,
): Array<T> {
  let result: Array<T> = []
  for (const valueFromFirst of first) {
    let foundIntersection: boolean = false
    for (const valueFromSecond of second) {
      foundIntersection = eqFn(valueFromFirst, valueFromSecond)
      if (foundIntersection) {
        let shouldAdd: boolean = true
        for (const valueFromResult of result) {
          if (eqFn(valueFromResult, valueFromFirst)) {
            shouldAdd = false
            break
          }
        }
        if (shouldAdd) {
          result.push(valueFromFirst)
        }
      }
    }
  }

  return result
}

export function difference<T>(
  first: Array<T>,
  second: Array<T>,
  eqFn: (l: T, r: T) => boolean = is,
): Array<T> {
  let result: Array<T> = []
  for (const valueFromFirst of first) {
    let foundInSecondArray: boolean = false
    for (const valueFromSecond of second) {
      foundInSecondArray = eqFn(valueFromFirst, valueFromSecond)
      if (foundInSecondArray) {
        break
      }
    }
    if (!foundInSecondArray) {
      result.push(valueFromFirst)
    }
  }

  return result
}

export function insert<T>(index: number, element: T, array: ReadonlyArray<T>): Array<T> {
  const clampedIndex = clamp(0, array.length, index)
  return [...array.slice(0, clampedIndex), element, ...array.slice(clampedIndex, array.length)]
}

export function insertMultiple<T>(index: number, element: Array<T>, array: Array<T>): Array<T> {
  const clampedIndex = clamp(0, array.length, index)
  return [...array.slice(0, clampedIndex), ...element, ...array.slice(clampedIndex, array.length)]
}

export function reverse<T>(array: Array<T>): Array<T> {
  let result = [...array]
  result.reverse()
  return result
}

export function aperture<T>(n: number, array: Array<T>): Array<Array<T>> {
  if (n > 0) {
    if (n > array.length) {
      return [array]
    } else {
      let result: Array<Array<T>> = []
      for (let arrayIndex: number = 0; arrayIndex < array.length - n + 1; arrayIndex++) {
        result.push(array.slice(arrayIndex, arrayIndex + n))
      }
      return result
    }
  } else {
    return []
  }
}

export function cartesianProduct<T, U>(one: ReadonlyArray<T>, other: ReadonlyArray<U>): [T, U][] {
  return one.flatMap((x) => other.map((y): [T, U] => [x, y]))
}

export function allElemsEqual<T>(
  ts: T[],
  areEqual: (a: T, b: T) => boolean = shallowEqual,
): boolean {
  if (ts.length === 0) {
    return false
  }

  return ts.slice(1).every((obj) => areEqual(ts[0], obj))
}

export function strictEvery<T>(
  ts: T[],
  predicate: (t: T, index: number, array: T[]) => boolean,
): boolean {
  return ts.length > 0 && ts.every(predicate)
}

export function arrayAccumulate<T>(callback: (acc: Array<T>) => void): ReadonlyArray<T> {
  const accumulator: Array<T> = []
  callback(accumulator)
  return accumulator
}

export function accumulate<T>(accumulator: T, callback: (acc: T) => void): Readonly<T> {
  callback(accumulator)
  return accumulator
}

export function zip<A, B, C>(one: A[], other: B[], make: (a: A, b: B) => C): C[] {
  const doZip = (oneInner: A[], otherInner: B[]) =>
    oneInner.map((elem, idx) => make(elem, otherInner[idx]))

  return one.length < other.length ? doZip(one, other) : doZip(one.slice(0, other.length), other)
}

// https://matiashernandez.dev/blog/post/typescript-how-to-create-a-non-empty-array-type
export type NonEmptyArray<T> = [T, ...T[]]
export function isNonEmptyArray<T>(array: T[]): array is NonEmptyArray<T> {
  let [first] = array
  return first != null
}

export function isEmptyArray<T>(array: T[]): array is [] {
  return array.length === 0
}

export function possiblyUniqueInArray(
  array: number[],
  existing: (number | null)[],
  start: number,
): number {
  let index = start
  while (existing.includes(index)) {
    index++
    if (index >= array.length) {
      index = 0
    }
    if (index === start) {
      return start
    }
  }
  return index
}

export function isPrefixOf<T>(
  possiblePrefix: Array<T>,
  checkAgainst: Array<T>,
  equals: (first: T, second: T) => boolean = (first, second) => first === second,
): boolean {
  if (possiblePrefix.length <= checkAgainst.length) {
    return possiblePrefix.every((prefixValue, prefixIndex) => {
      return equals(prefixValue, checkAgainst[prefixIndex])
    })
  } else {
    // Prefix is too long to be a prefix.
    return false
  }
}

export function valueOrArrayToArray<T>(ts: T | T[]): T[] {
  return Array.isArray(ts) ? ts : [ts]
}

export function createArrayWithLength<T>(length: number, value: (index: number) => T): T[] {
  return Array.from({ length }, (_, index) => {
    // see issue https://github.com/microsoft/TypeScript/issues/37750
    return value instanceof Function ? value(index) : value
  })
}

export function matrixGetter<T>(array: T[], width: number): (row: number, column: number) => T {
  return (row, column) => {
    return array[row * width + column]
  }
}

export function range(start: number, end: number): Array<number> {
  let result: Array<number> = []
  for (let i = start; i < end; i++) {
    result.push(i)
  }
  return result
}

export function chunkArrayEqually<T>(
  sortedArray: T[],
  numberOfChunks: number,
  valueFn: (t: T) => number,
): T[][] {
  const chunks: T[][] = Array.from({ length: numberOfChunks }, () => [])
  const chunkSums: number[] = Array(numberOfChunks).fill(0)
  for (const data of sortedArray) {
    let minIndex = 0
    for (let i = 1; i < numberOfChunks; i++) {
      if (chunkSums[i] < chunkSums[minIndex]) {
        minIndex = i
      }
    }
    chunks[minIndex].push(data)
    chunkSums[minIndex] += valueFn(data)
  }
  return chunks.filter((chunk) => chunk.length > 0)
}

export function sortArrayByAndReturnPermutation<T>(
  array: T[],
  sortFn: (t: T) => number,
  ascending: boolean = true,
): { sortedArray: T[]; permutation: number[] } {
  const permutation = array.map((_, index) => index)
  permutation.sort((a, b) => {
    const sortResult = sortFn(array[a]) - sortFn(array[b])
    return ascending ? sortResult : -sortResult
  })
  const sortedArray = permutation.map((index) => array[index])
  return { sortedArray, permutation }
}

export function revertArrayOrder<T>(array: T[], permutation: number[]): T[] {
  return array.map((_, index) => array[permutation.indexOf(index)])
}

// From https://stackoverflow.com/a/31879739
export function interleaveArray<T>(array: T[], elem: T): T[] {
  const newArray = []
  let i = 0
  if (i < array.length) {
    newArray.push(array[i++])
  }
  while (i < array.length) {
    newArray.push(elem, array[i++])
  }
  return newArray
}
