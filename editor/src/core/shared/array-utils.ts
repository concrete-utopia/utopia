import { MapLike } from 'typescript'
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

export function flatMapArray<T, U>(fn: (t: T, index: number) => Array<U>, arr: Array<T>): Array<U> {
  var workingArray: Array<U> = []
  fastForEach(arr, (arrayEntry, index) => {
    workingArray.push(...fn(arrayEntry, index))
  })
  return workingArray
}

export function mapDropNulls<T, U>(
  fn: (t: T, i: number) => U | null | undefined,
  a: Array<T>,
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

export function addAllUniquely<T extends string | number | boolean | null | undefined>(
  array: Array<T>,
  values: Array<T>,
): Array<T> {
  return values.reduce(addUniquely, array)
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
