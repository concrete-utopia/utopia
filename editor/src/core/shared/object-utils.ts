import { fastForEach } from './utils'
import type { MapLike } from 'typescript'

export function get<O, T, F>(obj: O, unsafeDataOperation: (x: O) => T, valueIfFail: F): T | F {
  try {
    return unsafeDataOperation(obj)
  } catch (error) {
    return valueIfFail
  }
}

export function setOptionalProp<V, K extends keyof V>(
  value: V,
  key: K,
  prop: V[K] | undefined,
): void {
  if (prop !== undefined) {
    value[key] = prop
  }
}

export function propOr<OR, K extends keyof O, O>(
  val: O[K] | OR,
  key: K & string,
  obj: O | null | undefined,
): O[K] | OR {
  if (obj == null) {
    return val
  } else {
    const possibleValue = obj[key]
    if (possibleValue === undefined) {
      return val
    } else {
      return possibleValue
    }
  }
}

export function propOrNull<K extends keyof O, O>(
  key: K & string,
  obj: O | null | undefined,
): O[K] | null {
  return propOr(null, key, obj)
}

export function copyObjectWithoutFunctions(obj: any) {
  let copiedObject: any = {}
  Object.getOwnPropertyNames(obj).forEach((prop: any) => {
    if (typeof obj[prop] !== 'function') {
      copiedObject[prop] =
        typeof obj[prop] === 'object' ? copyObjectWithoutFunctions(obj[prop]) : obj[prop]
    }
  })
  return copiedObject
}

export function getAllObjectPaths(value: any): Array<Array<string>> {
  let result: Array<Array<string>> = []
  function addObjectPaths(pathsSoFar: Array<string>, workingValue: any): void {
    if (workingValue != null && typeof workingValue === 'object') {
      fastForEach(Object.keys(workingValue), (key) => {
        const valueAgainstKey = workingValue[key]
        const fullKeyPath = [...pathsSoFar, key]
        result.push(fullKeyPath)
        if (typeof valueAgainstKey === 'object') {
          addObjectPaths(fullKeyPath, valueAgainstKey)
        }
      })
    }
  }
  addObjectPaths([], value)
  return result
}

export function modifyKey<V>(
  map: {
    [key: string]: V
  },
  key: string,
  transform: (value: V) => V,
  defaultValue: () => V,
): {
  [key: string]: V
} {
  const possibleCurrent: V | undefined = map[key]
  return {
    ...map,
    [key]: transform(possibleCurrent === undefined ? defaultValue() : possibleCurrent),
  }
}

export function mapValues<U, V>(
  transform: (u: U, k: string) => V,
  map: {
    [key: string]: U
  },
): {
  [key: string]: V
} {
  var result: {
    [key: string]: V
  } = {}
  Object.keys(map).forEach((key) => {
    result[key] = transform(map[key], key)
  })
  return result
}

type ValueOf<T> = T[keyof T]
export function objectMap<T extends Record<string, unknown>, K extends keyof T, U>(
  transform: (t: ValueOf<T>, key: K) => U,
  obj: T,
): {
  [key in K]: U
} {
  const keys = Object.keys(obj) as Array<K>
  const mappedObj = {} as {
    [key in K]: U
  }
  fastForEach(keys, (key) => {
    const value = obj[key]
    mappedObj[key] = transform(value, key)
  })
  return mappedObj
}

export function objectMapDropNulls<T extends MapLike<any>, K extends keyof T, U>(
  transform: (t: ValueOf<T>, key: K) => U | null,
  obj: T,
): {
  [key in K]: U
} {
  const keys = Object.keys(obj) as Array<K>
  const mappedObj = {} as {
    [key in K]: U
  }
  fastForEach(keys, (key) => {
    const value = obj[key]
    const transformResult = transform(value, key)
    if (transformResult != null) {
      mappedObj[key] = transformResult
    }
  })
  return mappedObj
}

export function objectFilter<T extends MapLike<any>>(
  filter: (t: ValueOf<T>, key: keyof T) => boolean,
  obj: T,
): T {
  const keys = Object.keys(obj) as Array<keyof T>
  let filteredResult = {} as T
  fastForEach(keys, (key) => {
    const value = obj[key]
    if (filter(value, key)) {
      filteredResult[key] = value
    }
  })

  return filteredResult
}

export function forEachValue<T extends MapLike<any>>(
  fn: (val: ValueOf<T>, key: keyof T) => void,
  obj: T,
): void {
  const keys = Object.keys(obj) as Array<keyof T>
  fastForEach(keys, (key) => {
    const value = obj[key]
    fn(value, key)
  })
}

export function mapToArray<T, U>(
  fn: (val: T, key: string, index: number) => U,
  obj: { [key: string]: T },
): Array<U> {
  const keys = Object.keys(obj)
  let result: Array<U> = []
  fastForEach(keys, (key, index) => {
    const value = obj[key]
    result.push(fn(value, key, index))
  })

  return result
}

export function mergeObjects<T, U>(
  array: Array<T>,
  fn: (elem: T) => {
    [key: string]: U
  },
): {
  [key: string]: U
} {
  let result: {
    [key: string]: U
  } = {}
  fastForEach(array, (elem) => {
    result = {
      ...result,
      ...fn(elem),
    }
  })
  return result
}

// this type filters out Map and Set and other iterable object-like types
export type SimpleObject<T> = { [key: string]: T } & { [Symbol.iterator]?: never }

export function objectValues<T>(object: { [key: string]: T }): Array<T> {
  const objectKeys = Object.keys(object)
  return objectKeys.map((key) => object[key])
}

export function isEmptyObject(obj: SimpleObject<any>): boolean {
  return Object.keys(obj).length === 0
}

export function objectFlattenKeys(obj: any): Array<string> {
  let keys: Array<string> = []
  const isArray = Array.isArray(obj)
  fastForEach(Object.keys(obj), (key) => {
    if (!isArray) {
      keys.push(key)
    }
    const value = obj[key]
    if (typeof value === 'object') {
      const subKeys = objectFlattenKeys(value)
      keys.push(...subKeys)
    }
  })
  return keys
}

// From https://github.com/ramda/ramda/blob/v0.27.0/source/omit.js
export function omit<K extends string | number, T extends Record<K, any>>(
  keys: ReadonlyArray<K>,
  obj: T,
): Omit<T, K> {
  var result = {} as T
  var index = {} as Record<K, number>
  var idx = 0
  var len = keys.length

  while (idx < len) {
    index[keys[idx]] = 1
    idx += 1
  }

  for (var prop in obj) {
    if (!index.hasOwnProperty(prop)) {
      result[prop] = obj[prop]
    }
  }
  return result
}

export function omitWithPredicate<T extends MapLike<any>>(
  obj: T,
  pred: <K extends keyof T>(k: K, v: T[K]) => boolean,
): T {
  var result = {} as T

  for (var prop in obj) {
    if (!pred(prop, obj[prop])) {
      result[prop] = obj[prop]
    }
  }
  return result
}

export function pick<T extends MapLike<any>, K extends keyof T>(
  keys: ReadonlyArray<K>,
  obj: T,
): Pick<T, K> {
  var result = {} as Pick<T, K>

  fastForEach(keys, (key) => {
    if (key in obj) {
      result[key] = obj[key]
    }
  })
  return result
}

export function objectContainsKey<T extends MapLike<any>>(obj: T, key: keyof T): boolean {
  return key in obj
}

export function objectContainsAllKeys<T extends MapLike<any>>(
  obj: T,
  keys: Array<keyof T>,
): boolean {
  return keys.every((key) => objectContainsKey(obj, key))
}

export function typedObjectKeys<T extends object>(obj: T): Array<keyof T> {
  return Object.keys(obj) as Array<keyof T>
}
