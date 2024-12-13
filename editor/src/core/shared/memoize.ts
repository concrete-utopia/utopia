import moize from 'moize'
import type { Options, Moizeable } from 'moize'
import { shallowEqual } from './equality-utils'

function getMemoizeOptions<T extends Moizeable>(options?: Partial<Options<T>>): Options<T> {
  let matchesArg = shallowEqual
  let maxSize = 5
  if (options != null) {
    if (options.matchesArg != null) {
      matchesArg = options.matchesArg
    }
    if (options.maxSize != null) {
      maxSize = options.maxSize
    }
  }
  return {
    matchesArg: matchesArg,
    maxSize: maxSize,
  }
}

export function memoize<T extends Moizeable>(func: T, options?: Partial<Options<T>>): T {
  const memoizeOptions = getMemoizeOptions(options)
  return moize(func, memoizeOptions)
}

export function valueDependentCache<Value, Input, Result>(
  fallback: (value: Value, input: Input) => Result,
  inputToString: (input: Input) => string,
  options?: {
    equality?: (a: Value, b: Value) => boolean
  },
): (value: Value, input: Input) => Result {
  let cache: { [key: string]: Result } = {}
  let lastSeenValue: Value | null = null
  const eq = options?.equality ?? ((a, b) => a === b)
  return (value: Value, input: Input) => {
    const inputAsString = inputToString(input)
    if (lastSeenValue == null || !eq(lastSeenValue, value)) {
      // Either this is the first use of the function, or the value has changed.
      lastSeenValue = value
      const result = fallback(value, input)
      cache = { [inputAsString]: result }
      return result
    } else {
      // The value hasn't changed, so we can use the cache.
      if (inputAsString in cache) {
        return cache[inputAsString]
      } else {
        const result = fallback(value, input)
        cache[inputAsString] = result
        return result
      }
    }
  }
}
