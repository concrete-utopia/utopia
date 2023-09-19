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
