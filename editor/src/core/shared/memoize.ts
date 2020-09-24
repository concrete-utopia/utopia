import moize from 'moize'
import { shallowEqual } from './equality-utils'

export interface MemoizeOptions<T> {
  maxSize: number
  equals: (a: T, b: T) => boolean
}

function getMemoizeOptions<T>(options?: Partial<MemoizeOptions<T>>): MemoizeOptions<T> {
  let equalsFunction = shallowEqual
  let maxSize = 5
  if (options != null) {
    if (options.equals != null) {
      equalsFunction = options.equals
    }
    if (options.maxSize != null) {
      maxSize = options.maxSize
    }
  }
  return {
    equals: equalsFunction,
    maxSize: maxSize,
  }
}

export function memoize<F extends (...args: Array<any>) => any, T>(
  func: F,
  options?: Partial<MemoizeOptions<T>>,
): F {
  const memoizeOptions = getMemoizeOptions(options)
  return moize(func, memoizeOptions)
}
