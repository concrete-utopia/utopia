import moize from 'moize'
import { shallowEqual } from './equality-utils'

export interface MemoizeOptions {
  maxSize: number
  equals: <T>(a: T, b: T) => boolean
}

function getMemoizeOptions(options?: Partial<MemoizeOptions>): MemoizeOptions {
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

export function memoize<F extends (...args: Array<any>) => any>(
  func: F,
  options?: Partial<MemoizeOptions>,
): F {
  const memoizeOptions = getMemoizeOptions(options)
  return moize(func, memoizeOptions)
}
