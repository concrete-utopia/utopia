import type { Optic } from './optics'
import type { Either } from '../either'
import { right, left, foldEither, isLeft } from '../either'

export function toArrayOf<S, A>(withOptic: Optic<S, A>, s: S): Array<A> {
  let result: Array<A> = []
  withOptic.forEach(s, (a) => {
    result.push(a)
  })
  return result
}

export function toFirst<S, A>(withOptic: Optic<S, A>, s: S): Either<string, A> {
  let result: Either<string, A> = left('No values present.')
  withOptic.forEach(s, (a) => {
    if (isLeft(result)) {
      result = right(a)
    }
  })
  return result
}

export function toLast<S, A>(withOptic: Optic<S, A>, s: S): Either<string, A> {
  let result: Either<string, A> = left('No values present.')
  withOptic.forEach(s, (a) => {
    result = right(a)
  })
  return result
}

export function foldLOf<S, A, R>(
  withOptic: Optic<S, A>,
  fold: (r: R, a: A) => R,
  initialValue: R,
  s: S,
): R {
  let result = initialValue
  withOptic.forEach(s, (a) => {
    result = fold(result, a)
  })
  return result
}

export function anyBy<S, A>(withOptic: Optic<S, A>, fromA: (a: A) => boolean, s: S): boolean {
  return foldLOf<S, A, boolean>(
    withOptic,
    (r, a) => {
      return r || fromA(a)
    },
    false,
    s,
  )
}

export function any<S>(withOptic: Optic<S, boolean>, s: S): boolean {
  return anyBy(withOptic, (b) => b, s)
}

export function allBy<S, A>(withOptic: Optic<S, A>, fromA: (a: A) => boolean, s: S): boolean {
  return foldLOf<S, A, boolean>(
    withOptic,
    (r, a) => {
      return r && fromA(a)
    },
    true,
    s,
  )
}

export function all<S>(withOptic: Optic<S, boolean>, s: S): boolean {
  return allBy(withOptic, (b) => b, s)
}

export function modify<S, A>(withOptic: Optic<S, A>, update: (a: A) => A, s: S): S {
  return withOptic.update(s, update)
}

export function set<S, A>(withOptic: Optic<S, A>, newValue: A, s: S): S {
  return withOptic.update(s, () => newValue)
}

export function exists<S, A>(withOptic: Optic<S, A>, s: S): boolean {
  let result = false
  withOptic.forEach(s, () => {
    result = true
  })
  return result
}

export function unsafeGet<S, A>(withOptic: Optic<S, A>, s: S): A {
  return foldEither(
    (err) => {
      throw new Error(`unsafeGet: Optic returned no results: ${err}`)
    },
    (a) => {
      return a
    },
    toFirst(withOptic, s),
  )
}

export function forEachOf<S, A>(withOptic: Optic<S, A>, s: S, withEach: (a: A) => void): void {
  withOptic.forEach(s, withEach)
}

export function lengthOf<S, A>(withOptic: Optic<S, A>, s: S): number {
  let result = 0
  withOptic.forEach(s, () => {
    result++
  })
  return result
}

// If we can obtain a value from `getFrom` using `getWithOptic`,
// then attempt to set that value into `setInto` using `setWithOptic`.
export function getAndSet<S1, S2, A2, A1 extends A2>(
  getWithOptic: Optic<S1, A1>,
  setWithOptic: Optic<S2, A2>,
  getFrom: S1,
  setInto: S2,
): S2 {
  const valueToSet = toFirst(getWithOptic, getFrom)
  return foldEither(
    () => {
      return setInto
    },
    (value) => {
      return set(setWithOptic, value, setInto)
    },
    valueToSet,
  )
}
