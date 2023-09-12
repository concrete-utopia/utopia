import type { Optic } from './optics'
import { prism, lens, traversal, iso } from './optics'
import type { Either } from '../either'
import { right, left, foldEither, forEachRight } from '../either'
import { assertNever } from '../utils'

export function toArrayOf<S, A>(withOptic: Optic<S, A>, s: S): Array<A> {
  switch (withOptic.type) {
    case 'ISO':
      return [withOptic.from(s)]
    case 'LENS':
      return [withOptic.from(s)]
    case 'PRISM':
      return foldEither<string, A, Array<A>>(
        () => {
          return []
        },
        (a) => {
          return [a]
        },
        withOptic.from(s),
      )
    case 'TRAVERSAL':
      return withOptic.from(s)
    default:
      assertNever(withOptic)
  }
}

export function toFirst<S, A>(withOptic: Optic<S, A>, s: S): Either<string, A> {
  const asList = toArrayOf(withOptic, s)
  if (asList.length > 0) {
    return right(asList[0])
  } else {
    return left('No values present.')
  }
}

export function toLast<S, A>(withOptic: Optic<S, A>, s: S): Either<string, A> {
  const asList = toArrayOf(withOptic, s)
  if (asList.length > 0) {
    return right(asList[asList.length - 1])
  } else {
    return left('No values present.')
  }
}

export function inverseGet<S, A>(withOptic: Optic<S, A>, a: A): Either<string, S> {
  switch (withOptic.type) {
    case 'ISO':
      return right(withOptic.to(a))
    case 'LENS':
      return left(`Unable to invert a LENS.`)
    case 'PRISM':
      return right(withOptic.to(a))
    case 'TRAVERSAL':
      return left(`Unable to invert a TRAVERSAL.`)
    default:
      assertNever(withOptic)
  }
}

export function foldLOf<S, A, R>(
  withOptic: Optic<S, A>,
  fold: (r: R, a: A) => R,
  initialValue: R,
  s: S,
): R {
  return toArrayOf(withOptic, s).reduce(fold, initialValue)
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
  switch (withOptic.type) {
    case 'ISO':
      return withOptic.to(update(withOptic.from(s)))
    case 'LENS':
      return withOptic.update(s, update(withOptic.from(s)))
    case 'PRISM':
      return foldEither(
        () => {
          return s
        },
        (a) => {
          return withOptic.to(update(a))
        },
        withOptic.from(s),
      )
    case 'TRAVERSAL':
      return withOptic.update(s, update)
    default:
      assertNever(withOptic)
  }
}

export function set<S, A>(withOptic: Optic<S, A>, newValue: A, s: S): S {
  switch (withOptic.type) {
    case 'ISO':
      return withOptic.to(newValue)
    case 'LENS':
      return withOptic.update(s, newValue)
    case 'PRISM':
      return foldEither(
        () => {
          return s
        },
        () => {
          return withOptic.to(newValue)
        },
        withOptic.from(s),
      )
    case 'TRAVERSAL':
      return withOptic.update(s, () => newValue)
    default:
      assertNever(withOptic)
  }
}

export function exists<S, A>(withOptic: Optic<S, A>, s: S): boolean {
  switch (withOptic.type) {
    case 'ISO':
      return true
    case 'LENS':
      return true
    case 'PRISM':
      return foldEither(
        () => {
          return false
        },
        () => {
          return true
        },
        withOptic.from(s),
      )
    case 'TRAVERSAL':
      return withOptic.from(s).length > 0
    default:
      assertNever(withOptic)
  }
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
  switch (withOptic.type) {
    case 'ISO':
      withEach(withOptic.from(s))
      break
    case 'LENS':
      withEach(withOptic.from(s))
      break
    case 'PRISM':
      forEachRight(withOptic.from(s), withEach)
      break
    case 'TRAVERSAL':
      withOptic.from(s).forEach(withEach)
      break
    default:
      assertNever(withOptic)
  }
}

export function lengthOf<S, A>(withOptic: Optic<S, A>, s: S): number {
  switch (withOptic.type) {
    case 'ISO':
      return 1
    case 'LENS':
      return 1
    case 'PRISM':
      return foldEither(
        () => 0,
        () => 1,
        withOptic.from(s),
      )
    case 'TRAVERSAL':
      return withOptic.from(s).length
    default:
      assertNever(withOptic)
  }
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
