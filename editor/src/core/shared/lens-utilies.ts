import { prism, lens, traversal, iso, GeneralLens } from './lens'
import { right, left, foldEither, Either } from './either'
import { assertNever } from './utils'

export function toArrayOf<S, A>(withLens: GeneralLens<S, A>, s: S): Array<A> {
  switch (withLens.type) {
    case 'ISO':
      return [withLens.from(s)]
    case 'LENS':
      return [withLens.from(s)]
    case 'PRISM':
      return foldEither<string, A, Array<A>>(
        () => {
          return []
        },
        (a) => {
          return [a]
        },
        withLens.from(s),
      )
    case 'TRAVERSAL':
      return withLens.from(s)
    default:
      assertNever(withLens)
  }
}

export function toFirst<S, A>(withLens: GeneralLens<S, A>, s: S): Either<string, A> {
  const asList = toArrayOf(withLens, s)
  if (asList.length > 0) {
    return right(asList[0])
  } else {
    return left('No values present.')
  }
}

export function toLast<S, A>(withLens: GeneralLens<S, A>, s: S): Either<string, A> {
  const asList = toArrayOf(withLens, s)
  if (asList.length > 0) {
    return right(asList[asList.length - 1])
  } else {
    return left('No values present.')
  }
}

export function inverseGet<S, A>(withLens: GeneralLens<S, A>, a: A): Either<string, S> {
  switch (withLens.type) {
    case 'ISO':
      return right(withLens.to(a))
    case 'LENS':
      return left(`Unable to invert a LENS.`)
    case 'PRISM':
      return right(withLens.to(a))
    case 'TRAVERSAL':
      return left(`Unable to invert a TRAVERSAL.`)
    default:
      assertNever(withLens)
  }
}

export function foldLOf<S, A, R>(
  withLens: GeneralLens<S, A>,
  fold: (r: R, a: A) => R,
  initialValue: R,
  s: S,
): R {
  return toArrayOf(withLens, s).reduce(fold, initialValue)
}

export function anyBy<S, A>(withLens: GeneralLens<S, A>, fromA: (a: A) => boolean, s: S): boolean {
  return foldLOf<S, A, boolean>(
    withLens,
    (r, a) => {
      return r || fromA(a)
    },
    false,
    s,
  )
}

export function any<S>(withLens: GeneralLens<S, boolean>, s: S): boolean {
  return anyBy(withLens, (b) => b, s)
}

export function allBy<S, A>(withLens: GeneralLens<S, A>, fromA: (a: A) => boolean, s: S): boolean {
  return foldLOf<S, A, boolean>(
    withLens,
    (r, a) => {
      return r && fromA(a)
    },
    true,
    s,
  )
}

export function all<S>(withLens: GeneralLens<S, boolean>, s: S): boolean {
  return allBy(withLens, (b) => b, s)
}

export function modify<S, A>(withLens: GeneralLens<S, A>, update: (a: A) => A, s: S): S {
  switch (withLens.type) {
    case 'ISO':
      return withLens.to(update(withLens.from(s)))
    case 'LENS':
      return withLens.update(s, update(withLens.from(s)))
    case 'PRISM':
      return foldEither(
        () => {
          return s
        },
        (a) => {
          return withLens.to(update(a))
        },
        withLens.from(s),
      )
    case 'TRAVERSAL':
      return withLens.update(s, update)
    default:
      assertNever(withLens)
  }
}

export function set<S, A>(withLens: GeneralLens<S, A>, newValue: A, s: S): S {
  switch (withLens.type) {
    case 'ISO':
      return withLens.to(newValue)
    case 'LENS':
      return withLens.update(s, newValue)
    case 'PRISM':
      return foldEither(
        () => {
          return s
        },
        () => {
          return withLens.to(newValue)
        },
        withLens.from(s),
      )
    case 'TRAVERSAL':
      return withLens.update(s, () => newValue)
    default:
      assertNever(withLens)
  }
}

export function exists<S, A>(withLens: GeneralLens<S, A>, s: S): boolean {
  switch (withLens.type) {
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
        withLens.from(s),
      )
    case 'TRAVERSAL':
      return withLens.from(s).length > 0
    default:
      assertNever(withLens)
  }
}

export function unsafeGet<S, A>(withLens: GeneralLens<S, A>, s: S): A {
  switch (withLens.type) {
    case 'ISO':
      return withLens.from(s)
    case 'LENS':
      return withLens.from(s)
    case 'PRISM':
      return foldEither(
        (err) => {
          throw new Error(`unsafeGet: Prism returned: ${err}`)
        },
        (a) => {
          return a
        },
        withLens.from(s),
      )
    case 'TRAVERSAL':
      const array = withLens.from(s)
      switch (array.length) {
        case 0:
          throw new Error(`unsafeGet: Traversal returned no results.`)
        case 1:
          return array[0]
        default:
          throw new Error(`unsafeGet: Traversal returned too many results.`)
      }
    default:
      assertNever(withLens)
  }
}
