import { prism, lens, traversal, iso, GeneralLens } from './lens'
import { right, left, foldEither, Either } from './either'
import { assertNever } from './utils'

export function fromTypeGuard<S, A extends S>(typeGuard: (s: S) => s is A): GeneralLens<S, A> {
  return prism(
    (s) => {
      if (typeGuard(s)) {
        return right(s)
      } else {
        return left('Failed typeguard.')
      }
    },
    (a) => {
      return a
    },
  )
}

export function fromField<S, K extends keyof S>(fieldName: K): GeneralLens<S, S[K]> {
  return lens(
    (s) => {
      return s[fieldName]
    },
    (s, a) => {
      return {
        ...s,
        [fieldName]: a,
      }
    },
  )
}

export function fromObjectField<A, S extends { [key: string]: A }>(
  fieldName: string,
): GeneralLens<S, A> {
  return traversal(
    (s: S) => {
      if (fieldName in s) {
        return [s[fieldName]]
      } else {
        return []
      }
    },
    (s: S, modify: (a: A) => A) => {
      if (fieldName in s) {
        return {
          ...s,
          [fieldName]: modify(s[fieldName]),
        }
      } else {
        return s
      }
    },
  )
}

export function traverseArray<A>(): GeneralLens<Array<A>, A> {
  return traversal(
    (a) => {
      return a
    },
    (array: Array<A>, modify: (a: A) => A) => {
      return array.map(modify)
    },
  )
}

export function filtered<A>(filter: (a: A) => boolean): GeneralLens<A, A> {
  return prism(
    (a) => {
      if (filter(a)) {
        return right(a)
      } else {
        return left(`Value fails the filte predicate.`)
      }
    },
    (a) => {
      return a
    },
  )
}

export const not: GeneralLens<boolean, boolean> = iso(
  (s) => !s,
  (a) => !a,
)

export function eitherLeft<L, R>(): GeneralLens<Either<L, R>, L> {
  return prism((either) => {
    return foldEither(
      (l) => {
        return right(l)
      },
      () => {
        return left(`Either is not a left.`)
      },
      either,
    )
  }, left)
}

export function eitherRight<L, R>(): GeneralLens<Either<L, R>, R> {
  return prism((either) => {
    return foldEither(
      () => {
        return left(`Either is not a right.`)
      },
      (r) => {
        return right(r)
      },
      either,
    )
  }, right)
}
