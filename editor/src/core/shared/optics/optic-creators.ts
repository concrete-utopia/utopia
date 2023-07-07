import type { Optic } from './optics'
import { prism, lens, traversal, iso } from './optics'
import type { Either } from '../either'
import { right, left, foldEither } from '../either'

// Produces a prism from a type guard, so that only instances of `S` that are an `A` make it through.
export function fromTypeGuard<S, A extends S>(typeGuard: (s: S) => s is A): Optic<S, A> {
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

// Produces a lens down into a fixed field of a type as it may be defined in an interface.
export function fromField<S, K extends keyof S>(fieldName: K): Optic<S, S[K]> {
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

// Produces a traversal which points into a field of a dictionary, as it may or may not be present.
export function fromObjectField<A, S extends { [key: string]: A }>(fieldName: string): Optic<S, A> {
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

// Produces a traversal over a particular element of an array.
export function fromArrayIndex<A>(index: number): Optic<Array<A>, A> {
  return traversal(
    (array) => {
      if (index in array) {
        return [array[index]]
      } else {
        return []
      }
    },
    (array: Array<A>, modify: (a: A) => A) => {
      return array.map((arrayValue, arrayIndex) => {
        if (arrayIndex === index) {
          return modify(arrayValue)
        } else {
          return arrayValue
        }
      })
    },
  )
}

// Produces a traversal over an array, presenting each value of the array.
export function traverseArray<A>(): Optic<Array<A>, A> {
  return traversal(
    (a) => {
      return a
    },
    (array: Array<A>, modify: (a: A) => A) => {
      return array.map(modify)
    },
  )
}

// Produces a prism which only allows values which result in true being returned
// from the predicate supplied.
export function filtered<A>(predicate: (a: A) => boolean): Optic<A, A> {
  return prism(
    (a) => {
      if (predicate(a)) {
        return right(a)
      } else {
        return left(`Value fails the filter predicate.`)
      }
    },
    (a) => {
      return a
    },
  )
}

// Produces an iso which gives us the inverse value of a boolean.
export const not: Optic<boolean, boolean> = iso(
  (s) => !s,
  (a) => !a,
)

// Produces a prism into the left side of an `Either`.
export function eitherLeft<L, R>(): Optic<Either<L, R>, L> {
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

// Produces a prism into the right side of an `Either`.
export function eitherRight<L, R>(): Optic<Either<L, R>, R> {
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

// Produces a prism which excludes values which are `null`.
export function notNull<S>(): Optic<S | null, S> {
  return prism(
    (valueOrNull) => {
      if (valueOrNull === null) {
        return left(`Value was null.`)
      } else {
        return right(valueOrNull)
      }
    },
    (value) => value,
  )
}

// Produces a prism which excludes values which are `undefined`.
export function notUndefined<S>(): Optic<S | undefined, S> {
  return prism(
    (valueOrUndefined) => {
      if (valueOrUndefined === undefined) {
        return left(`Value was undefined.`)
      } else {
        return right(valueOrUndefined)
      }
    },
    (value) => value,
  )
}

// Produces a prism which excludes values which are `null` or `undefined`.
export function notNullOrUndefined<S>(): Optic<S | null | undefined, S> {
  return prism(
    (valueOrNullOrUndefined) => {
      if (valueOrNullOrUndefined == null) {
        return left(`Value was null or undefined.`)
      } else {
        return right(valueOrNullOrUndefined)
      }
    },
    (value) => value,
  )
}

// A no-op prism, which logs values as they travel through it in either direction.
export function logOptic<S>(): Optic<S, S> {
  return iso(
    (s) => {
      // eslint-disable-next-line no-console
      console.log('logOptic: from', JSON.stringify(s, null, 2))
      return s
    },
    (s) => {
      // eslint-disable-next-line no-console
      console.log('logOptic: to', JSON.stringify(s, null, 2))
      return s
    },
  )
}

// An identity lens which returns the new value on an update.
export function identityOptic<S>(): Optic<S, S> {
  return lens(
    (s) => {
      return s
    },
    (_, s) => {
      return s
    },
  )
}
