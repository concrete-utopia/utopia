import { makeOptic, type Optic } from './optics'
import type { Either } from '../either'
import { right, left, foldEither, forEachLeft, forEachRight } from '../either'

// Produces a prism from a type guard, so that only instances of `S` that are an `A` make it through.
export function fromTypeGuard<S, A extends S>(typeGuard: (s: S) => s is A): Optic<S, A> {
  return makeOptic(
    (s, f) => {
      if (typeGuard(s)) {
        f(s)
      }
    },
    (s, modify) => {
      if (typeGuard(s)) {
        return modify(s)
      } else {
        return s
      }
    },
  )
}

// Produces a lens down into a fixed field of a type as it may be defined in an interface.
export function fromField<S, K extends keyof S>(fieldName: K): Optic<S, S[K]> {
  return makeOptic(
    (s, f) => {
      f(s[fieldName])
    },
    (s, modify) => {
      return {
        ...s,
        [fieldName]: modify(s[fieldName]),
      }
    },
  )
}

// Produces a traversal which points into a field of a dictionary, as it may or may not be present.
export function fromObjectField<A, S extends { [key: string]: A }>(fieldName: string): Optic<S, A> {
  return makeOptic(
    (s, f) => {
      if (fieldName in s) {
        f(s[fieldName])
      }
    },
    (s, modify) => {
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
  return makeOptic(
    (s, f) => {
      if (index < s.length) {
        f(s[index])
      }
    },
    (s, modify) => {
      return s.map((a, i) => {
        if (i === index) {
          return modify(a)
        } else {
          return a
        }
      })
    },
  )
}

export function pairTupleFirst<A1, A2>(): Optic<[A1, A2], A1> {
  return makeOptic(
    ([a1, _a2], f) => {
      f(a1)
    },
    ([a1, a2], modify) => {
      return [modify(a1), a2]
    },
  )
}

export function pairTupleSecond<A1, A2>(): Optic<[A1, A2], A2> {
  return makeOptic(
    ([_a1, a2], f) => {
      f(a2)
    },
    ([a1, a2], modify) => {
      return [a1, modify(a2)]
    },
  )
}

// Produces a traversal over an array, presenting each value of the array.
export function traverseArray<A>(): Optic<Array<A>, A> {
  return makeOptic(
    (s, f) => {
      for (const a of s) {
        f(a)
      }
    },
    (s, modify) => {
      return s.map(modify)
    },
  )
}

// Produces a traversal over the values of an object.
export function objectValues<A>(): Optic<{ [key: string]: A }, A> {
  return makeOptic(
    (s, f) => {
      for (const key in s) {
        f(s[key])
      }
    },
    (s, modify) => {
      let result: { [key: string]: A } = {}
      for (const [key, value] of Object.entries(s)) {
        result[key] = modify(value)
      }
      return result
    },
  )
}

// Produces a traversal over the keys and values of an object.
export function objectEntries<A>(): Optic<{ [key: string]: A }, [string, A]> {
  return makeOptic(
    (s, f) => {
      for (const key in s) {
        f([key, s[key]])
      }
    },
    (s, modify) => {
      let result: { [key: string]: A } = {}
      for (const [key, value] of Object.entries(s)) {
        const [newKey, newValue] = modify([key, value])
        result[newKey] = newValue
      }
      return result
    },
  )
}

// Produces a prism which only allows values which result in true being returned
// from the predicate supplied.
export function filtered<A>(predicate: (a: A) => boolean): Optic<A, A> {
  return makeOptic(
    (s, f) => {
      if (predicate(s)) {
        f(s)
      }
    },
    (s, modify) => {
      if (predicate(s)) {
        return modify(s)
      } else {
        return s
      }
    },
  )
}

// Produces an iso which gives us the inverse value of a boolean.
export const not: Optic<boolean, boolean> = makeOptic(
  (s, f) => {
    f(!s)
  },
  (s, modify) => {
    return !modify(!s)
  },
)

// Produces a prism into the left side of an `Either`.
export function eitherLeft<L, R>(): Optic<Either<L, R>, L> {
  return makeOptic(
    (either, f) => {
      forEachLeft(either, f)
    },
    (either, modify) => {
      return foldEither(
        (l) => {
          return left(modify(l))
        },
        (r) => {
          return right(r)
        },
        either,
      )
    },
  )
}

// Produces a prism into the right side of an `Either`.
export function eitherRight<L, R>(): Optic<Either<L, R>, R> {
  return makeOptic(
    (either, f) => {
      forEachRight(either, f)
    },
    (either, modify) => {
      return foldEither(
        (l) => {
          return left(l)
        },
        (r) => {
          return right(modify(r))
        },
        either,
      )
    },
  )
}

// Produces a prism which excludes values which are `null`.
export function notNull<S>(): Optic<S | null, S> {
  return makeOptic(
    (valueOrNull, f) => {
      if (valueOrNull !== null) {
        f(valueOrNull)
      }
    },
    (value) => value,
  )
}

// Produces a prism which excludes values which are `undefined`.
export function notUndefined<S>(): Optic<S | undefined, S> {
  return makeOptic(
    (valueOrUndefined, f) => {
      if (valueOrUndefined !== undefined) {
        f(valueOrUndefined)
      }
    },
    (value) => value,
  )
}

// Produces a prism which excludes values which are `null` or `undefined`.
export function notNullOrUndefined<S>(): Optic<S | null | undefined, S> {
  return makeOptic(
    (valueOrNullUndefined, f) => {
      if (valueOrNullUndefined !== null && valueOrNullUndefined !== undefined) {
        f(valueOrNullUndefined)
      }
    },
    (value) => value,
  )
}

// A no-op prism, which logs values as they travel through it in either direction.
export function logOptic<S>(): Optic<S, S> {
  return makeOptic(
    (s, f) => {
      // eslint-disable-next-line no-console
      console.log('Log: ', s)
      f(s)
    },
    (s, modify) => {
      // eslint-disable-next-line no-console
      console.log('Log: ', s)
      return modify(s)
    },
  )
}

// An identity lens which returns the new value on an update.
export function identityOptic<S>(): Optic<S, S> {
  return makeOptic(
    (s, f) => {
      f(s)
    },
    (s, modify) => {
      return modify(s)
    },
  )
}
