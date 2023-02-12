import {
  defaultEither,
  Either,
  foldEither,
  traverseEither,
  mapEither,
  flatMapEither,
  right,
  left,
} from '../core/shared/either'
import { assertNever } from '../core/shared/utils'

export interface Iso<S, A> {
  type: 'ISO'
  from: (s: S) => A
  to: (a: A) => S
}

export function iso<S, A>(from: (s: S) => A, to: (a: A) => S): Iso<S, A> {
  return {
    type: 'ISO',
    from: from,
    to: to,
  }
}

export interface Lens<S, A> {
  type: 'LENS'
  from: (s: S) => A
  update: (s: S, a: A) => S
}

export function lens<S, A>(from: (s: S) => A, update: (s: S, a: A) => S): Lens<S, A> {
  return {
    type: 'LENS',
    from: from,
    update: update,
  }
}

export interface Prism<S, A> {
  type: 'PRISM'
  from: (s: S) => Either<string, A>
  to: (a: A) => S
}

export function prism<S, A>(from: (s: S) => Either<string, A>, to: (a: A) => S): Prism<S, A> {
  return {
    type: 'PRISM',
    from: from,
    to: to,
  }
}

export interface Traversal<S, A> {
  type: 'TRAVERSAL'
  from: (s: S) => Array<A>
  update: (s: S, modify: (a: A) => A) => S
}

export function traversal<S, A>(
  from: (s: S) => Array<A>,
  update: (s: S, modify: (a: A) => A) => S,
): Traversal<S, A> {
  return {
    type: 'TRAVERSAL',
    from: from,
    update: update,
  }
}

export type GeneralLens<S, A> = Iso<S, A> | Lens<S, A> | Prism<S, A> | Traversal<S, A>

export function composeLensToIso<A, B, C>(
  first: Iso<A, B>,
  second: GeneralLens<B, C>,
): GeneralLens<A, C> {
  switch (second.type) {
    case 'ISO':
      return iso(
        (a: A) => {
          return second.from(first.from(a))
        },
        (c: C) => {
          return first.to(second.to(c))
        },
      )
    case 'LENS':
      return lens(
        (a: A) => {
          return second.from(first.from(a))
        },
        (a: A, c: C) => {
          return first.to(second.update(first.from(a), c))
        },
      )
    case 'PRISM':
      return prism(
        (a: A) => {
          return second.from(first.from(a))
        },
        (c: C) => {
          return first.to(second.to(c))
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return second.from(first.from(a))
        },
        (a: A, modify: (c: C) => C) => {
          return first.to(second.update(first.from(a), modify))
        },
      )
    default:
      assertNever(second)
  }
}

export function composeLensToLens<A, B, C>(
  first: Lens<A, B>,
  second: GeneralLens<B, C>,
): GeneralLens<A, C> {
  switch (second.type) {
    case 'ISO':
      return lens(
        (a: A) => {
          return second.from(first.from(a))
        },
        (a: A, c: C) => {
          return first.update(a, second.to(c))
        },
      )
    case 'LENS':
      return lens(
        (a: A) => {
          return second.from(first.from(a))
        },
        (a: A, c: C) => {
          return first.update(a, second.update(first.from(a), c))
        },
      )
    case 'PRISM':
      return traversal(
        (a: A) => {
          return foldEither(
            () => {
              return []
            },
            (c) => {
              return [c]
            },
            second.from(first.from(a)),
          )
        },
        (a: A, modify: (c: C) => C) => {
          return foldEither(
            () => {
              return a
            },
            (c) => {
              return first.update(a, second.to(modify(c)))
            },
            second.from(first.from(a)),
          )
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return second.from(first.from(a))
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, second.update(first.from(a), modify))
        },
      )
    default:
      assertNever(second)
  }
}

export function composeLensToPrism<A, B, C>(
  first: Prism<A, B>,
  second: GeneralLens<B, C>,
): GeneralLens<A, C> {
  switch (second.type) {
    case 'ISO':
      return prism(
        (a: A) => {
          return mapEither(second.from, first.from(a))
        },
        (c: C) => {
          return first.to(second.to(c))
        },
      )
    case 'LENS':
      return traversal(
        (a: A) => {
          return foldEither(
            () => {
              return []
            },
            (b) => {
              return [second.from(b)]
            },
            first.from(a),
          )
        },
        (a: A, modify: (c: C) => C) => {
          return foldEither(
            () => {
              return a
            },
            (b) => {
              return first.to(second.update(b, modify(second.from(b))))
            },
            first.from(a),
          )
        },
      )
    case 'PRISM':
      return prism(
        (a: A) => {
          return flatMapEither(second.from, first.from(a))
        },
        (c: C) => {
          return first.to(second.to(c))
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return foldEither(
            () => {
              return []
            },
            (b) => {
              return second.from(b)
            },
            first.from(a),
          )
        },
        (a: A, modify: (c: C) => C) => {
          return foldEither(
            () => {
              return a
            },
            (b) => {
              return first.to(second.update(b, modify))
            },
            first.from(a),
          )
        },
      )
    default:
      assertNever(second)
  }
}

export function composeLensToTraversal<A, B, C>(
  first: Traversal<A, B>,
  second: GeneralLens<B, C>,
): GeneralLens<A, C> {
  switch (second.type) {
    case 'ISO':
      return traversal(
        (a: A) => {
          return first.from(a).map(second.from)
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, (b) => second.to(modify(second.from(b))))
        },
      )
    case 'LENS':
      return traversal(
        (a: A) => {
          return first.from(a).map(second.from)
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, (b) => second.update(b, modify(second.from(b))))
        },
      )
    case 'PRISM':
      return traversal(
        (a: A) => {
          const traversed = traverseEither(second.from, first.from(a))
          return defaultEither([], traversed)
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, (b) => {
            return foldEither(
              () => {
                return b
              },
              (c) => {
                return second.to(modify(c))
              },
              second.from(b),
            )
          })
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return first.from(a).flatMap(second.from)
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, (b) => {
            return second.update(b, modify)
          })
        },
      )
    default:
      assertNever(second)
  }
}

export function compose2Lenses<A, B, C>(
  first: GeneralLens<A, B>,
  second: GeneralLens<B, C>,
): GeneralLens<A, C> {
  switch (first.type) {
    case 'ISO':
      return composeLensToIso(first, second)
    case 'LENS':
      return composeLensToLens(first, second)
    case 'PRISM':
      return composeLensToPrism(first, second)
    case 'TRAVERSAL':
      return composeLensToTraversal(first, second)
    default:
      assertNever(first)
  }
}

export function compose3Lenses<A, B, C, D>(
  first: GeneralLens<A, B>,
  second: GeneralLens<B, C>,
  third: GeneralLens<C, D>,
): GeneralLens<A, D> {
  return compose2Lenses(compose2Lenses(first, second), third)
}

export function compose4Lenses<A, B, C, D, E>(
  first: GeneralLens<A, B>,
  second: GeneralLens<B, C>,
  third: GeneralLens<C, D>,
  fourth: GeneralLens<D, E>,
): GeneralLens<A, E> {
  return compose2Lenses(compose3Lenses(first, second, third), fourth)
}

export function compose5Lenses<A, B, C, D, E, F>(
  first: GeneralLens<A, B>,
  second: GeneralLens<B, C>,
  third: GeneralLens<C, D>,
  fourth: GeneralLens<D, E>,
  fifth: GeneralLens<E, F>,
): GeneralLens<A, F> {
  return compose2Lenses(compose4Lenses(first, second, third, fourth), fifth)
}

export function compose6Lenses<A, B, C, D, E, F, G>(
  first: GeneralLens<A, B>,
  second: GeneralLens<B, C>,
  third: GeneralLens<C, D>,
  fourth: GeneralLens<D, E>,
  fifth: GeneralLens<E, F>,
  sixth: GeneralLens<F, G>,
): GeneralLens<A, G> {
  return compose2Lenses(compose5Lenses(first, second, third, fourth, fifth), sixth)
}

export function compose7Lenses<A, B, C, D, E, F, G, H>(
  first: GeneralLens<A, B>,
  second: GeneralLens<B, C>,
  third: GeneralLens<C, D>,
  fourth: GeneralLens<D, E>,
  fifth: GeneralLens<E, F>,
  sixth: GeneralLens<F, G>,
  seventh: GeneralLens<G, H>,
): GeneralLens<A, H> {
  return compose2Lenses(compose6Lenses(first, second, third, fourth, fifth, sixth), seventh)
}

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

export function toListOf<S, A>(withLens: GeneralLens<S, A>, s: S): Array<A> {
  switch (withLens.type) {
    case 'ISO':
      return [withLens.from(s)]
    case 'LENS':
      return [withLens.from(s)]
    case 'PRISM':
      return foldEither(
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
  const asList = toListOf(withLens, s)
  if (asList.length > 0) {
    return right(asList[0])
  } else {
    return left('No values present.')
  }
}

export function toLast<S, A>(withLens: GeneralLens<S, A>, s: S): Either<string, A> {
  const asList = toListOf(withLens, s)
  if (asList.length > 0) {
    return right(asList[asList.length - 1])
  } else {
    return left('No values present.')
  }
}
