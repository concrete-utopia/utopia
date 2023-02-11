import {
  defaultEither,
  Either,
  foldEither,
  traverseEither,
  mapEither,
  flatMapEither,
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
  get: (s: S) => A
  update: (s: S, a: A) => S
}

export function lens<S, A>(get: (s: S) => A, update: (s: S, a: A) => S): Lens<S, A> {
  return {
    type: 'LENS',
    get: get,
    update: update,
  }
}

export interface Prism<S, A> {
  type: 'PRISM'
  get: (s: S) => Either<string, A>
  to: (a: A) => S
}

export function prism<S, A>(get: (s: S) => Either<string, A>, to: (a: A) => S): Prism<S, A> {
  return {
    type: 'PRISM',
    get: get,
    to: to,
  }
}

export interface Traversal<S, A> {
  type: 'TRAVERSAL'
  get: (s: S) => Array<A>
  update: (s: S, modify: (a: A) => A) => S
}

export function traversal<S, A>(
  get: (s: S) => Array<A>,
  update: (s: S, modify: (a: A) => A) => S,
): Traversal<S, A> {
  return {
    type: 'TRAVERSAL',
    get: get,
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
          return second.get(first.from(a))
        },
        (a: A, c: C) => {
          return first.to(second.update(first.from(a), c))
        },
      )
    case 'PRISM':
      return prism(
        (a: A) => {
          return second.get(first.from(a))
        },
        (c: C) => {
          return first.to(second.to(c))
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return second.get(first.from(a))
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
          return second.from(first.get(a))
        },
        (a: A, c: C) => {
          return first.update(a, second.to(c))
        },
      )
    case 'LENS':
      return lens(
        (a: A) => {
          return second.get(first.get(a))
        },
        (a: A, c: C) => {
          return first.update(a, second.update(first.get(a), c))
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
            second.get(first.get(a)),
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
            second.get(first.get(a)),
          )
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return second.get(first.get(a))
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, second.update(first.get(a), modify))
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
          return mapEither(second.from, first.get(a))
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
              return [second.get(b)]
            },
            first.get(a),
          )
        },
        (a: A, modify: (c: C) => C) => {
          return foldEither(
            () => {
              return a
            },
            (b) => {
              return first.to(second.update(b, modify(second.get(b))))
            },
            first.get(a),
          )
        },
      )
    case 'PRISM':
      return prism(
        (a: A) => {
          return flatMapEither(second.get, first.get(a))
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
              return second.get(b)
            },
            first.get(a),
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
            first.get(a),
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
          return first.get(a).map(second.from)
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, (b) => second.to(modify(second.from(b))))
        },
      )
    case 'LENS':
      return traversal(
        (a: A) => {
          return first.get(a).map(second.get)
        },
        (a: A, modify: (c: C) => C) => {
          return first.update(a, (b) => second.update(b, modify(second.get(b))))
        },
      )
    case 'PRISM':
      return traversal(
        (a: A) => {
          const traversed = traverseEither(second.get, first.get(a))
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
              second.get(b),
            )
          })
        },
      )
    case 'TRAVERSAL':
      return traversal(
        (a: A) => {
          return first.get(a).flatMap(second.get)
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
