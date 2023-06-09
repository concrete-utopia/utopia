import { Either, foldEither, mapEither, flatMapEither, forEachRight } from '../either'
import { assertNever } from '../utils'

// An optic that represents an isomorphism between two types.
// Inspiration: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Iso.html#t:Iso
// For each and every value in both types there is a single
// value in the other type which it can be changed to.
export interface Iso<S, A> {
  type: 'ISO'
  from: (s: S) => A
  to: (a: A) => S
}

export function iso<S, A>(from: (s: S) => A, to: (a: A) => S): Optic<S, A> {
  return addComposeToRawOptic<S, A>({
    type: 'ISO',
    from: from,
    to: to,
  })
}

// An optic which represents a view into a part of a type, often in relation to product types.
// Inspiration: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Lens.html#t:Lens
// The value can always be retrieved from the original, but as it will only be a part, the
// original value is required to reconstruct it via the `update` function.
interface Lens<S, A> {
  type: 'LENS'
  from: (s: S) => A
  update: (s: S, a: A) => S
}

export function lens<S, A>(from: (s: S) => A, update: (s: S, a: A) => S): Optic<S, A> {
  return addComposeToRawOptic<S, A>({
    type: 'LENS',
    from: from,
    update: update,
  })
}

// An optic which represents a possible case of a value, often in relation to sum types.
// Inspiration: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Prism.html#t:Prism
// The value may not always be representable as the `A` case, but if we have a value of that type
// we can always get back to the original.
interface Prism<S, A> {
  type: 'PRISM'
  from: (s: S) => Either<string, A>
  to: (a: A) => S
}

export function prism<S, A>(from: (s: S) => Either<string, A>, to: (a: A) => S): Optic<S, A> {
  return addComposeToRawOptic<S, A>({
    type: 'PRISM',
    from: from,
    to: to,
  })
}

// An optic which can represent any case, going from a value to possibly many others.
// Inspiration: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Type.html#t:Traversal
// This has the weakest set of guarantees as it may not produce any values and always
// requires the original to do any updates.
interface Traversal<S, A> {
  type: 'TRAVERSAL'
  from: (s: S) => Array<A>
  update: (s: S, modify: (a: A) => A) => S
}

export function traversal<S, A>(
  from: (s: S) => Array<A>,
  update: (s: S, modify: (a: A) => A) => S,
): Optic<S, A> {
  return addComposeToRawOptic<S, A>({
    type: 'TRAVERSAL',
    from: from,
    update: update,
  })
}

export interface OpticCompose<S, A1> {
  compose<A2>(withOptic: Optic<A1, A2>): Optic<S, A2>
}

type RawOptic<S, A> = Iso<S, A> | Lens<S, A> | Prism<S, A> | Traversal<S, A>

export type Optic<S, A> = RawOptic<S, A> & OpticCompose<S, A>

function compose2Optics<A, B, C>(first: RawOptic<A, B>, second: RawOptic<B, C>): RawOptic<A, C> {
  switch (first.type) {
    case 'ISO':
      return composeOpticToIso(first, second)
    case 'LENS':
      return composeOpticToLens(first, second)
    case 'PRISM':
      return composeOpticToPrism(first, second)
    case 'TRAVERSAL':
      return composeOpticToTraversal(first, second)
    default:
      assertNever(first)
  }
}

function addComposeToRawOptic<S, A>(rawOptic: RawOptic<S, A>): Optic<S, A> {
  function compose<A2>(withOptic: Optic<A, A2>): Optic<S, A2> {
    return addComposeToRawOptic(compose2Optics(rawOptic, withOptic))
  }
  return {
    ...rawOptic,
    compose: compose,
  }
}

function composeOpticToIso<A, B, C>(first: Iso<A, B>, second: RawOptic<B, C>): RawOptic<A, C> {
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

function composeOpticToLens<A, B, C>(first: Lens<A, B>, second: RawOptic<B, C>): RawOptic<A, C> {
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

function composeOpticToPrism<A, B, C>(first: Prism<A, B>, second: RawOptic<B, C>): RawOptic<A, C> {
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

function composeOpticToTraversal<A, B, C>(
  first: Traversal<A, B>,
  second: RawOptic<B, C>,
): RawOptic<A, C> {
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
          let result: Array<C> = []
          for (const entry of first.from(a)) {
            const secondResult = second.from(entry)
            forEachRight(secondResult, (c) => {
              result.push(c)
            })
          }
          return result
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
