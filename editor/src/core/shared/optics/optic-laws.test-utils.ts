import * as FastCheck from 'fast-check'
import type { Optic } from './optics'
import fastDeepEquals from 'fast-deep-equal'
import { flatMapEither, foldEither, right, traverseEither } from '../either'
import { toArrayOf, modify, unsafeGet, set, toFirst } from './optic-utilities'

function traversalIdentityLaw<S, A>(lens: Optic<S, A>, arbitraryS: FastCheck.Arbitrary<S>): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Type.html#t:Traversal
  // Performing an update with the identity function should be the same as making no change at all.
  const property = FastCheck.property(arbitraryS, (s) => {
    return fastDeepEquals(
      modify(lens, (a) => a, s),
      s,
    )
  })
  FastCheck.assert(property, { verbose: true })
}

function traversalCompositionLaw<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  func1: (a: A) => A,
  func2: (a: A) => A,
): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Type.html#t:Traversal
  // Doing two updates with two functions should be the same as doing a single update with those functions
  // composed together.
  const property = FastCheck.property(arbitraryS, (s) => {
    return fastDeepEquals(
      modify(lens, func1, modify(lens, func2, s)),
      modify(lens, (a) => func1(func2(a)), s),
    )
  })
  FastCheck.assert(property, { verbose: true })
}

export function traversalLaws<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  func1: (a: A) => A,
  func2: (a: A) => A,
): void {
  traversalIdentityLaw(lens, arbitraryS)
  traversalCompositionLaw(lens, arbitraryS, func1, func2)
}

function lensSetGetLaw<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  arbitraryA: FastCheck.Arbitrary<A>,
): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Lens.html#t:Lens
  // Getting the value `a` from an `s` after setting it should return the original value `a`.
  const property = FastCheck.property(FastCheck.tuple(arbitraryS, arbitraryA), ([s, a]) => {
    return fastDeepEquals(unsafeGet(lens, set(lens, a, s)), a)
  })
  FastCheck.assert(property, { verbose: true })
}

function lensGetSetLaw<S, A>(lens: Optic<S, A>, arbitraryS: FastCheck.Arbitrary<S>): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Lens.html#t:Lens
  // Setting a value `a` that was taken from a `s` back into it should be the same as making no change at all.
  const property = FastCheck.property(arbitraryS, (s) => {
    return fastDeepEquals(set(lens, unsafeGet(lens, s), s), s)
  })
  FastCheck.assert(property, { verbose: true })
}

function lensSetSetLaw<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  arbitraryA: FastCheck.Arbitrary<A>,
): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Lens.html#t:Lens
  // Setting a value twice with a lens, should be the same as setting it once.
  const property = FastCheck.property(FastCheck.tuple(arbitraryS, arbitraryA), ([s, a]) => {
    return fastDeepEquals(set(lens, a, set(lens, a, s)), set(lens, a, s))
  })
  FastCheck.assert(property, { verbose: true })
}

function lensTraversalLengthLaw<S, A>(lens: Optic<S, A>, arbitraryS: FastCheck.Arbitrary<S>): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Lens.html#t:Lens
  // With a lens used to return the array, it should always return an array of length 1.
  const property = FastCheck.property(arbitraryS, (s) => {
    const arrayLength = toArrayOf(lens, s).length
    return arrayLength === 1
  })
  FastCheck.assert(property, { verbose: true })
}

export function lensLaws<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  arbitraryA: FastCheck.Arbitrary<A>,
  func1: (a: A) => A,
  func2: (a: A) => A,
): void {
  lensSetGetLaw(lens, arbitraryS, arbitraryA)
  lensGetSetLaw(lens, arbitraryS)
  lensSetSetLaw(lens, arbitraryS, arbitraryA)
  lensTraversalLengthLaw(lens, arbitraryS)
  // A lens is also valid as a traversal.
  traversalLaws(lens, arbitraryS, func1, func2)
}

function prismTraversalLengthLaw<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
): void {
  // Law taken from here: https://hackage.haskell.org/package/lens-5.2/docs/Control-Lens-Prism.html#t:Prism
  // Getting the values from a prism as an array should only ever return an array of length 0 or 1.
  const property = FastCheck.property(arbitraryS, (s) => {
    const arrayLength = toArrayOf(lens, s).length
    return arrayLength === 0 || arrayLength === 1
  })
  FastCheck.assert(property, { verbose: true })
}

export function prismLaws<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  arbitraryA: FastCheck.Arbitrary<A>,
  func1: (a: A) => A,
  func2: (a: A) => A,
): void {
  prismTraversalLengthLaw(lens, arbitraryS)
  // A prism is also valid as a traversal.
  traversalLaws(lens, arbitraryS, func1, func2)
}

export function isoLaws<S, A>(
  lens: Optic<S, A>,
  arbitraryS: FastCheck.Arbitrary<S>,
  arbitraryA: FastCheck.Arbitrary<A>,
  func1: (a: A) => A,
  func2: (a: A) => A,
): void {
  // An iso is also valid as both a prism and a lens.
  prismLaws(lens, arbitraryS, arbitraryA, func1, func2)
  lensLaws(lens, arbitraryS, arbitraryA, func1, func2)
}
