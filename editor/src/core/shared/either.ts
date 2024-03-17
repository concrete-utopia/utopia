// Often treated as the "failure" case.
export interface Left<L> {
  type: 'LEFT'
  value: L
}

// Often treated as the "success" case.
export interface Right<R> {
  type: 'RIGHT'
  value: R
}

// Usually treated as having bias to the right.
export type Either<L, R> = Left<L> | Right<R>

export function left<L, R>(value: L): Either<L, R> {
  return {
    type: 'LEFT',
    value: value,
  }
}

export function right<L, R>(value: R): Either<L, R> {
  return {
    type: 'RIGHT',
    value: value,
  }
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html#v:isLeft
export function isLeft<L, R>(either: Either<L, R>): either is Left<L> {
  return either.type === 'LEFT'
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html#v:isRight
export function isRight<L, R>(either: Either<L, R>): either is Right<R> {
  return either.type === 'RIGHT'
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html#v:either
export function foldEither<L, R, X>(
  foldLeft: (l: L) => X,
  foldRight: (r: R) => X,
  either: Either<L, R>,
): X {
  if (isLeft(either)) {
    return foldLeft(either.value)
  } else {
    return foldRight(either.value)
  }
}

export function defaultEither<L, R>(defaultValue: R, either: Either<L, R>): R {
  return foldEither(
    (_) => defaultValue,
    (r) => r,
    either,
  )
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html#v:-62--62--61-
export function flatMapEither<L, R1, R2>(
  transform: (r: R1) => Either<L, R2>,
  either: Either<L, R1>,
): Either<L, R2> {
  if (isLeft(either)) {
    return either
  } else {
    return transform(either.value)
  }
}

export const flatMap2Either = <L, R1, R2, R3>(
  transform: (r1: R1, r2: R2) => Either<L, R3>,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
): Either<L, R3> => joinEither(applicative2Either(transform, either1, either2))

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html#v:fmap
export function mapEither<L, R1, R2>(
  transform: (r: R1) => R2,
  either: Either<L, R1>,
): Either<L, R2> {
  if (isLeft(either)) {
    return either
  } else {
    return right(transform(either.value))
  }
}

export function leftMapEither<L1, L2, R>(
  transform: (l: L1) => L2,
  either: Either<L1, R>,
): Either<L2, R> {
  if (isLeft(either)) {
    return left(transform(either.value))
  } else {
    return either
  }
}
// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bifunctor.html#v:bimap
export function bimapEither<L1, L2, R1, R2>(
  transformLeft: (l: L1) => L2,
  transformRight: (r: R1) => R2,
  either: Either<L1, R1>,
): Either<L2, R2> {
  if (isLeft(either)) {
    return left(transformLeft(either.value))
  } else {
    return right(transformRight(either.value))
  }
}

export function eitherToMaybe<L, R>(either: Either<L, R>): R | null {
  if (isLeft(either)) {
    return null
  } else {
    return either.value
  }
}

export function maybeEitherToMaybe<L, R>(either: Either<L, R> | null | undefined): R | null {
  if (either == null || isLeft(either)) {
    return null
  } else {
    return either.value
  }
}

export function eitherFromMaybe<L, R>(leftDefault: L, maybe: R | null): Either<L, R> {
  if (maybe == null) {
    return left(leftDefault)
  } else {
    return right(maybe)
  }
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Traversable.html#v:sequence
export function sequenceEither<L, R>(eithers: Array<Either<L, R>>): Either<L, Array<R>> {
  let rightResults: Array<R> = []
  for (const either of eithers) {
    if (isLeft(either)) {
      return either
    } else {
      rightResults.push(either.value)
    }
  }
  return right(rightResults)
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Traversable.html#v:traverse
export function traverseEither<L, R, X>(
  transform: (x: X, i: number) => Either<L, R>,
  values: Array<X>,
): Either<L, Array<R>> {
  let rightResults: Array<R> = []
  for (var i = 0, len = values.length; i < len; i++) {
    const value = values[i]
    const result = transform(value, i)
    if (isLeft(result)) {
      return result
    } else {
      rightResults.push(result.value)
    }
  }
  return right(rightResults)
}

export function equalEither<L, R>(
  leftEqual: (l1: L, l2: L) => boolean,
  rightEqual: (r1: R, r2: R) => boolean,
  either1: Either<L, R>,
  either2: Either<L, R>,
): boolean {
  if (isLeft(either1)) {
    if (isLeft(either2)) {
      return leftEqual(either1.value, either2.value)
    } else {
      return false
    }
  } else {
    if (isLeft(either2)) {
      return false
    } else {
      return rightEqual(either1.value, either2.value)
    }
  }
}

export function alternativeEither<L, R>(first: Either<L, R>, second: Either<L, R>): Either<L, R> {
  if (isRight(first)) {
    return first
  } else {
    return second
  }
}

export function anyEither<L, R>(eithers: Array<Either<L, R>>): Either<L, R> {
  return eithers.reduce(alternativeEither)
}

// http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Foldable.html#v:foldlM
export function reduceWithEither<U, L, R>(
  reducer: (accumulator: R, currentValue: U, currentIndex: number) => Either<L, R>,
  initialAccumulator: R,
  listToReduce: ReadonlyArray<U>,
): Either<L, R> {
  return listToReduce.reduce((working, current, index) => {
    return flatMapEither((workingRight) => {
      return reducer(workingRight, current, index)
    }, working)
  }, right<L, R>(initialAccumulator))
}

export function applicative2Either<L, R1, R2, X>(
  transform: (r1: R1, r2: R2) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else {
    return right(transform(either1.value, either2.value))
  }
}

export function applicative3Either<L, R1, R2, R3, X>(
  transform: (r1: R1, r2: R2, r3: R3) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else {
    return right(transform(either1.value, either2.value, either3.value))
  }
}

export function applicative4Either<L, R1, R2, R3, R4, X>(
  transform: (r1: R1, r2: R2, r3: R3, r4: R4) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else {
    return right(transform(either1.value, either2.value, either3.value, either4.value))
  }
}

export function applicative5Either<L, R1, R2, R3, R4, R5, X>(
  transform: (r1: R1, r2: R2, r3: R3, r4: R4, r5: R5) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
  either5: Either<L, R5>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else if (isLeft(either5)) {
    return either5
  } else {
    return right(
      transform(either1.value, either2.value, either3.value, either4.value, either5.value),
    )
  }
}

export function applicative6Either<L, R1, R2, R3, R4, R5, R6, X>(
  transform: (r1: R1, r2: R2, r3: R3, r4: R4, r5: R5, r6: R6) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
  either5: Either<L, R5>,
  either6: Either<L, R6>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else if (isLeft(either5)) {
    return either5
  } else if (isLeft(either6)) {
    return either6
  } else {
    return right(
      transform(
        either1.value,
        either2.value,
        either3.value,
        either4.value,
        either5.value,
        either6.value,
      ),
    )
  }
}

export function applicative7Either<L, R1, R2, R3, R4, R5, R6, R7, X>(
  transform: (r1: R1, r2: R2, r3: R3, r4: R4, r5: R5, r6: R6, r7: R7) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
  either5: Either<L, R5>,
  either6: Either<L, R6>,
  either7: Either<L, R7>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else if (isLeft(either5)) {
    return either5
  } else if (isLeft(either6)) {
    return either6
  } else if (isLeft(either7)) {
    return either7
  } else {
    return right(
      transform(
        either1.value,
        either2.value,
        either3.value,
        either4.value,
        either5.value,
        either6.value,
        either7.value,
      ),
    )
  }
}

export function applicative8Either<L, R1, R2, R3, R4, R5, R6, R7, R8, X>(
  transform: (r1: R1, r2: R2, r3: R3, r4: R4, r5: R5, r6: R6, r7: R7, r8: R8) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
  either5: Either<L, R5>,
  either6: Either<L, R6>,
  either7: Either<L, R7>,
  either8: Either<L, R8>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else if (isLeft(either5)) {
    return either5
  } else if (isLeft(either6)) {
    return either6
  } else if (isLeft(either7)) {
    return either7
  } else if (isLeft(either8)) {
    return either8
  } else {
    return right(
      transform(
        either1.value,
        either2.value,
        either3.value,
        either4.value,
        either5.value,
        either6.value,
        either7.value,
        either8.value,
      ),
    )
  }
}

export function applicative9Either<L, R1, R2, R3, R4, R5, R6, R7, R8, R9, X>(
  transform: (r1: R1, r2: R2, r3: R3, r4: R4, r5: R5, r6: R6, r7: R7, r8: R8, r9: R9) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
  either5: Either<L, R5>,
  either6: Either<L, R6>,
  either7: Either<L, R7>,
  either8: Either<L, R8>,
  either9: Either<L, R9>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else if (isLeft(either5)) {
    return either5
  } else if (isLeft(either6)) {
    return either6
  } else if (isLeft(either7)) {
    return either7
  } else if (isLeft(either8)) {
    return either8
  } else if (isLeft(either9)) {
    return either9
  } else {
    return right(
      transform(
        either1.value,
        either2.value,
        either3.value,
        either4.value,
        either5.value,
        either6.value,
        either7.value,
        either8.value,
        either9.value,
      ),
    )
  }
}

export function applicative10Either<L, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, X>(
  transform: (
    r1: R1,
    r2: R2,
    r3: R3,
    r4: R4,
    r5: R5,
    r6: R6,
    r7: R7,
    r8: R8,
    r9: R9,
    r10: R10,
  ) => X,
  either1: Either<L, R1>,
  either2: Either<L, R2>,
  either3: Either<L, R3>,
  either4: Either<L, R4>,
  either5: Either<L, R5>,
  either6: Either<L, R6>,
  either7: Either<L, R7>,
  either8: Either<L, R8>,
  either9: Either<L, R9>,
  either10: Either<L, R10>,
): Either<L, X> {
  if (isLeft(either1)) {
    return either1
  } else if (isLeft(either2)) {
    return either2
  } else if (isLeft(either3)) {
    return either3
  } else if (isLeft(either4)) {
    return either4
  } else if (isLeft(either5)) {
    return either5
  } else if (isLeft(either6)) {
    return either6
  } else if (isLeft(either7)) {
    return either7
  } else if (isLeft(either8)) {
    return either8
  } else if (isLeft(either9)) {
    return either9
  } else if (isLeft(either10)) {
    return either10
  } else {
    return right(
      transform(
        either1.value,
        either2.value,
        either3.value,
        either4.value,
        either5.value,
        either6.value,
        either7.value,
        either8.value,
        either9.value,
        either10.value,
      ),
    )
  }
}

export function joinEither<L, R>(either: Either<L, Either<L, R>>): Either<L, R> {
  if (isLeft(either)) {
    return either
  } else {
    return either.value
  }
}

// Danger Will Robinson! This defeats the purpose of an Either. I've added this for the
// sake of testing AND ONLY FOR THE SAKE OF TESTING!
// If you use this for *ANYTHING* other than a test, I will personally hunt you down!
export function forceRight<L, R>(e: Either<L, R>, reason?: string): R {
  if (isLeft(e)) {
    throw new Error(reason ?? 'Unable to force either to a right' + ' - ' + JSON.stringify(e.value))
  } else {
    return e.value
  }
}

export function forEachRight<R>(e: Either<unknown, R>, forEach: (r: R) => void): void {
  if (isRight(e)) {
    forEach(e.value)
  }
}

export function forEachLeft<L>(e: Either<L, unknown>, forEach: (l: L) => void): void {
  if (isLeft(e)) {
    forEach(e.value)
  }
}

export function unwrapEither<R>(e: Either<unknown, R>, defaultValue: R): R {
  return isRight(e) ? e.value : defaultValue
}
