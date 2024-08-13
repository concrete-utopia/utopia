// TODO Move either.ts here?

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
