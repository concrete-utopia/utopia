export interface This<A> {
  type: 'THIS'
  thisValue: A
}

export interface That<B> {
  type: 'THAT'
  thatValue: B
}

export interface ThisAndThat<A, B> {
  type: 'THISANDTHAT'
  thisValue: A
  thatValue: B
}

export type These<A, B> = This<A> | That<B> | ThisAndThat<A, B>

export function makeThis<A>(thisValue: A): This<A> {
  return {
    type: 'THIS',
    thisValue: thisValue,
  }
}

export function makeThat<B>(thatValue: B): That<B> {
  return {
    type: 'THAT',
    thatValue: thatValue,
  }
}

export function makeThisAndThat<A, B>(thisValue: A, thatValue: B): ThisAndThat<A, B> {
  return {
    type: 'THISANDTHAT',
    thisValue: thisValue,
    thatValue: thatValue,
  }
}

export function isThis<A, B>(these: These<A, B>): these is This<A> {
  return these.type === 'THIS'
}

export function isThat<A, B>(these: These<A, B>): these is That<B> {
  return these.type === 'THAT'
}

export function isThisAndThat<A, B>(these: These<A, B>): these is ThisAndThat<A, B> {
  return these.type === 'THISANDTHAT'
}

export function foldThese<A, B, X>(
  withThis: (thisValue: A) => X,
  withThat: (thatValue: B) => X,
  withThisAndThat: (thisValue: A, thatValue: B) => X,
  these: These<A, B>,
): X {
  switch (these.type) {
    case 'THIS':
      return withThis(these.thisValue)
    case 'THAT':
      return withThat(these.thatValue)
    case 'THISANDTHAT':
      return withThisAndThat(these.thisValue, these.thatValue)
    default:
      const _exhaustiveCheck: never = these
      throw new Error(`Unhandled type ${JSON.stringify(these)}`)
  }
}

export function setThis<A, B>(thisValue: A, these: These<A, B>): These<A, B> {
  return foldThese<A, B, These<A, B>>(
    (_) => makeThis(thisValue),
    (thatValue) => makeThisAndThat(thisValue, thatValue),
    (_, thatValue) => makeThisAndThat(thisValue, thatValue),
    these,
  )
}

export function setThat<A, B>(thatValue: B, these: These<A, B>): These<A, B> {
  return foldThese<A, B, These<A, B>>(
    (thisValue) => makeThisAndThat(thisValue, thatValue),
    (_) => makeThat(thatValue),
    (thisValue, _) => makeThisAndThat(thisValue, thatValue),
    these,
  )
}

export function thisMap<A1, A2, B>(transform: (a: A1) => A2, these: These<A1, B>): These<A2, B> {
  return foldThese<A1, B, These<A2, B>>(
    (thisValue) => makeThis(transform(thisValue)),
    (thatValue) => makeThat(thatValue),
    (thisValue, thatValue) => makeThisAndThat(transform(thisValue), thatValue),
    these,
  )
}

export function thatMap<A, B1, B2>(transform: (b: B1) => B2, these: These<A, B1>): These<A, B2> {
  return foldThese<A, B1, These<A, B2>>(
    (thisValue) => makeThis(thisValue),
    (thatValue) => makeThat(transform(thatValue)),
    (thisValue, thatValue) => makeThisAndThat(thisValue, transform(thatValue)),
    these,
  )
}

export function biMapThese<A1, B1, A2, B2>(
  thisTransform: (a: A1) => A2,
  thatTransform: (b: B1) => B2,
  these: These<A1, B1>,
): These<A2, B2> {
  return foldThese<A1, B1, These<A2, B2>>(
    (thisValue) => makeThis(thisTransform(thisValue)),
    (thatValue) => makeThat(thatTransform(thatValue)),
    (thisValue, thatValue) => makeThisAndThat(thisTransform(thisValue), thatTransform(thatValue)),
    these,
  )
}

export function mergeThese<A>(merge: (first: A, second: A) => A, these: These<A, A>): A {
  return foldThese<A, A, A>(
    (thisValue) => thisValue,
    (thatValue) => thatValue,
    merge,
    these,
  )
}
