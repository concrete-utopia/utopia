import { shallowEqual } from '../core/shared/equality-utils'
import { fastForEach } from '../core/shared/utils'
import type { ComplexMap, ComplexMapValue } from './map'
import { complexMapValue } from './map'
import { keepDeepReferenceEqualityIfPossible } from './react-performance'

export interface KeepDeepEqualityResult<T> {
  value: T
  areEqual: boolean
}

export function keepDeepEqualityResult<T>(value: T, areEqual: boolean): KeepDeepEqualityResult<T> {
  return {
    value: value,
    areEqual: areEqual,
  }
}

export function mapKeepDeepEqualityResult<T, U>(
  transform: (t: T) => U,
  result: KeepDeepEqualityResult<T>,
): KeepDeepEqualityResult<U> {
  return keepDeepEqualityResult(transform(result.value), result.areEqual)
}

export type EqualityCheck<T> = (oldValue: T, newValue: T) => boolean

export type KeepDeepEqualityCall<T> = (oldValue: T, newValue: T) => KeepDeepEqualityResult<T>

export function combine1EqualityCall<A, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  combine: (a: A) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const areEqual = resultA.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(resultA.value)
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine2EqualityCalls<A, B, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  combine: (a: A, b: B) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const areEqual = resultA.areEqual && resultB.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(resultA.value, resultB.value)
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine3EqualityCalls<A, B, C, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  combine: (a: A, b: B, c: C) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const areEqual = resultA.areEqual && resultB.areEqual && resultC.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(resultA.value, resultB.value, resultC.value)
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine4EqualityCalls<A, B, C, D, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  combine: (a: A, b: B, c: C, d: D) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const areEqual = resultA.areEqual && resultB.areEqual && resultC.areEqual && resultD.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(resultA.value, resultB.value, resultC.value, resultD.value)
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine5EqualityCalls<A, B, C, D, E, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  combine: (a: A, b: B, c: C, d: D, e: E) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine6EqualityCalls<A, B, C, D, E, F, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine7EqualityCalls<A, B, C, D, E, F, G, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine8EqualityCalls<A, B, C, D, E, F, G, H, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine9EqualityCalls<A, B, C, D, E, F, G, H, I, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine10EqualityCalls<A, B, C, D, E, F, G, H, I, J, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine11EqualityCalls<A, B, C, D, E, F, G, H, I, J, K, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  getKValue: (x: X) => K,
  callK: KeepDeepEqualityCall<K>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const resultK = callK(getKValue(oldValue), getKValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual &&
      resultK.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
        resultK.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine12EqualityCalls<A, B, C, D, E, F, G, H, I, J, K, L, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  getKValue: (x: X) => K,
  callK: KeepDeepEqualityCall<K>,
  getLValue: (x: X) => L,
  callL: KeepDeepEqualityCall<L>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const resultK = callK(getKValue(oldValue), getKValue(newValue))
    const resultL = callL(getLValue(oldValue), getLValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual &&
      resultK.areEqual &&
      resultL.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
        resultK.value,
        resultL.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine13EqualityCalls<A, B, C, D, E, F, G, H, I, J, K, L, M, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  getKValue: (x: X) => K,
  callK: KeepDeepEqualityCall<K>,
  getLValue: (x: X) => L,
  callL: KeepDeepEqualityCall<L>,
  getMValue: (x: X) => M,
  callM: KeepDeepEqualityCall<M>,
  combine: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const resultK = callK(getKValue(oldValue), getKValue(newValue))
    const resultL = callL(getLValue(oldValue), getLValue(newValue))
    const resultM = callM(getMValue(oldValue), getMValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual &&
      resultK.areEqual &&
      resultL.areEqual &&
      resultM.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
        resultK.value,
        resultL.value,
        resultM.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine14EqualityCalls<A, B, C, D, E, F, G, H, I, J, K, L, M, N, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  getKValue: (x: X) => K,
  callK: KeepDeepEqualityCall<K>,
  getLValue: (x: X) => L,
  callL: KeepDeepEqualityCall<L>,
  getMValue: (x: X) => M,
  callM: KeepDeepEqualityCall<M>,
  getNValue: (x: X) => N,
  callN: KeepDeepEqualityCall<N>,
  combine: (
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
    f: F,
    g: G,
    h: H,
    i: I,
    j: J,
    k: K,
    l: L,
    m: M,
    n: N,
  ) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const resultK = callK(getKValue(oldValue), getKValue(newValue))
    const resultL = callL(getLValue(oldValue), getLValue(newValue))
    const resultM = callM(getMValue(oldValue), getMValue(newValue))
    const resultN = callN(getNValue(oldValue), getNValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual &&
      resultK.areEqual &&
      resultL.areEqual &&
      resultM.areEqual &&
      resultN.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
        resultK.value,
        resultL.value,
        resultM.value,
        resultN.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine15EqualityCalls<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  getKValue: (x: X) => K,
  callK: KeepDeepEqualityCall<K>,
  getLValue: (x: X) => L,
  callL: KeepDeepEqualityCall<L>,
  getMValue: (x: X) => M,
  callM: KeepDeepEqualityCall<M>,
  getNValue: (x: X) => N,
  callN: KeepDeepEqualityCall<N>,
  getOValue: (x: X) => O,
  callO: KeepDeepEqualityCall<O>,
  combine: (
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
    f: F,
    g: G,
    h: H,
    i: I,
    j: J,
    k: K,
    l: L,
    m: M,
    n: N,
    o: O,
  ) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const resultK = callK(getKValue(oldValue), getKValue(newValue))
    const resultL = callL(getLValue(oldValue), getLValue(newValue))
    const resultM = callM(getMValue(oldValue), getMValue(newValue))
    const resultN = callN(getNValue(oldValue), getNValue(newValue))
    const resultO = callO(getOValue(oldValue), getOValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual &&
      resultK.areEqual &&
      resultL.areEqual &&
      resultM.areEqual &&
      resultN.areEqual &&
      resultO.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
        resultK.value,
        resultL.value,
        resultM.value,
        resultN.value,
        resultO.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function combine16EqualityCalls<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  getCValue: (x: X) => C,
  callC: KeepDeepEqualityCall<C>,
  getDValue: (x: X) => D,
  callD: KeepDeepEqualityCall<D>,
  getEValue: (x: X) => E,
  callE: KeepDeepEqualityCall<E>,
  getFValue: (x: X) => F,
  callF: KeepDeepEqualityCall<F>,
  getGValue: (x: X) => G,
  callG: KeepDeepEqualityCall<G>,
  getHValue: (x: X) => H,
  callH: KeepDeepEqualityCall<H>,
  getIValue: (x: X) => I,
  callI: KeepDeepEqualityCall<I>,
  getJValue: (x: X) => J,
  callJ: KeepDeepEqualityCall<J>,
  getKValue: (x: X) => K,
  callK: KeepDeepEqualityCall<K>,
  getLValue: (x: X) => L,
  callL: KeepDeepEqualityCall<L>,
  getMValue: (x: X) => M,
  callM: KeepDeepEqualityCall<M>,
  getNValue: (x: X) => N,
  callN: KeepDeepEqualityCall<N>,
  getOValue: (x: X) => O,
  callO: KeepDeepEqualityCall<O>,
  getPValue: (x: X) => P,
  callP: KeepDeepEqualityCall<P>,
  combine: (
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
    f: F,
    g: G,
    h: H,
    i: I,
    j: J,
    k: K,
    l: L,
    m: M,
    n: N,
    o: O,
    p: P,
  ) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }

    const resultA = callA(getAValue(oldValue), getAValue(newValue))
    const resultB = callB(getBValue(oldValue), getBValue(newValue))
    const resultC = callC(getCValue(oldValue), getCValue(newValue))
    const resultD = callD(getDValue(oldValue), getDValue(newValue))
    const resultE = callE(getEValue(oldValue), getEValue(newValue))
    const resultF = callF(getFValue(oldValue), getFValue(newValue))
    const resultG = callG(getGValue(oldValue), getGValue(newValue))
    const resultH = callH(getHValue(oldValue), getHValue(newValue))
    const resultI = callI(getIValue(oldValue), getIValue(newValue))
    const resultJ = callJ(getJValue(oldValue), getJValue(newValue))
    const resultK = callK(getKValue(oldValue), getKValue(newValue))
    const resultL = callL(getLValue(oldValue), getLValue(newValue))
    const resultM = callM(getMValue(oldValue), getMValue(newValue))
    const resultN = callN(getNValue(oldValue), getNValue(newValue))
    const resultO = callO(getOValue(oldValue), getOValue(newValue))
    const resultP = callP(getPValue(oldValue), getPValue(newValue))
    const areEqual =
      resultA.areEqual &&
      resultB.areEqual &&
      resultC.areEqual &&
      resultD.areEqual &&
      resultE.areEqual &&
      resultF.areEqual &&
      resultG.areEqual &&
      resultH.areEqual &&
      resultI.areEqual &&
      resultJ.areEqual &&
      resultK.areEqual &&
      resultL.areEqual &&
      resultM.areEqual &&
      resultN.areEqual &&
      resultO.areEqual &&
      resultP.areEqual
    if (areEqual) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      const value = combine(
        resultA.value,
        resultB.value,
        resultC.value,
        resultD.value,
        resultE.value,
        resultF.value,
        resultG.value,
        resultH.value,
        resultI.value,
        resultJ.value,
        resultK.value,
        resultL.value,
        resultM.value,
        resultN.value,
        resultO.value,
        resultP.value,
      )
      return keepDeepEqualityResult(value, false)
    }
  }
}

export function createCallWithTripleEquals<T>(): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const areEqual = oldValue === newValue
    return keepDeepEqualityResult(areEqual ? oldValue : newValue, areEqual)
  }
}

export const StringKeepDeepEquality: KeepDeepEqualityCall<string> =
  createCallWithTripleEquals<string>()

export const NullableStringKeepDeepEquality: KeepDeepEqualityCall<string | null> =
  createCallWithTripleEquals<string | null>()

export const NumberKeepDeepEquality: KeepDeepEqualityCall<number> =
  createCallWithTripleEquals<number>()

export const NullableNumberKeepDeepEquality: KeepDeepEqualityCall<number | null> =
  createCallWithTripleEquals<number | null>()

export const BooleanKeepDeepEquality: KeepDeepEqualityCall<boolean> =
  createCallWithTripleEquals<boolean>()

export const NullableBooleanKeepDeepEquality: KeepDeepEqualityCall<boolean | null> =
  createCallWithTripleEquals<boolean | null>()

export function createCallWithDeepEquals<T>(): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const keepDeepResult = keepDeepReferenceEqualityIfPossible(oldValue, newValue)
    if (keepDeepResult === oldValue) {
      return keepDeepEqualityResult(oldValue, true)
    } else {
      return keepDeepEqualityResult(keepDeepResult, false)
    }
  }
}

export function createCallWithShallowEquals<T>(): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const areEqual = shallowEqual(oldValue, newValue)
    return keepDeepEqualityResult(areEqual ? oldValue : newValue, areEqual)
  }
}

export function createCallFromEqualsFunction<T>(check: EqualityCheck<T>): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const areEqual = check(oldValue, newValue)
    return keepDeepEqualityResult(areEqual ? oldValue : newValue, areEqual)
  }
}

export function readOnlyArrayDeepEquality<T>(
  elementCall: KeepDeepEqualityCall<T>,
): KeepDeepEqualityCall<ReadonlyArray<T>> {
  return (oldArray, newArray) => {
    if (oldArray === newArray) {
      return keepDeepEqualityResult(oldArray, true)
    } else {
      let areEquals: boolean = true
      let workingResult: Array<T> = []
      const length = newArray.length
      const oldLength = oldArray.length
      for (let arrayIndex = 0; arrayIndex < length; arrayIndex++) {
        // Assume this value exists because of the bounds checking done in the for loop.
        const newArrayElement = newArray[arrayIndex]!
        if (arrayIndex < oldLength) {
          // Assume this value exists because of the bounds check done on the line above.
          const oldArrayElement = oldArray[arrayIndex]!
          const equalityResult = elementCall(oldArrayElement, newArrayElement)
          areEquals = areEquals && equalityResult.areEqual
          workingResult.push(equalityResult.value)
        } else {
          areEquals = false
          workingResult.push(newArrayElement)
        }
      }

      if (length === oldArray.length && areEquals) {
        return keepDeepEqualityResult(oldArray, true)
      } else {
        return keepDeepEqualityResult(workingResult, false)
      }
    }
  }
}

export function arrayDeepEquality<T>(
  elementCall: KeepDeepEqualityCall<T>,
): KeepDeepEqualityCall<Array<T>> {
  return (oldArray, newArray) => {
    if (oldArray === newArray) {
      return keepDeepEqualityResult(oldArray, true)
    } else {
      let areEquals: boolean = true
      let workingResult: Array<T> = []
      const length = newArray.length
      const oldLength = oldArray.length
      for (let arrayIndex = 0; arrayIndex < length; arrayIndex++) {
        // Assume this value exists because of the bounds checking done in the for loop.
        const newArrayElement = newArray[arrayIndex]!
        if (arrayIndex < oldLength) {
          // Assume this value exists because of the bounds check done on the line above.
          const oldArrayElement = oldArray[arrayIndex]!
          const equalityResult = elementCall(oldArrayElement, newArrayElement)
          areEquals = areEquals && equalityResult.areEqual
          workingResult.push(equalityResult.value)
        } else {
          areEquals = false
          workingResult.push(newArrayElement)
        }
      }

      if (length === oldArray.length && areEquals) {
        return keepDeepEqualityResult(oldArray, true)
      } else {
        return keepDeepEqualityResult(workingResult, false)
      }
    }
  }
}

export function objectDeepEquality<T>(
  valueCall: KeepDeepEqualityCall<T>,
): KeepDeepEqualityCall<Record<string, T>> {
  return (oldObject, newObject) => {
    if (oldObject === newObject) {
      return keepDeepEqualityResult(oldObject, true)
    } else {
      let areEquals: boolean = true
      let workingResult: Record<string, T> = {}
      const newObjectEntries = Object.entries(newObject)
      for (const [newObjectKey, newObjectValue] of newObjectEntries) {
        if (newObjectKey in oldObject) {
          // The `in` check proves the value definitely exists.
          const oldObjectValue = oldObject[newObjectKey]!
          const valueResult = valueCall(oldObjectValue, newObjectValue)
          workingResult[newObjectKey] = valueResult.value
          areEquals = areEquals && valueResult.areEqual
        } else {
          workingResult[newObjectKey] = newObjectValue
          areEquals = false
        }
      }
      // Check if keys have been removed.
      if (areEquals) {
        const oldObjectKeys = Object.keys(oldObject)
        if (oldObjectKeys.length !== newObjectEntries.length) {
          areEquals = false
        }
      }

      if (areEquals) {
        return keepDeepEqualityResult(oldObject, true)
      } else {
        return keepDeepEqualityResult(workingResult, false)
      }
    }
  }
}

export function nullableDeepEquality<T>(
  elementCall: KeepDeepEqualityCall<T>,
): KeepDeepEqualityCall<T | null> {
  return (oldNullableValue, newNullableValue) => {
    if (oldNullableValue === null) {
      if (newNullableValue === null) {
        return keepDeepEqualityResult(oldNullableValue, true)
      } else {
        return keepDeepEqualityResult(newNullableValue, false)
      }
    } else {
      if (newNullableValue === null) {
        return keepDeepEqualityResult(newNullableValue, false)
      } else {
        return elementCall(oldNullableValue, newNullableValue)
      }
    }
  }
}

export function undefinableDeepEquality<T>(
  elementCall: KeepDeepEqualityCall<T>,
): KeepDeepEqualityCall<T | undefined> {
  return (oldUndefinableValue, newUndefinableValue) => {
    if (oldUndefinableValue === undefined) {
      if (newUndefinableValue === undefined) {
        return keepDeepEqualityResult(oldUndefinableValue, true)
      } else {
        return keepDeepEqualityResult(newUndefinableValue, false)
      }
    } else {
      if (newUndefinableValue === undefined) {
        return keepDeepEqualityResult(newUndefinableValue, false)
      } else {
        return elementCall(oldUndefinableValue, newUndefinableValue)
      }
    }
  }
}

export function unionDeepEquality<T1, T2>(
  elementCall1: KeepDeepEqualityCall<T1>,
  elementCall2: KeepDeepEqualityCall<T2>,
  typeGuardT1: (param: T1 | T2) => param is T1,
  typeGuardT2: (param: T1 | T2) => param is T2,
): KeepDeepEqualityCall<T1 | T2> {
  return <T extends T1 | T2>(oldValue: T, newValue: T) => {
    if (oldValue === newValue) {
      return keepDeepEqualityResult(oldValue, true)
    }
    if (typeGuardT1(oldValue) && typeGuardT1(newValue)) {
      return elementCall1(oldValue, newValue)
    } else if (typeGuardT2(oldValue) && typeGuardT2(newValue)) {
      return elementCall2(oldValue, newValue)
    } else {
      return keepDeepEqualityResult(newValue, false)
    }
  }
}

// Makes the assumption that the key value stored is consistent with the key value string used to key into the dictionary.
export function ComplexMapKeepDeepEquality<K, V>(
  keyDeepEquality: KeepDeepEqualityCall<K>,
  valueDeepEquality: KeepDeepEqualityCall<V>,
): KeepDeepEqualityCall<ComplexMap<K, V>> {
  const mapValueDeepEquality: KeepDeepEqualityCall<ComplexMapValue<K, V>> = combine2EqualityCalls(
    (value) => value.key,
    keyDeepEquality,
    (value) => value.value,
    valueDeepEquality,
    complexMapValue,
  )
  return objectDeepEquality(mapValueDeepEquality)
}
