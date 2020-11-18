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

export type EqualityCheck<T> = (oldValue: T, newValue: T) => boolean

export type KeepDeepEqualityCall<T> = (oldValue: T, newValue: T) => KeepDeepEqualityResult<T>

export function combine2EqualityCalls<A, B, X>(
  getAValue: (x: X) => A,
  callA: KeepDeepEqualityCall<A>,
  getBValue: (x: X) => B,
  callB: KeepDeepEqualityCall<B>,
  combine: (a: A, b: B) => X,
): KeepDeepEqualityCall<X> {
  return (oldValue, newValue) => {
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

export function createCallWithTripleEquals<T>(): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const areEqual = oldValue === newValue
    return keepDeepEqualityResult(areEqual ? oldValue : newValue, areEqual)
  }
}

export function createCallFromEqualsFunction<T>(check: EqualityCheck<T>): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const areEqual = check(oldValue, newValue)
    return keepDeepEqualityResult(areEqual ? oldValue : newValue, areEqual)
  }
}

export function arrayDeepEquality<T>(
  elementCall: KeepDeepEqualityCall<T>,
): KeepDeepEqualityCall<Array<T>> {
  return (oldArray, newArray) => {
    let areEquals: boolean = true
    let workingResult: Array<T> = []
    if (oldArray === newArray) {
      return keepDeepEqualityResult(oldArray, true)
    } else {
      const length = newArray.length
      for (let arrayIndex = 0; arrayIndex < length; arrayIndex++) {
        const oldArrayElement = oldArray[arrayIndex]
        const newArrayElement = newArray[arrayIndex]
        const equalityResult = elementCall(oldArrayElement, newArrayElement)
        areEquals = areEquals && equalityResult.areEqual
        workingResult.push(equalityResult.value)
      }

      if (length === oldArray.length && areEquals) {
        return keepDeepEqualityResult(oldArray, true)
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
