import { keepDeepReferenceEqualityIfPossible } from '../../utils/react-performance'

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

export function createCallFromEqualsFunction<T>(check: EqualityCheck<T>): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const areEqual = check(oldValue, newValue)
    return keepDeepEqualityResult(areEqual ? oldValue : newValue, areEqual)
  }
}

export function createCallFromIntrospectiveKeepDeep<T>(): KeepDeepEqualityCall<T> {
  return (oldValue, newValue) => {
    const value = keepDeepReferenceEqualityIfPossible(oldValue, newValue)
    return keepDeepEqualityResult(value, value === oldValue)
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
