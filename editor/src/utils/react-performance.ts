import * as React from 'react'
import * as fastDeepEqual from 'fast-deep-equal'
import { PRODUCTION_ENV } from '../common/env-vars'
import { colorTheme } from 'uuiui'

export function keepDeepReferenceEqualityIfPossible<T>(
  oldValue: T | null | undefined,
  possibleNewValue: T,
  stackSize?: number,
): T
export function keepDeepReferenceEqualityIfPossible(
  oldValue: any,
  possibleNewValue: any,
  stackSize: number = 0,
) {
  function deepReferenceEquality(
    oldValueInner: any,
    possibleNewValueInner: any,
    stackSizeInner: number,
    valueStackSoFar: Array<any>,
  ): any {
    // most of the actual structure is copy-pasted from https://github.com/epoberezkin/fast-deep-equal/blob/v2/index.js
    // I've made it return oldValue or newValue instead of true and false
    // and the recursion only stops if we get to return oldValue
    // otherwise we still drill into non-equal objects to try and find
    // keys that can be made equal

    // If we're more than 100 frames deep, let's just call it a day
    if (stackSizeInner > 100) {
      return possibleNewValueInner
    }

    if (oldValueInner == null) {
      return possibleNewValueInner
    }
    var isArray = Array.isArray
    var keyList = Object.keys
    var hasProp = Object.prototype.hasOwnProperty

    if (oldValueInner === possibleNewValueInner) return oldValueInner

    // We appear to have looped back on ourselves,
    // escape by just returning the value.
    if (valueStackSoFar.includes(possibleNewValueInner)) {
      return possibleNewValueInner
    }

    const newValueStack: Array<any> = [...valueStackSoFar, possibleNewValueInner]

    if (
      oldValueInner &&
      possibleNewValueInner &&
      typeof oldValueInner == 'object' &&
      typeof possibleNewValueInner == 'object'
    ) {
      const arrA = isArray(oldValueInner)
      const arrB = isArray(possibleNewValueInner)

      // reusable vars for all the performance
      let i: number = 0
      let length: number = 0
      let key: string = ''

      if (arrA && arrB) {
        length = possibleNewValueInner.length

        var newArrayToReturn: any[] = []
        var canSaveOldArray = true
        if (length != oldValueInner.length) {
          canSaveOldArray = false
        }
        for (i = length; i-- !== 0; ) {
          // try to recurse into the array item here and save it if possible
          newArrayToReturn[i] = deepReferenceEquality(
            oldValueInner[i],
            possibleNewValueInner[i],
            stackSizeInner + 1,
            newValueStack,
          )
          if (oldValueInner[i] !== newArrayToReturn[i]) {
            canSaveOldArray = false
          }
        }
        if (canSaveOldArray) {
          return oldValueInner
        } else {
          return newArrayToReturn
        }
      }

      if (arrA != arrB) return possibleNewValueInner

      var dateA = oldValueInner instanceof Date,
        dateB = possibleNewValueInner instanceof Date
      if (dateA != dateB) return possibleNewValueInner
      if (dateA && dateB)
        return oldValueInner.getTime() == possibleNewValueInner.getTime()
          ? oldValueInner
          : possibleNewValueInner

      var regexpA = oldValueInner instanceof RegExp,
        regexpB = possibleNewValueInner instanceof RegExp
      if (regexpA != regexpB) return possibleNewValueInner
      if (regexpA && regexpB)
        return oldValueInner.toString() == possibleNewValueInner.toString()
          ? oldValueInner
          : possibleNewValueInner

      // for objects we do a deep recursion
      var keys = keyList(possibleNewValueInner)
      length = keys.length

      var newObjectToReturn: any = {}
      var canSaveOldObject = true

      const oldKeys = keyList(oldValueInner)
      if (length !== oldKeys.length) {
        canSaveOldObject = false
      }

      for (i = 0; i < length; i++) {
        const newKey = keys[i]
        if (!hasProp.call(oldValueInner, newKey) || newKey !== oldKeys[i]) {
          canSaveOldObject = false
        }
      }

      for (i = 0; i < length; i++) {
        key = keys[i]
        newObjectToReturn[key] = deepReferenceEquality(
          oldValueInner[key],
          possibleNewValueInner[key],
          stackSizeInner + 1,
          newValueStack,
        )
        if (oldValueInner[key] !== newObjectToReturn[key]) {
          canSaveOldObject = false
        }
      }

      if (canSaveOldObject) {
        return oldValueInner
      } else {
        return newObjectToReturn
      }
    }

    return oldValueInner !== oldValueInner && possibleNewValueInner !== possibleNewValueInner
      ? oldValueInner
      : possibleNewValueInner
  }

  return deepReferenceEquality(oldValue, possibleNewValue, stackSize, [])
}

export function betterReactMemo<P extends unknown>(
  displayName: string,
  componentToMemo: React.FunctionComponent<unknown>,
  propsAreEqual?: (
    prevProps: Readonly<React.PropsWithChildren<unknown>>,
    nextProps: Readonly<React.PropsWithChildren<unknown>>,
  ) => boolean,
  whyDidYouRender: boolean = false,
) {
  componentToMemo.displayName = displayName
  ;(componentToMemo as any).whyDidYouRender = whyDidYouRender
  const memoized = React.memo(componentToMemo, propsAreEqual)
  memoized.displayName = displayName
  return memoized
}
