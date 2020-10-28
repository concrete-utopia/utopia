import * as React from 'react'
import * as fastDeepEqual from 'fast-deep-equal'
import { PRODUCTION_ENV } from '../common/env-vars'

export function useHookUpdateAnalysisStrictEquals<P>(name: string, newValue: P) {
  const previousValue = React.useRef(newValue)
  if (previousValue.current !== newValue) {
    console.warn(
      `Previous value for ${name} -- ${JSON.stringify(
        previousValue.current,
      )} is not the same as next props ${JSON.stringify(newValue)}.`,
    )
  }
  previousValue.current = newValue
}

export function useHookUpdateAnalysis<P>(newValue: P) {
  const previousValue = React.useRef(newValue)
  shouldComponentUpdateAnalysis(previousValue.current, newValue, {}, {})
  previousValue.current = newValue
  return newValue
}

export function shouldComponentUpdateAnalysis<P, S>(
  previousProps: P,
  nextProps: P,
  previousState: S,
  nextState: S,
): boolean {
  if (typeof previousProps === 'object') {
    if (previousProps === null) {
      if (nextProps !== null) {
        console.warn('Previous props was null, but next props is not.')
        return true
      }
    } else {
      if (nextProps === null) {
        console.warn('Previous props was not null, but next props is.')
        return true
      } else {
        let differingPropKeys: Array<string> = []
        for (const propKey of Object.keys(previousProps)) {
          const previousPropValue = (previousProps as any)[propKey]
          const nextPropValue = (nextProps as any)[propKey]
          if (previousPropValue !== nextPropValue) {
            differingPropKeys.push(propKey)
          }
        }
        if (differingPropKeys.length > 0) {
          console.warn(`Prop keys ${JSON.stringify(differingPropKeys)} have changed.`)
          return true
        }
      }
    }
  } else {
    // Not an object just check referential equality and roll the dice.
    if (previousProps !== nextProps) {
      console.warn(
        `Previous props ${JSON.stringify(
          previousProps,
        )} is not the same as next props ${JSON.stringify(nextProps)}.`,
      )
      return true
    }
  }

  if (typeof previousState === 'object') {
    if (previousState === null) {
      if (nextState !== null) {
        console.warn('Previous state was null, but next state is not.')
        return true
      }
    } else {
      if (nextState === null) {
        console.warn('Previous state was not null, but next state is.')
        return true
      } else {
        let differingStateKeys: Array<string> = []
        for (const stateKey of Object.keys(previousState)) {
          const previousStateValue = (previousState as any)[stateKey]
          const nextStateValue = (nextState as any)[stateKey]
          if (previousStateValue !== nextStateValue) {
            differingStateKeys.push(stateKey)
          }
        }
        if (differingStateKeys.length > 0) {
          console.warn(`State keys ${JSON.stringify(differingStateKeys)} have changed.`)
          return true
        }
      }
    }
  } else {
    // Not an object just check referential equality and roll the dice.
    if (previousState !== nextState) {
      console.warn(
        `Previous state ${JSON.stringify(
          previousState,
        )} is not the same as next state ${JSON.stringify(nextState)}.`,
      )
      return true
    }
  }

  console.warn('Nothing has changed.')
  return false
}

export function memoEqualityCheckAnalysis<P>(previousProps: P, nextProps: P): boolean {
  if (typeof previousProps === 'object') {
    let differingPropKeys: Array<string> = []
    for (const propKey of Object.keys(previousProps)) {
      const previousPropValue = (previousProps as any)[propKey]
      const nextPropValue = (nextProps as any)[propKey]
      if (previousPropValue !== nextPropValue) {
        differingPropKeys.push(propKey)
      }
    }
    if (differingPropKeys.length > 0) {
      console.warn(`Prop keys ${JSON.stringify(differingPropKeys)} have changed.`)
      return false
    }
  } else {
    // Not an object just check referential equality and roll the dice.
    if (previousProps !== nextProps) {
      console.warn(
        `Previous props ${JSON.stringify(
          previousProps,
        )} is not the same as next props ${JSON.stringify(nextProps)}.`,
      )
      return false
    }
  }

  return true
}

export function failSafeReactMemo<P extends Record<string, unknown>>(
  displayName: string,
  severity: 'strict' | 'gentle',
  Component: React.FunctionComponent<P>,
): React.NamedExoticComponent<P>
export function failSafeReactMemo<T extends React.ComponentType<any>>(
  displayName: string,
  severity: 'strict' | 'gentle',
  Component: T,
): React.MemoExoticComponent<T>
export function failSafeReactMemo<T extends React.ComponentType<any>>(
  displayName: string,
  severity: 'strict' | 'gentle',
  Component: T,
): React.MemoExoticComponent<T> {
  const memoizedComponent = React.memo(
    Component,
    failSafeMemoEqualityFunction(displayName, severity),
  )
  memoizedComponent.displayName = displayName
  return memoizedComponent
}

function failSafeMemoEqualityFunction(componentDisplayName: string, severity: 'strict' | 'gentle') {
  return function <P>(previousProps: P, nextProps: P): boolean {
    if (typeof previousProps === 'object') {
      let differingPropKeys: Array<string> = []
      let propKeysThatAreDifferentYetDeeplyEqual: Array<string> = []
      for (const propKey of Object.keys(previousProps)) {
        const previousPropValue = (previousProps as any)[propKey]
        const nextPropValue = (nextProps as any)[propKey]
        if (previousPropValue !== nextPropValue) {
          if (severity === 'strict') {
            if (!PRODUCTION_ENV) {
              if (fastDeepEqual(previousPropValue, nextPropValue)) {
                // set the editor on fire
                throw new Error(
                  `MEMOIZATION ERROR in ${componentDisplayName}: the prop ${propKey} lost referential equality, but it has the same value`,
                )
              }
            }
          } else {
            // in gentle mode, we fall back to fastDeepEqual as our equality function,
            // and print a console warning if we found a referentially unstable prop
            if (fastDeepEqual(previousPropValue, nextPropValue)) {
              // the two values are actually deep equal, so let's show an error message but otherwise continue the comparison
              propKeysThatAreDifferentYetDeeplyEqual.push(propKey)
            } else {
              // the two values really are different, push them to differingPropKeys
              differingPropKeys.push(propKey)
            }
          }
        }
      }
      if (propKeysThatAreDifferentYetDeeplyEqual.length > 0 && !PRODUCTION_ENV) {
        console.warn(
          `MEMOIZATION ERROR: ${componentDisplayName} Component received props which are deep equal but referentially unstable: ${JSON.stringify(
            propKeysThatAreDifferentYetDeeplyEqual,
          )}`,
        )
      }
      if (differingPropKeys.length > 0) {
        return false
      }
    } else {
      // Not an object just check referential equality and roll the dice.
      if (previousProps !== nextProps) {
        return false
      }
    }

    return true
  }
}

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
  return deepReferenceEquality(oldValue, possibleNewValue, stackSize, []) as any
}

/**
 * Flasher hook.
 */
export function useFlasher<T extends HTMLElement>() {
  const ref = React.useRef<T | null>(null)
  React.useEffect(() => {
    if (ref.current != null) {
      ref.current.setAttribute(
        'style',
        `box-shadow: 0 0 8px 1px #FF00FF;
         background-color: #FF00FFBB;
         transition: box-shadow 50ms ease-out;`,
      )
      setTimeout(() => ref.current!.setAttribute('style', ''), 100)
    }
  })
  return ref
}

export function betterReactMemo<P extends Record<string, any>>(
  displayName: string,
  componentToMemo: React.FunctionComponent<P>,
  propsAreEqual?: (
    prevProps: Readonly<React.PropsWithChildren<P>>,
    nextProps: Readonly<React.PropsWithChildren<P>>,
  ) => boolean,
  whyDidYouRender: boolean = false,
) {
  componentToMemo.displayName = displayName
  ;(componentToMemo as any).whyDidYouRender = whyDidYouRender
  const memoized = React.memo(componentToMemo, propsAreEqual)
  memoized.displayName = displayName
  return memoized
}
