import React from 'react'
import fastDeepEqual from 'fast-deep-equal'
import { PRODUCTION_ENV } from '../common/env-vars'
import type { KeepDeepEqualityCall, KeepDeepEqualityResult } from './deep-equality'
import { keepDeepEqualityResult } from './deep-equality'
import { shallowEqual } from '../core/shared/equality-utils'

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
  if (typeof previousProps === 'object' && previousProps != null) {
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
  Component: React.FunctionComponent<React.PropsWithChildren<P>>,
): React.NamedExoticComponent<P>
export function failSafeReactMemo<T extends React.ComponentType<React.PropsWithChildren<any>>>(
  displayName: string,
  severity: 'strict' | 'gentle',
  Component: T,
): React.MemoExoticComponent<T>
export function failSafeReactMemo<T extends React.ComponentType<React.PropsWithChildren<any>>>(
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
    if (typeof previousProps === 'object' && previousProps != null) {
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

// this function has been adopted from https://github.com/epoberezkin/fast-deep-equal/tree/a33d49ab5cc659e331ff445109f35dd323230d41
function keepDeepReferenceEqualityInner(
  oldValue: any,
  possibleNewValue: any,
  stackSizeInner: number,
  valueStackSoFar: Set<any>,
): any {
  if (oldValue === possibleNewValue) return oldValue

  if (stackSizeInner > 100) {
    return possibleNewValue
  }

  // We appear to have looped back on ourselves,
  // escape by just returning the value.
  if (valueStackSoFar.has(possibleNewValue)) {
    return possibleNewValue
  }
  // mutation
  valueStackSoFar.add(possibleNewValue)

  if (
    oldValue != null &&
    possibleNewValue != null &&
    typeof oldValue == 'object' &&
    typeof possibleNewValue == 'object'
  ) {
    if (oldValue.constructor !== possibleNewValue.constructor) return possibleNewValue

    var length, i, entry, keys
    if (Array.isArray(oldValue)) {
      let newArrayToReturn: any[] = []
      let canSaveOldArray = true

      length = possibleNewValue.length
      if (length != oldValue.length) {
        canSaveOldArray = false
      }
      for (i = length; i-- !== 0; ) {
        // try to recurse into the array item here and save it if possible
        newArrayToReturn[i] = keepDeepReferenceEqualityInner(
          oldValue[i],
          possibleNewValue[i],
          stackSizeInner + 1,
          valueStackSoFar,
        )
        if (oldValue[i] !== newArrayToReturn[i]) {
          canSaveOldArray = false
        }
      }
      if (canSaveOldArray) {
        return oldValue
      } else {
        return newArrayToReturn
      }
    }

    if (oldValue instanceof Map && possibleNewValue instanceof Map) {
      let canSaveOldMap = true
      let newMapToReturn: Map<any, any> = new Map()

      if (oldValue.size !== possibleNewValue.size) {
        canSaveOldMap = false
      }
      for (entry of possibleNewValue.entries()) {
        const oldMapValue = oldValue.get(entry[0])
        const newMapValue = keepDeepReferenceEqualityInner(
          oldMapValue,
          entry[1],
          stackSizeInner + 1,
          valueStackSoFar,
        )
        newMapToReturn.set(entry[0], newMapValue)
        if (newMapValue !== oldMapValue) {
          canSaveOldMap = false
        }
      }
      if (canSaveOldMap) {
        return oldValue
      } else {
        return newMapToReturn
      }
    }

    if (oldValue instanceof Set && possibleNewValue instanceof Set) {
      /**
       * Sets use reference equality to determine if something is in the set or not,
       * which makes salvaging sub-values very hard. We don't attempt to do that here
       * */

      if (oldValue.size !== possibleNewValue.size) {
        return possibleNewValue
      }
      for (entry of oldValue.entries()) {
        if (!possibleNewValue.has(entry[0])) {
          return possibleNewValue
        }
      }
      return oldValue
    }

    if (ArrayBuffer.isView(oldValue) && ArrayBuffer.isView(possibleNewValue)) {
      /**
       * we don't use ArrayBufferViews in Utopia, so I'm not going to attempt salvaging the subvalues
       * typescript had the issues with length and index signature, I'm blindly trusting the original author here
       */
      length = (oldValue as any).length
      if (length != (possibleNewValue as any).length) return possibleNewValue
      for (i = length; i-- !== 0; )
        if ((oldValue as any)[i] !== (possibleNewValue as any)[i]) return possibleNewValue
      return oldValue
    }

    if (oldValue.constructor === RegExp) {
      if (
        oldValue.source === possibleNewValue.source &&
        oldValue.flags === possibleNewValue.flags
      ) {
        return oldValue
      } else {
        return possibleNewValue
      }
    }
    if (
      oldValue.valueOf !== Object.prototype.valueOf &&
      typeof oldValue.valueOf === 'function' &&
      typeof possibleNewValue.valueOf === 'function'
    ) {
      if (oldValue.valueOf() === possibleNewValue.valueOf()) {
        return oldValue
      } else {
        return possibleNewValue
      }
    }
    if (
      oldValue.toString !== Object.prototype.toString &&
      typeof oldValue.toString === 'function' &&
      typeof possibleNewValue.toString === 'function'
    ) {
      if (oldValue.toString() === possibleNewValue.toString()) {
        return oldValue
      } else {
        return possibleNewValue
      }
    }

    keys = Object.keys(possibleNewValue)
    const oldKeys = Object.keys(oldValue)
    length = keys.length

    let newObjectToReturn: any = {}
    let canSaveOldObject = true

    if (length !== Object.keys(oldValue).length) {
      canSaveOldObject = false
    }

    for (i = 0; i < length; i++) {
      var key = keys[i]
      if (key === undefined) {
        continue
      }

      if (key !== oldKeys[i]) {
        canSaveOldObject = false
      }

      if (key === '_owner' && oldValue.$$typeof != null) {
        // React-specific: avoid traversing React elements' _owner.
        //  _owner contains circular references
        // and is not needed when comparing the actual elements (and not their owners)
        newObjectToReturn[key] = possibleNewValue[key]
        continue
      }

      newObjectToReturn[key] = keepDeepReferenceEqualityInner(
        oldValue[key],
        possibleNewValue[key],
        stackSizeInner + 1,
        valueStackSoFar,
      )
      if (oldValue[key] !== newObjectToReturn[key]) {
        canSaveOldObject = false
      }
    }

    if (canSaveOldObject) {
      return oldValue
    } else {
      return newObjectToReturn
    }
  }

  // true if both NaN, false otherwise
  if (oldValue !== oldValue && possibleNewValue !== possibleNewValue) {
    return oldValue
  } else {
    return possibleNewValue
  }
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
  return keepDeepReferenceEqualityInner(oldValue, possibleNewValue, stackSize, new Set())
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

export function getIntrospectiveKeepDeepResult<T>(
  oldValue: T,
  newValue: T,
): KeepDeepEqualityResult<T> {
  const value = keepDeepReferenceEqualityIfPossible(oldValue, newValue)
  return keepDeepEqualityResult(value, value === oldValue)
}

export function createCallFromIntrospectiveKeepDeep<T>(): KeepDeepEqualityCall<T> {
  return getIntrospectiveKeepDeepResult
}

export function useKeepShallowReferenceEquality<T>(possibleNewValue: T, measure = false): T {
  const oldValue = React.useRef<T>(possibleNewValue)
  if (!shallowEqual(oldValue.current, possibleNewValue, measure)) {
    oldValue.current = possibleNewValue
  }
  return oldValue.current
}

export function useKeepReferenceEqualityIfPossible<T>(possibleNewValue: T, measure = false): T {
  const oldValue = React.useRef<T | null>(null)
  oldValue.current = keepDeepReferenceEqualityIfPossible(oldValue.current, possibleNewValue)
  return oldValue.current!
}

export function useKeepDeepEqualityCall<T>(newValue: T, keepDeepCall: KeepDeepEqualityCall<T>): T {
  const oldValue = React.useRef<T>(newValue)
  oldValue.current = keepDeepCall(oldValue.current, newValue).value
  return oldValue.current
}
