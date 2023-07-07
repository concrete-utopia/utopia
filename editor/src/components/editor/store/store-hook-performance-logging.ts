import React from 'react'
import { PERFORMANCE_MARKS_ALLOWED } from '../../../common/env-vars'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import type { StateSelector } from './store-hook'
import type { StoreKey, Substates } from './store-hook-substore-types'

export function resetSelectorTimings(): void {
  SelectorTimings.current = {}
  SubstoreTimings.current = {}
}

const SelectorTimings: {
  current: {
    [selectorName: string]: {
      accumulatedTime: number
      calledNumberOfTimes: number
      substateName: string
    }
  }
} = { current: {} }

function logging_ensureSelectorTimingExists(selectorName: string, substoreKey: string) {
  if (
    isFeatureEnabled('Debug – Measure Selectors') &&
    SelectorTimings.current[selectorName] == null
  ) {
    SelectorTimings.current[selectorName] = {
      accumulatedTime: 0,
      calledNumberOfTimes: 0,
      substateName: substoreKey,
    }
  }
}

const SubstoreTimings: {
  current: {
    [storeKey: string]: {
      beginTimeStamp?: number | null
      accumulatedTime: number | null
      calledNumberOfTimes: number
    }
  }
} = { current: {} }

export function logging_ensureSubstoreTimingExists(storeKey: string): void {
  if (isFeatureEnabled('Debug – Measure Selectors') && SubstoreTimings.current[storeKey] == null) {
    SubstoreTimings.current[storeKey] = {
      beginTimeStamp: null,
      accumulatedTime: null,
      calledNumberOfTimes: 0,
    }
  }
}

export function logBeforeStoreUpdate(key: keyof Substates): void {
  const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')
  if (MeasureSelectors) {
    logging_ensureSubstoreTimingExists(key)
    SubstoreTimings.current[key].beginTimeStamp = performance.now()
  }
}

export function logAfterStoreUpdate(key: keyof Substates): void {
  const startTimestamp = SubstoreTimings.current[key]?.beginTimeStamp
  if (startTimestamp != null) {
    SubstoreTimings.current[key].accumulatedTime =
      (SubstoreTimings.current[key].accumulatedTime ?? 0) + performance.now() - startTimestamp
  }
}

export function useWrapSelectorInPerformanceMeasureBlock<K extends StoreKey, U>(
  storeKey: K,
  selector: StateSelector<Substates[K], U>,
  selectorName: string,
): StateSelector<Substates[K], U> {
  const previousSelectorRef = React.useRef<StateSelector<Substates[K], U>>()
  const previousWrappedSelectorRef = React.useRef<StateSelector<Substates[K], U>>()

  if (selector === previousSelectorRef.current && previousWrappedSelectorRef.current != null) {
    // we alreaedy wrapped this selector
    return previousWrappedSelectorRef.current
  } else {
    // let's create a new wrapped selector
    const wrappedSelector = (state: Substates[K]): U => {
      const LogSelectorPerformance =
        isFeatureEnabled('Debug – Performance Marks (Slow)') && PERFORMANCE_MARKS_ALLOWED

      const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')

      if (LogSelectorPerformance) {
        window.performance.mark('selector_begin')
      }

      const beforeSelector = MeasureSelectors ? performance.now() : 0

      let calledNumberOfTimes = 1

      // Uncomment this to stress-test selectors
      // if (isFeatureEnabled('Debug – Measure Selectors')) {
      //   for (let index = 0; index < 99; index++) {
      //     calledNumberOfTimes++
      //     selector(state)
      //   }
      // }

      const result = selector(state)
      if (LogSelectorPerformance) {
        window.performance.mark('selector_end')
        window.performance.measure(
          `Zustand Selector ${selectorName}`,
          'selector_begin',
          'selector_end',
        )
      }

      const afterSelector = MeasureSelectors ? performance.now() : 0

      if (MeasureSelectors) {
        logging_ensureSelectorTimingExists(selectorName, storeKey)
        const currentValue = SelectorTimings.current[selectorName]
        currentValue.accumulatedTime += afterSelector - beforeSelector
        currentValue.calledNumberOfTimes += calledNumberOfTimes
        SelectorTimings.current[selectorName] = currentValue
        logging_ensureSubstoreTimingExists(storeKey)
        SubstoreTimings.current[storeKey].calledNumberOfTimes += calledNumberOfTimes
      }

      return result
    }
    previousSelectorRef.current = selector
    previousWrappedSelectorRef.current = wrappedSelector
    return wrappedSelector
  }
}

export function useWrapCallbackInPerformanceMeasureBlock<K extends StoreKey, U>(
  storeKey: K,
  callbackRef: React.MutableRefObject<(slice: U) => void>,
  selectorName: string,
  previouslySelectedStateRef: React.MutableRefObject<U>,
  equalityFnRef: React.MutableRefObject<(oldSlice: U, newSlice: U) => boolean>,
  explainMe: boolean,
): (slice: U) => void {
  return React.useCallback<(newSlice: U) => void>(
    (newSlice) => {
      const MeasureSelectors = isFeatureEnabled('Debug – Measure Selectors')
      const beforeCallback = MeasureSelectors ? performance.now() : 0
      // innerCallback is called by Zustand and also by us, to make sure everything is correct we run our own equality check before calling the user's callback here
      if (!equalityFnRef.current(previouslySelectedStateRef.current, newSlice)) {
        if (explainMe) {
          console.info(
            'selected state has a new value according to the provided equalityFn, notifying callback',
            newSlice,
          )
        }
        callbackRef.current(newSlice)
        previouslySelectedStateRef.current = newSlice
      }
      const afterCallback = MeasureSelectors ? performance.now() : 0
      if (MeasureSelectors) {
        logging_ensureSelectorTimingExists(selectorName, storeKey)
        SelectorTimings.current[selectorName].accumulatedTime += afterCallback - beforeCallback
        SelectorTimings.current[selectorName].calledNumberOfTimes += 1
      }
    },
    [storeKey, explainMe, selectorName, callbackRef, previouslySelectedStateRef, equalityFnRef],
  )
}

export function logSelectorTimings(phase: string): void {
  // eslint-disable-next-line no-console
  console.log(
    `Number of Selectors called during ${phase}`,
    Object.keys(SelectorTimings.current).length,
  )
  // eslint-disable-next-line no-console
  console.table(SelectorTimings.current)
  // eslint-disable-next-line no-console
  console.log(`Pre-Selectors during ${phase}`)
  // eslint-disable-next-line no-console
  console.table(SubstoreTimings.current, ['accumulatedTime', 'calledNumberOfTimes'])
}
