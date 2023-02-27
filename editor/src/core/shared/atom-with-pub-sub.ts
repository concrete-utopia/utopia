import * as PubSub from 'pubsub-js'
import React from 'react'
import { unstable_batchedUpdates } from 'react-dom'
import { HMR, IS_JEST_ENVIRONMENT } from '../../common/env-vars'
import { useForceUpdate } from '../../components/editor/hook-utils'

// From https://github.com/dai-shi/use-context-selector/blob/2dd334d727fc3b4cbadf7876b6ce64e0c633fd25/src/index.ts#L25
const isSSR =
  typeof window === 'undefined' ||
  // eslint-disable-next-line @typescript-eslint/strict-boolean-expressions
  /ServerSideRendering/.test(window.navigator && window.navigator.userAgent) ||
  IS_JEST_ENVIRONMENT

export const useIsomorphicLayoutEffect = isSSR ? React.useEffect : React.useLayoutEffect

export interface AtomWithPubSub<T> {
  key: string
  currentValue: T
  Provider: React.FunctionComponent<React.PropsWithChildren<React.PropsWithChildren<{ value: T }>>>
}

const GlobalAtomMap: { [key: string]: AtomWithPubSub<any> } = {}

export function atomWithPubSub<T>(options: { key: string; defaultValue: T }): AtomWithPubSub<T> {
  const { key, defaultValue } = options
  if (key in GlobalAtomMap && !HMR) {
    throw new Error(`Tried to create multiple atoms with the same key: ${key}`)
  }
  const newAtom: AtomWithPubSub<T> = {
    key: key,
    currentValue: defaultValue,
    Provider: ({ children, value }) => {
      const updateAtomSynchronously = usePubSubAtomWriteOnlyInner(newAtom, 'sync')
      newAtom.currentValue = value // TODO this is sneaky and we should use API instead
      useIsomorphicLayoutEffect(() => {
        unstable_batchedUpdates(() => {
          updateAtomSynchronously(() => value)
        })
      })
      return React.createElement(React.Fragment, {}, children)
    },
  }
  GlobalAtomMap[key] = newAtom
  return newAtom
}

export function useSubscribeToPubSubAtom<T>(
  atom: AtomWithPubSub<T>,
  referentiallyStableCallback: (newData: T) => void,
): void {
  const pubsubCallback = React.useCallback(
    (
      message: string,
      data: any, // TODO once eslint for hooks is updated, replace data: any with data: T
    ) => {
      referentiallyStableCallback(data)
    },
    [referentiallyStableCallback],
  )

  React.useEffect(() => {
    const token = PubSub.subscribe(atom.key, pubsubCallback)
    return function cleanup() {
      PubSub.unsubscribe(token)
    }
  }, [atom.key, pubsubCallback])
}

export const AlwaysTrue = (): boolean => true
export const AlwaysFalse = (): boolean => false

export function usePubSubAtomReadOnly<T>(
  atom: AtomWithPubSub<T>,
  shouldUpdateCallback: (newValue: T) => boolean,
): T {
  const forceUpdate = useForceUpdate()
  const previousValueRef = React.useRef(atom.currentValue)

  const shouldUpdateCallbackRef = React.useRef(shouldUpdateCallback)
  shouldUpdateCallbackRef.current = shouldUpdateCallback

  useSubscribeToPubSubAtom(
    atom,
    React.useCallback(() => {
      if (
        previousValueRef.current !== atom.currentValue &&
        shouldUpdateCallbackRef.current(atom.currentValue)
      ) {
        forceUpdate()
      }
    }, [forceUpdate, atom]),
  )
  previousValueRef.current = atom.currentValue
  return atom.currentValue
}

function usePubSubAtomWriteOnlyInner<T>(
  atom: AtomWithPubSub<T>,
  publishMode: 'sync' | 'async',
): (newValueOrUpdater: T | ((oldValue: T) => T)) => void {
  return React.useCallback(
    (newValueOrUpdater: T | ((oldValue: T) => T)) => {
      let newValue: T
      if (typeof newValueOrUpdater === 'function') {
        // if the new value is a function, we assume it is an updater
        newValue = (newValueOrUpdater as (oldValue: T) => T)(atom.currentValue)
      } else {
        newValue = newValueOrUpdater
      }
      atom.currentValue = newValue
      if (publishMode === 'sync') {
        PubSub.publishSync(atom.key, newValue)
      } else {
        PubSub.publish(atom.key, newValue)
      }
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [atom.key],
  )
}

export function usePubSubAtomWriteOnly<T>(
  atom: AtomWithPubSub<T>,
): (newValueOrUpdater: T | ((oldValue: T) => T)) => void {
  return usePubSubAtomWriteOnlyInner(atom, 'async')
}

export function usePubSubAtom<T>(
  atom: AtomWithPubSub<T>,
): [T, (newValueOrUpdater: T | ((oldValue: T) => T)) => void] {
  return [usePubSubAtomReadOnly(atom, AlwaysTrue), usePubSubAtomWriteOnly(atom)]
}
