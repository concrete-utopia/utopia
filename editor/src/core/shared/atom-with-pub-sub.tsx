import * as PubSub from 'pubsub-js'
import * as React from 'react'
import { unstable_batchedUpdates } from 'react-dom'
import { useForceUpdate } from '../../components/editor/hook-utils'

export interface AtomWithPubSub<T> {
  key: string
  currentValue: T
  Provider: React.FunctionComponent<React.PropsWithChildren<{ value: T }>>
}

const GlobalAtomMap: { [key: string]: AtomWithPubSub<any> } = {}

export function atomWithPubSub<T>(options: { key: string; defaultValue: T }): AtomWithPubSub<T> {
  const { key, defaultValue } = options
  if (key in GlobalAtomMap) {
    throw new Error(`Tried to create multiple atoms with the same key: ${key}`)
  }
  const newAtom: AtomWithPubSub<T> = {
    key: key,
    currentValue: defaultValue,
    Provider: ({ children, value }) => {
      const updateAtom = usePubSubAtomWriteOnly(newAtom, true)
      // TODO ALSO immediately update the atom value
      setTimeout(() => {
        unstable_batchedUpdates(() => {
          updateAtom(() => value)
        })
      }, 0)
      return <>{children}</>
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

export function usePubSubAtomReadOnly<T>(atom: AtomWithPubSub<T>): T {
  const forceUpdate = useForceUpdate()
  useSubscribeToPubSubAtom(
    atom,
    React.useCallback(() => {
      // TODO only forceUpdate if the last returned atomValue does not equal the current one
      forceUpdate()
    }, [forceUpdate]),
  )
  return atom.currentValue
}

export function usePubSubAtomWriteOnly<T>(
  atom: AtomWithPubSub<T>,
  sync: boolean,
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
      // TODO add way to force update here, or maybe this check is not needed if the reader side runs an equality check
      if (atom.currentValue !== newValue) {
        atom.currentValue = newValue
        if (sync) {
          PubSub.publishSync(atom.key, newValue)
        } else {
          PubSub.publish(atom.key, newValue)
        }
      }
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [atom.key],
  )
}

export function usePubSubAtom<T>(
  atom: AtomWithPubSub<T>,
  pushSync: boolean,
): [T, (newValueOrUpdater: T | ((oldValue: T) => T)) => void] {
  return [usePubSubAtomReadOnly(atom), usePubSubAtomWriteOnly(atom, pushSync)]
}
