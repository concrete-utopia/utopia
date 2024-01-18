import React from 'react'
import { useAtomCallback } from 'jotai/utils'
import type { Atom } from 'jotai'

export function useValueResetState<T>(
  defaultValue: T,
  ...resetArgs: Array<any>
): [T, (overrideValue: T) => void] {
  const [localState, setLocalState] = React.useState(defaultValue)
  React.useEffect(() => {
    setLocalState(defaultValue)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [defaultValue, ...resetArgs])
  return [localState, setLocalState]
}

export function usePrevious<T>(currentValue: T): T | undefined {
  const previousRef = React.useRef<T | undefined>(undefined)

  const prev = previousRef.current

  previousRef.current = currentValue

  return prev
}

export function useForceUpdate() {
  const [, forceUpdate] = React.useReducer((c) => c + 1, 0)
  return forceUpdate
}

export function useInputFocusOnCountIncrease<T extends { focus: () => void }>(
  triggerCount: number,
): React.RefObject<T> {
  const ref = React.useRef<T>(null)
  const previousTriggerCountRef = React.useRef(triggerCount)
  if (previousTriggerCountRef.current !== triggerCount) {
    previousTriggerCountRef.current = triggerCount
    ref.current?.focus()
  }
  return ref
}

export class Getter<T> {
  public getter: (() => T) | null = null

  get current(): T {
    if (this.getter == null) {
      throw new Error('Getter.getter is null')
    }
    return this.getter()
  }
}

export function useRefAtom<T>(atom: Atom<T>): Getter<T> {
  const getAtomValue = useAtomCallback(
    React.useCallback(
      (get) => {
        const currCount = get(atom)
        return currCount
      },
      [atom],
    ),
  )

  const getterShell = React.useMemo(() => new Getter<T>(), [])
  getterShell.getter = () => getAtomValue()

  return getterShell
}
