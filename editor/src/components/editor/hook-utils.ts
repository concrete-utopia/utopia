import React from 'react'

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
