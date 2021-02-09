import * as React from 'react'

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
  const previousRef = React.useRef<T>()

  React.useEffect(() => {
    previousRef.current = currentValue
  }, [currentValue])

  return previousRef.current
}

export function useForceUpdate() {
  const [, forceUpdate] = React.useReducer((c) => c + 1, 0)
  return forceUpdate
}
