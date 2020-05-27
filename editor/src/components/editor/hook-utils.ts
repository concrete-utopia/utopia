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
