import * as React from 'react'

const forceUpdateFunction = (value: number) => value + 1

export function usePropControlledState<T>(
  propValue: T,
): [T, React.Dispatch<T>, React.DispatchWithoutAction] {
  const [localState, setLocalState] = React.useState<T>(propValue)
  const [forceUpdateValue, forceUpdate] = React.useReducer(forceUpdateFunction, 0)
  React.useEffect(() => {
    setLocalState(propValue)
  }, [propValue, forceUpdateValue])
  return [localState, setLocalState, forceUpdate]
}

export const stopPropagation = (e: React.MouseEvent) => {
  e.stopPropagation()
}

export const useHandleCloseOnESCOrEnter = (closePopup: () => void) => {
  const handleCloseOnESCOrEnter = React.useCallback(
    (e: KeyboardEvent) => {
      if (e.key === 'Escape' || e.key === 'Enter') {
        e.stopPropagation()
        if (closePopup != null) {
          closePopup()
        }
      }
    },
    [closePopup],
  )

  React.useEffect(() => {
    document.addEventListener('keydown', handleCloseOnESCOrEnter)
    return () => {
      document.removeEventListener('keydown', handleCloseOnESCOrEnter)
    }
  }, [handleCloseOnESCOrEnter])
}

export const checkerboardBackground: Pick<
  React.CSSProperties,
  'backgroundImage' | 'backgroundSize' | 'backgroundPosition'
> = {
  backgroundImage: `
    linear-gradient(to bottom left,   #e7e7e7 25%,  transparent 25%),
    linear-gradient(to bottom left,   transparent 75%,  #e7e7e7 75%),
    linear-gradient(to bottom right,  #e7e7e7 25%,  transparent 25%),
    linear-gradient(to bottom right,  transparent 75%,  #e7e7e7 75%)`,
  backgroundSize: '12px 12px, 12px 12px, 12px 12px, 12px 12px',
  backgroundPosition: '-9px 0px, -3px -6px, 3px 6px, -3px 0',
}

export function clampString(value: string, maxLength: number) {
  return value.length > maxLength ? `${value.substring(0, maxLength)}…` : value
}
