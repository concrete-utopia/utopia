import React from 'react'

const ControlsDisabledContext = React.createContext(false)

export function useControlsDisabledInSubtree() {
  return React.useContext(ControlsDisabledContext)
}

export const DisableControlsInSubtree = (props: React.PropsWithChildren<unknown>) => {
  return (
    <ControlsDisabledContext.Provider value={true}>
      {props.children}
    </ControlsDisabledContext.Provider>
  )
}

export const EnableControlsInSubtree = (props: React.PropsWithChildren<unknown>) => {
  return (
    <ControlsDisabledContext.Provider value={false}>
      {props.children}
    </ControlsDisabledContext.Provider>
  )
}
