import React from 'react'

const ControlsDisabledContext = React.createContext(false)

export function useControlsDisabledInSubtree() {
  return React.useContext(ControlsDisabledContext)
}

export const DisableControlsInSubtree = (props: React.PropsWithChildren<{ disable: boolean }>) => {
  const { disable } = props
  return (
    <ControlsDisabledContext.Provider value={disable}>
      {props.children}
    </ControlsDisabledContext.Provider>
  )
}
