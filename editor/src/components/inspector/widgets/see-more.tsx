import * as React from 'react'
import { ToggleButton } from 'uuiui'

export const SeeMoreButton: React.FunctionComponent<{
  seeMoreVisible: boolean
  toggleSeeMoreVisible: () => void
}> = (props) => (
  <ToggleButton onClick={props.toggleSeeMoreVisible} value={props.seeMoreVisible}>
    …
  </ToggleButton>
)

export const SeeMoreContainer: React.FunctionComponent<{
  visible: boolean
}> = (props) => {
  return <div style={{ display: props.visible ? 'block' : 'none' }}>{props.children}</div>
}

export const SeeMoreHOC: React.FunctionComponent<{
  visible: boolean
}> = (props) => {
  return <>{props.visible ? props.children : null}</>
}

export function useToggle(initialValue: boolean): [boolean, () => void] {
  const [value, setValue] = React.useState(initialValue)
  const toggleSeeMoreVisible = React.useCallback(() => {
    setValue((v) => {
      return !v
    })
  }, [setValue])
  return [value, toggleSeeMoreVisible]
}
