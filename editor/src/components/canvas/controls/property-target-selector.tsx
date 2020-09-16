import * as React from 'react'
import { colorTheme } from '../../../uuiui'

interface PropertyTargetSelector {
  top: number
  left: number
  options: string[]
  selected: number
}

export const PropertyTargetSelector = (props: PropertyTargetSelector): JSX.Element => {
  return (
    <div
      style={{
        position: 'absolute',
        backgroundColor: '#d4f3ff',
        border: `1px solid ${colorTheme.controlledBlue.value}`,
        borderRadius: 5,
        top: props.top,
        left: props.left,
      }}
    >
      {props.options.map((option) => {
        return (
          <div key={option} style={{ padding: '0 3px', color: colorTheme.controlledBlue.value }}>
            {option}
          </div>
        )
      })}
    </div>
  )
}
