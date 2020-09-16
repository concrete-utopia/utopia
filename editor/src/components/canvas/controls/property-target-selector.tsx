import * as React from 'react'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { colorTheme } from '../../../uuiui'

interface PropertyTargetSelector {
  top: number
  left: number
  options: LayoutTargetableProp[]
  selected: number
  setOptionsCallback: (options: Array<LayoutTargetableProp>) => void
}

export const PropertyTargetSelector = (props: PropertyTargetSelector): JSX.Element => {
  props.setOptionsCallback(props.options)

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
      {props.options.map((option, index) => {
        return (
          <div
            key={option}
            style={{
              padding: '0 3px',
              color: props.selected === index ? 'white' : colorTheme.controlledBlue.value,
              backgroundColor:
                props.selected === index ? colorTheme.controlledBlue.value : 'inherit',
              borderRadius: 5,
            }}
          >
            {option}
          </div>
        )
      })}
    </div>
  )
}
