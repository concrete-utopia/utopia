import React from 'react'
import { Icn, Icons } from '../../../uuiui'

export const ExpansionArrowWidth = 8
export const ExpansionArrowHeight = 8

interface ExpandableIndicatorProps {
  visible: boolean
  collapsed: boolean
  selected: boolean
  onMouseDown?: (e: any) => void
  onClick?: (e: any) => void
  testId?: string
  style?: React.CSSProperties
}

export const ExpandableIndicator: React.FunctionComponent<
  React.PropsWithChildren<ExpandableIndicatorProps>
> = React.memo((props) => {
  return (
    <div data-testid={props.testId} style={{ width: 16, height: 16, ...props.style }}>
      <Icn
        category='semantic'
        type={`expansionarrow-${props.collapsed ? 'right' : 'down'}`}
        color={props.selected ? 'on-highlight-main' : 'main'}
        style={{
          pointerEvents: props.visible ? 'all' : 'none',
          visibility: props.visible ? 'visible' : 'hidden',
          cursor: 'pointer',
        }}
        onMouseDown={props.onMouseDown}
        onClick={props.onClick}
        testId={`${props.testId}-button`}
      />
    </div>
  )
})
