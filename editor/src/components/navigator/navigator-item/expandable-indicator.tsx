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

export const ExpandableIndicator: React.FunctionComponent<ExpandableIndicatorProps> = React.memo(
  (props) => {
    return (
      <div data-testid={props.testId} style={{ width: 16, height: 16, ...props.style }}>
        {props.visible ? (
          <Icn
            category='semantic'
            type={`expansionarrow-${props.collapsed ? 'right' : 'down'}`}
            color={props.selected ? 'on-highlight-main' : 'main'}
            onMouseDown={props.onMouseDown}
            onClick={props.onClick}
          />
        ) : null}
      </div>
    )
  },
)
