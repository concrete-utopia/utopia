import * as React from 'react'
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
}

export const ExpandableIndicator: React.StatelessComponent<ExpandableIndicatorProps> = (props) => (
  <div data-testid={props.testId} style={{ width: 16, height: 16 }}>
    {props.visible ? (
      <Icn
        category='semantic'
        type={`expansionarrow-${props.collapsed ? 'right' : 'down'}`}
        color={props.selected ? 'white' : 'black'}
        onMouseDown={props.onMouseDown}
        onClick={props.onClick}
      />
    ) : null}
  </div>
)
