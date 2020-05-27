import * as React from 'react'

export const ExpansionArrowWidth = 8
export const ExpansionArrowHeight = 8

interface ExpandableIndicatorProps {
  visible: boolean
  collapsed: boolean
  selected: boolean
  onMouseDown?: (e: any) => void
  onClick?: (e: any) => void
}

export const ExpandableIndicator: React.StatelessComponent<ExpandableIndicatorProps> = (props) => (
  <svg
    className={'row-expansion ' + (props.visible ? 'expansive' : 'not-expansive')}
    style={{
      transform: props.collapsed ? 'rotate(-90deg)' : '',
      transformOrigin: 'center center',
      transition: 'transform .05s',
      padding: '7px 4px',
    }}
    width={ExpansionArrowWidth}
    height={ExpansionArrowHeight}
    strokeLinecap='round'
    shapeRendering='geometricPrecision'
    onMouseDown={props.onMouseDown}
    onClick={props.onClick}
  >
    <polyline
      className='expandable-indicator'
      strokeWidth={props.visible ? 1 : 0}
      stroke={props.selected ? 'white' : '#666'}
      fill='transparent'
      points='2,3 5,6 8,3'
    />
  </svg>
)
