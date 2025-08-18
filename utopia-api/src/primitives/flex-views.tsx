import React from 'react'
import type { UtopiaComponentProps } from './common'

export interface FlexRowProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
}

export const FlexRow: React.FunctionComponent<FlexRowProps> = (props: FlexRowProps) => {
  return (
    <div
      {...props}
      style={{display: 'flex', flexDirection: 'row', ...props.style}}
    >
      {props.children}
    </div>
  )
}
FlexRow.displayName = 'FlexRow'

type FlexColProps = FlexRowProps

export const FlexCol: React.FunctionComponent<FlexColProps> = (props: FlexColProps) => {
  return (
    <div
      {...props}
      style={{display: 'flex', flexDirection: 'column', ...props.style}}
    >
      {props.children}
    </div>
  )
}
FlexCol.displayName = 'FlexCol'
