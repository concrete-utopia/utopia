import React from 'react'
import { InspectorSubsectionHeader } from '../../../uuiui'
import type { DEPRECATEDControlProps } from '../controls/control'

export interface SubsectionHeaderControlProps extends DEPRECATEDControlProps<string> {}

export const SubsectionHeaderControl: React.FunctionComponent<
  React.PropsWithChildren<SubsectionHeaderControlProps>
> = (props) => {
  return (
    <InspectorSubsectionHeader
      className={props.controlClassName}
      onDoubleClick={props.highlightNode}
      onContextMenu={props.onContextMenu}
    >
      {props.children}
    </InspectorSubsectionHeader>
  )
}
