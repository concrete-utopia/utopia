import * as React from 'react'
import { InspectorSubsectionHeader } from '../../../uuiui'
import { DEPRECATEDControlProps } from '../controls/control'

export interface SubsectionHeaderControlProps extends DEPRECATEDControlProps<string> {}

export const SubsectionHeaderControl: React.FunctionComponent<SubsectionHeaderControlProps> = (
  props,
) => {
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
