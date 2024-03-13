import { Interpolation, Theme } from '@emotion/react'
import React from 'react'
import type { UtopiaComponentProps } from './common'
import { addEventHandlersToDivProps } from './common'

export interface GroupProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {}

// This is a placeholder Group component for development
export const Group: React.FunctionComponent<GroupProps> = (props: GroupProps) => {
  return <div {...props}>{props.children}</div>
}
Group.displayName = 'Group'
