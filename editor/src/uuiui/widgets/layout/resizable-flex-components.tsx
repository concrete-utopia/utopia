/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'

import React from 'react'
import type { ResizableProps } from '../../../uuiui-deps'
import { Resizable } from '../../../uuiui-deps'

import { UtopiaStyles } from '../../styles/theme'

export const ResizableFlexColumn: React.FunctionComponent<
  React.PropsWithChildren<ResizableProps>
> = (props) => (
  <Resizable
    style={UtopiaStyles.flexColumn}
    enable={{
      top: false,
      right: true,
      bottom: false,
      left: false,
      topRight: false,
      bottomRight: false,
      bottomLeft: false,
      topLeft: false,
      ...props.enable,
    }}
    {...props}
  />
)
ResizableFlexColumn.displayName = 'Resizable Flex Column'
