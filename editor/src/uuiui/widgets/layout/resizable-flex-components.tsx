/** @jsx jsx */
import { jsx } from '@emotion/core'

import * as React from 'react'

import { Resizable, ResizableProps } from 'uuiui-deps'

import { UtopiaStyles } from '../../styles/theme'

export const ResizableFlexColumn: React.FunctionComponent<ResizableProps> = (props) => (
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
    }}
    {...props}
  />
)
ResizableFlexColumn.displayName = 'Resizable Flex Column'
