/** @jsx jsx */
import { jsx, css } from '@emotion/react'
import * as React from 'react'
//TODO: switch to functional component and make use of 'useColorTheme':

interface TooltipProps {
  children?: React.ReactElement<any>
  title: React.ReactElement<any> | string
  disabled?: boolean
}

function tooltipPropsEqual(
  a: React.PropsWithChildren<TooltipProps>,
  b: React.PropsWithChildren<TooltipProps>,
): boolean {
  return a == b || (a.title === b.title && a.disabled === b.disabled && a.children === b.children)
}

export class Tooltip extends React.Component<React.PropsWithChildren<TooltipProps>> {
  shouldComponentUpdate(nextProps: React.PropsWithChildren<TooltipProps>): boolean {
    return !tooltipPropsEqual(this.props, nextProps)
  }

  render() {
    if (this.props.disabled) {
      return this.props.children
    } else {
      return (
        <div
          css={{
            position: 'relative',
            '&:hover:after': {
              background: 'orange',
              color: 'white',
              content: `"${this.props.title}"`,
              position: 'absolute',
            },
          }}
        >
          {this.props.children}
        </div>
      )
    }
  }
}
