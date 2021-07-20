/** @jsx jsx */
import { jsx } from '@emotion/react'
import Tippy from '@tippyjs/react'
import { Placement } from 'tippy.js'
import 'tippy.js/dist/tippy.css'
import * as React from 'react'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme } from './styles/theme'

interface TooltipProps {
  children?: React.ReactElement<any>
  title: React.ReactElement<any> | string
  placement?: Placement
  disabled?: boolean
}

function tooltipPropsEqual(
  a: React.PropsWithChildren<TooltipProps>,
  b: React.PropsWithChildren<TooltipProps>,
): boolean {
  return (
    a == b ||
    (a.title === b.title &&
      a.placement === b.placement &&
      a.disabled === b.disabled &&
      a.children === b.children)
  )
}

export class Tooltip extends React.Component<React.PropsWithChildren<TooltipProps>> {
  shouldComponentUpdate(nextProps: React.PropsWithChildren<TooltipProps>): boolean {
    return !tooltipPropsEqual(this.props, nextProps)
  }

  render() {
    return (
      <Tippy
        css={{
          fontWeight: 400,
          fontSize: 11,
          fontFamily:
            "utopian-inter, -apple-system, BlinkMacSystemFont, Helvetica, 'Segoe UI', Roboto, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'",
          backgroundColor: `${colorTheme.neutralInvertedBackground.value} !important`,
          color: `${colorTheme.neutralInvertedForeground.value} !important`,
          '& .tippy-content': {
            padding: '4px 8px !important',
          },
          '&[data-placement^=top] .tippy-arrow::before': {
            borderTopColor: `${colorTheme.neutralInvertedBackground.value} !important`,
          },
          '&[data-placement^=right] .tippy-arrow::before': {
            borderRightColor: `${colorTheme.neutralInvertedBackground.value} !important`,
          },
          '&[data-placement^=bottom] .tippy-arrow::before': {
            borderBottomColor: `${colorTheme.neutralInvertedBackground.value} !important`,
          },
          '&[data-placement^=left] .tippy-arrow::before': {
            borderLeftColor: `${colorTheme.neutralInvertedBackground.value} !important`,
          },
        }}
        arrow
        disabled={this.props.disabled}
        content={this.props.title}
        placement={this.props.placement ?? 'top'}
        delay={[500, 0]} // [show, hide] milliseconds
        animation='fade'
        theme='material'
      >
        {this.props.children}
      </Tippy>
    )
  }
}
