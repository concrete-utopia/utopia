import Tippy from '@tippyjs/react'
import { Placement } from 'tippy.js'
import 'tippy.js/dist/tippy.css'
import React from 'react'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme } from './styles/theme'

export interface TooltipProps {
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
