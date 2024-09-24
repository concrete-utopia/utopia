/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import Tippy from '@tippyjs/react'
import type { Placement } from 'tippy.js'
import 'tippy.js/dist/tippy.css'
import React from 'react'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme } from './styles/theme'
import { isFeatureEnabled } from '../utils/feature-switches'

export interface TooltipProps {
  children?: React.ReactElement<any>
  title: React.ReactElement<any> | string
  placement?: Placement
  disabled?: boolean
  backgroundColor?: string
  textColor?: string
}

export const Tooltip: React.FunctionComponent<React.PropsWithChildren<TooltipProps>> = (props) => {
  const backgroundColor = props.backgroundColor ?? colorTheme.neutralInvertedBackground.value
  const textColor = props.textColor ?? colorTheme.neutralInvertedForeground.value

  const css = React.useMemo(
    () =>
      ({
        fontWeight: 400,
        fontSize: 11,
        textAlign: 'center',
        fontFamily:
          "utopian-inter, -apple-system, BlinkMacSystemFont, Helvetica, 'Segoe UI', Roboto, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'",
        backgroundColor: `${backgroundColor} !important`,
        color: `${textColor} !important`,
        '& .tippy-content': {
          padding: '4px 8px !important',
        },
        '&[data-placement^=top] .tippy-arrow::before': {
          borderTopColor: `${backgroundColor} !important`,
        },
        '&[data-placement^=right] .tippy-arrow::before': {
          borderRightColor: `${backgroundColor} !important`,
        },
        '&[data-placement^=bottom] .tippy-arrow::before': {
          borderBottomColor: `${backgroundColor} !important`,
        },
        '&[data-placement^=left] .tippy-arrow::before': {
          borderLeftColor: `${backgroundColor} !important`,
        },
      } as const),
    [backgroundColor, textColor],
  )

  return isFeatureEnabled('Tooltip Debug') ? (
    <React.Fragment>{props.children}</React.Fragment>
  ) : (
    <Tippy
      css={css} // TODO we should not use Emotion for this component
      arrow
      disabled={props.disabled}
      content={props.title}
      placement={props.placement ?? 'top'}
      delay={[500, 0]} // [show, hide] milliseconds
      animation='fade'
      theme='material'
    >
      {props.children}
    </Tippy>
  )
}
