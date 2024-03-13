/** @jsx jsx */
/** @jsxRuntime classic */
import type { Interpolation, Theme } from '@emotion/react'
import { jsx } from '@emotion/react'
import React from 'react'
import type { UtopiaComponentProps } from './common'
import { addEventHandlersToDivProps } from './common'

export interface FlexRowProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  css: Interpolation<Theme>
}

export const FlexRow: React.FunctionComponent<FlexRowProps> = (props: FlexRowProps) => {
  return (
    <div
      {...props}
      // we use Emotion css prop here so that it is overwritable with both classNames and style props
      css={[{ display: 'flex', flexDirection: 'row' }, props.css]}
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
      // we use Emotion css prop here so that it is overwritable with both classNames and style props
      css={[{ display: 'flex', flexDirection: 'column' }, props.css]}
    >
      {props.children}
    </div>
  )
}
FlexCol.displayName = 'FlexCol'
