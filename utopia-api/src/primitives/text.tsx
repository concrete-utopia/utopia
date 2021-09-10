import React from 'react'
import { addEventHandlersToDivProps, ViewProps } from '..'

// eslint-disable-next-line @typescript-eslint/no-empty-function
const NoOp = () => {}

export const Text: React.FunctionComponent<ViewProps> = (props: ViewProps) => {
  let { 'data-uid': dataUid, 'data-label': dataLabel, ...divProps } = props
  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  return (
    <span {...propsWithEventHandlers} data-uid={dataUid} data-label={dataLabel}>
      {props.children}
    </span>
  )
}
Text.displayName = 'Text'
