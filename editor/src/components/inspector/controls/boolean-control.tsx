/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import type { DEPRECATEDControlProps } from './control'
import { CheckboxInput } from '../../../uuiui'

export interface BooleanControlProps extends DEPRECATEDControlProps<boolean> {
  onMouseDown?: (e: React.MouseEvent<HTMLInputElement>) => void
  focusOnMount?: boolean
}

export const BooleanControl: React.FunctionComponent<
  React.PropsWithChildren<BooleanControlProps>
> = ({ onSubmitValue: propsOnSubmitValue, value, ...props }) => {
  const onSubmitValue = React.useCallback(() => {
    const checked = !value
    propsOnSubmitValue(checked)
  }, [value, propsOnSubmitValue])

  return (
    <CheckboxInput
      key='boolean-control'
      className={`input checkbox`}
      checked={value}
      onContextMenu={props.onContextMenu}
      id={props.id}
      onChange={onSubmitValue}
      onMouseDown={props.onMouseDown}
      focusOnMount={props.focusOnMount}
    />
  )
}
