/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import { DEPRECATEDControlProps } from './control'
import { CheckboxInput } from '../../../uuiui'

export interface BooleanControlProps extends DEPRECATEDControlProps<boolean> {
  onMouseDown?: (e: React.MouseEvent<HTMLInputElement>) => void
}

export const BooleanControl: React.FunctionComponent<BooleanControlProps> = ({
  onSubmitValue: propsOnSubmitValue,
  value,
  ...props
}) => {
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
    />
  )
}
