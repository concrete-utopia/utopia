/** @jsx jsx */
import { jsx } from '@emotion/core'
import { ControlProps } from './control'
import { CheckboxInput } from 'uuiui'

export interface BooleanControlProps extends ControlProps<boolean> {
  onMouseDown?: (e: React.MouseEvent<HTMLInputElement>) => void
}

export const BooleanControl: React.FunctionComponent<BooleanControlProps> = (props) => {
  const onSubmitValue = () => {
    const checked = !props.value
    props.onSubmitValue(checked)
  }

  return (
    <CheckboxInput
      key='boolean-control'
      className={`input checkbox`}
      checked={props.value}
      onContextMenu={props.onContextMenu}
      id={props.id}
      onChange={onSubmitValue}
      onMouseDown={props.onMouseDown}
    />
  )
}
