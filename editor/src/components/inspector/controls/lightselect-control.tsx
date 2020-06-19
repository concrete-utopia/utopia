/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { PopupList } from 'uuiui'
import { DEPRECATEDControlProps } from './control'
import { SelectOption } from './select-control'
import { FlexRow } from 'uuiui'

// TODO stronger type for this control
export const LightSelectControl: React.StatelessComponent<DEPRECATEDControlProps<any>> = (
  props,
) => {
  const options = props.options as Array<SelectOption>
  const selectedOption = options.find((option) => option.value === props.value)
  const label = selectedOption != null ? selectedOption.label : props.value + ''

  const mixed = props.controlStyles.mixed
  const onSubmitValue = (newValue: SelectOption) => {
    if (newValue != null) {
      props.onSubmitValue(newValue.value)
    }
  }

  return (
    <FlexRow className={props.controlClassName} style={props.style}>
      <PopupList
        disabled={!props.controlStyles.interactive}
        value={mixed ? { value: props.value, label: 'mixed' } : { value: props.value, label }}
        onSubmitValue={onSubmitValue}
        options={options}
        containerMode='noBorder'
      />
    </FlexRow>
  )
}
