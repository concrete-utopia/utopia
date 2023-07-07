import React from 'react'
import type { NumberInputProps } from '../../../uuiui'
import { NumberInput } from '../../../uuiui'
import type { CSSNumber } from '../common/css-utils'
import { UIGridRow } from '../widgets/ui-grid-row'
import type { OnSubmitValue } from './control'
import type { SliderControlProps } from './slider-control'
import { SliderControl } from './slider-control'
interface SliderNumberControlProps {
  value: CSSNumber
  onSliderSubmitValue: OnSubmitValue<number>
  onSliderTransientSubmitValue: OnSubmitValue<number>
  transformSliderValueToCSSNumber: (value: number) => CSSNumber
}

type FilteredSliderProps = Omit<
  SliderControlProps,
  'onSubmitValue' | 'onTransientSubmitValue' | 'onForcedSubmitValue' | 'value'
>

// Slider + Number combo control
export const SliderNumberControl: React.FunctionComponent<
  React.PropsWithChildren<NumberInputProps & FilteredSliderProps & SliderNumberControlProps>
> = React.memo((props) => {
  const { value, transformSliderValueToCSSNumber, numberType } = props
  const [isDragging, setIsDragging] = React.useState(false)
  const [dragValue, setDragValue] = React.useState<CSSNumber>(value)

  const onDragStart = React.useCallback(() => {
    setIsDragging(true)
  }, [setIsDragging])
  const onDragEnd = React.useCallback(() => {
    setIsDragging(false)
  }, [setIsDragging])

  const onSliderDrag = React.useCallback(
    (newValue: number) => {
      setDragValue(transformSliderValueToCSSNumber(newValue))
    },
    [transformSliderValueToCSSNumber, setDragValue],
  )

  const transformCSSNumberToSliderValue = React.useCallback(() => {
    const valueToUse = isDragging ? dragValue : value
    if (numberType === 'UnitlessPercent' || numberType === 'Percent') {
      const scale = valueToUse.unit === '%' ? 100 : 1
      return valueToUse.value / scale
    } else {
      return valueToUse.value
    }
  }, [numberType, isDragging, dragValue, value])

  return (
    <UIGridRow padded={false} variant='<--------auto-------->|--45px--|'>
      <SliderControl
        id={`${props.id}-slider`}
        key={`${props.id}-slider`}
        testId={`${props.id}-slider`}
        value={transformCSSNumberToSliderValue()}
        DEPRECATED_controlOptions={props.DEPRECATED_controlOptions}
        onSubmitValue={props.onSliderSubmitValue}
        onTransientSubmitValue={props.onSliderTransientSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        onDragStart={onDragStart}
        onDragEnd={onDragEnd}
        onDrag={onSliderDrag}
      />
      <NumberInput
        value={isDragging ? dragValue : value}
        id={`${props.id}-number-input`}
        testId={`${props.id}-number-input`}
        onSubmitValue={props.onSubmitValue}
        onTransientSubmitValue={props.onTransientSubmitValue}
        controlStatus={props.controlStatus}
        minimum={props.minimum}
        maximum={props.maximum}
        stepSize={props.stepSize}
        numberType={props.numberType}
        defaultUnitToHide={props.defaultUnitToHide}
      />
    </UIGridRow>
  )
})
SliderNumberControl.displayName = 'SliderNumberControl'
