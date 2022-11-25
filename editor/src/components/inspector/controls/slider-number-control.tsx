import React from 'react'
import { NumberInput, NumberInputProps } from '../../../uuiui'
import { CSSNumber } from '../common/css-utils'
import { UIGridRow } from '../widgets/ui-grid-row'
import { OnSubmitValue } from './control'
import { SliderControl, SliderControlProps } from './slider-control'

interface SliderNumberControlProps {
  value: CSSNumber
  onSliderSubmitValue: OnSubmitValue<number>
  onSliderTransientSubmitValue: OnSubmitValue<number>
  transformSliderValueToCSSNumber: (value: number) => CSSNumber
  transformCSSNumberToSliderValue: (value: CSSNumber) => number
}

type FilteredSliderProps = Omit<
  SliderControlProps,
  'onSubmitValue' | 'onTransientSubmitValue' | 'value'
>

// Slider + Number combo control
export const SliderNumberControl: React.FunctionComponent<
  React.PropsWithChildren<NumberInputProps & FilteredSliderProps & SliderNumberControlProps>
> = React.memo((props) => {
  const { value, transformSliderValueToCSSNumber, transformCSSNumberToSliderValue } = props
  const [isDragging, setIsDragging] = React.useState(false)
  const [displayValue, setDisplayValue] = React.useState<CSSNumber>(value)

  React.useEffect(() => {
    if (!isDragging) {
      setDisplayValue(value)
    }
  }, [isDragging, value])

  const onDragStart = React.useCallback(() => {
    setIsDragging(true)
  }, [setIsDragging])
  const onDragEnd = React.useCallback(() => {
    setIsDragging(false)
  }, [setIsDragging])

  const onSliderDrag = React.useCallback(
    (newValue: number) => {
      setDisplayValue(transformSliderValueToCSSNumber(newValue))
    },
    [transformSliderValueToCSSNumber, setDisplayValue],
  )

  return (
    <UIGridRow padded={false} variant='<--------auto-------->|--45px--|'>
      <SliderControl
        id={`${props.id}-slider`}
        key={`${props.id}-slider`}
        testId={`${props.id}-slider`}
        value={transformCSSNumberToSliderValue(displayValue)}
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
        value={displayValue}
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
