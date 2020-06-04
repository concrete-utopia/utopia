import * as React from 'react'
import { useSpring, animated } from 'react-spring'
import { SegmentOption } from './segment-control'
import { OnSubmitValue } from '../../../uuiui-deps'

interface SegmentOptionTextControlProps<T extends string | number> {
  option: SegmentOption<T>
  selected: boolean
  onSubmitValue: OnSubmitValue<T>
}

export const SegmentOptionTextControl = <T extends string | number>({
  option,
  selected,
  onSubmitValue,
}: SegmentOptionTextControlProps<T>) => {
  const style = useSpring({
    fontWeight: selected ? 600 : 400,
    fontFamily: 'Inter, sans-serif',
    fontSize: 11,
    textAlign: 'center',
    paddingTop: 3,
  } as React.CSSProperties)
  const value = option.value
  const indexedOnSubmitValue = React.useCallback(() => {
    onSubmitValue(value)
  }, [onSubmitValue, value])
  return (
    <animated.div style={style} onClick={indexedOnSubmitValue}>
      {option.label}
    </animated.div>
  )
}
