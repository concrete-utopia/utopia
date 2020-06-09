import * as React from 'react'
import { animated, useSpring } from 'react-spring'
import { ControlStyles, OnSubmitValue } from '../../../uuiui-deps'
import { SegmentOption } from './segment-control'

interface SegmentOptionTextControlProps<T extends string | number> {
  option: SegmentOption<T>
  selected: boolean
  onSubmitValue: OnSubmitValue<T>
  controlStyles: ControlStyles
}

export const SegmentOptionTextControl = <T extends string | number>({
  option,
  selected,
  onSubmitValue,
  controlStyles,
}: SegmentOptionTextControlProps<T>) => {
  const springStyle = useSpring({
    color: selected ? 'white' : controlStyles.mainColor,
  } as React.CSSProperties)
  const value = option.value
  const indexedOnSubmitValue = React.useCallback(() => {
    onSubmitValue(value)
  }, [onSubmitValue, value])
  return (
    <animated.div
      style={{
        fontWeight: selected ? 600 : 400,
        fontFamily: 'Inter, sans-serif',
        fontSize: 11,
        textAlign: 'center',
        paddingTop: 3,
        ...springStyle,
      }}
      onClick={indexedOnSubmitValue}
    >
      {option.label}
    </animated.div>
  )
}
