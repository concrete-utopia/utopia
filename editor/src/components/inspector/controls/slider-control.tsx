import type { Marks } from 'rc-slider'
import Slider from 'rc-slider'
import React from 'react'
import { FlexRow, UtopiaTheme } from '../../../uuiui'
import type { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'

export interface DEPRECATEDSliderControlOptions extends DEPRECATEDGenericControlOptions {
  minimum: number
  maximum: number
  filled?: boolean
  stepSize?: number
  origin?: number
}

export const defaultSliderControlOptions = {
  filled: true,
  stepSize: 1,
}

export type SliderControlProps = DEPRECATEDControlProps<number> & {
  DEPRECATED_controlOptions: DEPRECATEDSliderControlOptions
}

export const SliderControl: React.FunctionComponent<React.PropsWithChildren<SliderControlProps>> = (
  props,
) => {
  const { onTransientSubmitValue, value, onDragStart, onDragEnd, onDrag } = props
  const [isSliding, setIsSliding] = React.useState(false)
  const [slidingValue, setSlidingValue] = React.useState(0)

  const handleBeforeChange = React.useCallback(() => {
    setIsSliding(true)
    if (onDragStart != null) {
      onDragStart()
    }
  }, [onDragStart])

  const handleDragging = React.useCallback(
    (newValue: number) => {
      onTransientSubmitValue!(newValue, true)
      setSlidingValue(newValue)
      if (onDrag != null) {
        onDrag(newValue)
      }
    },
    [onTransientSubmitValue, onDrag],
  )
  const onChangeFn = props.onForcedSubmitValue ?? props.onSubmitValue
  const handleOnAfterChange = React.useCallback(
    (n: number) => {
      onChangeFn(n)
      setIsSliding(false)
      if (onDragEnd != null) {
        onDragEnd()
      }
    },
    [onChangeFn, onDragEnd],
  )

  const controlOptions = React.useMemo(() => {
    return {
      ...defaultSliderControlOptions,
      ...props.DEPRECATED_controlOptions,
    }
  }, [props.DEPRECATED_controlOptions])

  const handleAndTrackStyle = React.useMemo(() => {
    return {
      backgroundColor: controlOptions.filled ? props.controlStyles.trackColor : undefined,
    }
  }, [controlOptions.filled, props.controlStyles])

  const activeDotStyle = React.useMemo(() => {
    return {
      backgroundColor: controlOptions.filled
        ? props.controlStyles.trackColor
        : props.controlStyles.railColor,
    }
  }, [controlOptions.filled, props.controlStyles])

  const railAndDotStyle = React.useMemo(() => {
    return {
      backgroundColor: props.controlStyles.railColor,
    }
  }, [props.controlStyles])

  let marks: Marks = {}
  if (typeof controlOptions.origin === 'number') {
    marks[controlOptions.origin] = {
      style: {
        display: 'none',
      },
      label: String(controlOptions.origin),
    }
  }

  return (
    <FlexRow
      style={{
        width: '100%',
        height: UtopiaTheme.layout.inputHeight.default,
        paddingLeft: 4,
        paddingRight: 4,
        alignItems: 'center',
        ...props.style,
      }}
      className={props.controlClassName}
      onContextMenu={props.onContextMenu}
    >
      <Slider
        disabled={!props.controlStyles.interactive}
        value={isSliding ? slidingValue : value}
        onBeforeChange={handleBeforeChange}
        onChange={handleDragging}
        onAfterChange={handleOnAfterChange}
        min={controlOptions.minimum}
        max={controlOptions.maximum}
        step={controlOptions.stepSize}
        marks={marks}
        handleStyle={handleAndTrackStyle}
        trackStyle={handleAndTrackStyle}
        activeDotStyle={activeDotStyle}
        railStyle={railAndDotStyle}
        dotStyle={railAndDotStyle}
      />
      {/* This div blocks other controls from receiving hover events 
          while you're sliding the slider. NB this needs to come here, 
          i.e. *after* the Slider component - so it doesn't obscure it. 
      */}
      {isSliding ? (
        <div
          style={{
            position: 'fixed',
            background: 'transparent',
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
            zIndex: 1,
          }}
        />
      ) : null}
    </FlexRow>
  )
}
