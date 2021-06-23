import Slider, { Marks } from 'rc-slider'
import * as React from 'react'
import { FlexRow, UtopiaTheme } from '../../../uuiui'
import { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'

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

export const SliderControl: React.FunctionComponent<SliderControlProps> = (props) => {
  const [isSliding, setIsSliding] = React.useState(false)

  const handleBeforeChange = React.useCallback(() => {
    setIsSliding(true)
  }, [])

  const onChangeFn = props.onForcedSubmitValue ?? props.onSubmitValue

  const handleOnAfterChange = React.useCallback(
    (n: number) => {
      onChangeFn(n)
      setIsSliding(false)
    },
    [onChangeFn],
  )

  const controlOptions = {
    ...defaultSliderControlOptions,
    ...props.DEPRECATED_controlOptions,
  }

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
        value={props.value}
        onBeforeChange={handleBeforeChange}
        onChange={props.onTransientSubmitValue}
        // onAfterChange={props.onForcedSubmitValue ?? props.onSubmitValue}
        onAfterChange={handleOnAfterChange}
        min={controlOptions.minimum}
        max={controlOptions.maximum}
        step={controlOptions.stepSize}
        marks={marks}
        handleStyle={{
          backgroundColor: controlOptions.filled ? props.controlStyles.trackColor : undefined,
        }}
        trackStyle={{
          backgroundColor: controlOptions.filled ? props.controlStyles.trackColor : undefined,
        }}
        activeDotStyle={{
          backgroundColor: controlOptions.filled
            ? props.controlStyles.trackColor
            : props.controlStyles.railColor,
        }}
        railStyle={{
          backgroundColor: props.controlStyles.railColor,
        }}
        dotStyle={{
          backgroundColor: props.controlStyles.railColor,
        }}
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

export class SliderControl2 extends React.Component<SliderControlProps> {
  render() {
    const controlOptions = {
      ...defaultSliderControlOptions,
      ...this.props.DEPRECATED_controlOptions,
    }

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
          ...this.props.style,
        }}
        className={this.props.controlClassName}
        onContextMenu={this.props.onContextMenu}
      >
        <Slider
          disabled={!this.props.controlStyles.interactive}
          value={this.props.value}
          onChange={this.props.onTransientSubmitValue}
          onAfterChange={this.props.onForcedSubmitValue ?? this.props.onSubmitValue}
          min={controlOptions.minimum}
          max={controlOptions.maximum}
          step={controlOptions.stepSize}
          marks={marks}
          handleStyle={{
            backgroundColor: controlOptions.filled
              ? this.props.controlStyles.trackColor
              : undefined,
          }}
          trackStyle={{
            backgroundColor: controlOptions.filled
              ? this.props.controlStyles.trackColor
              : undefined,
          }}
          activeDotStyle={{
            backgroundColor: controlOptions.filled
              ? this.props.controlStyles.trackColor
              : this.props.controlStyles.railColor,
          }}
          railStyle={{
            backgroundColor: this.props.controlStyles.railColor,
          }}
          dotStyle={{
            backgroundColor: this.props.controlStyles.railColor,
          }}
        />
      </FlexRow>
    )
  }
}
