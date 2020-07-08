import Slider, { Marks } from 'rc-slider'
import * as React from 'react'
import { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'
import { FlexRow, UtopiaTheme } from 'uuiui'

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

export class SliderControl extends React.Component<SliderControlProps, {}> {
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
