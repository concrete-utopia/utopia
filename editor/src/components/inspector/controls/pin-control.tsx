import React from 'react'
import Utils from '../../../utils/utils'
import type { ControlStatus, ControlStyles } from '../common/control-status'
import { getControlStyles } from '../common/control-status'
import { FramePoint } from 'utopia-api/core'
import type { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import type { FramePinsInfo } from '../common/layout-property-path-hooks'
import { UtopiaTheme, SquareButton } from '../../../uuiui'
import { unless } from '../../../utils/react-conditionals'

interface PinControlProps {
  handlePinMouseDown: (frameProp: LayoutPinnedProp) => void
  name: string
  controlStatus: ControlStatus
  framePoints: FramePinsInfo
  mixed?: boolean
  className?: string
  style?: React.CSSProperties
  exclude?: ExcludePinControls
}

export type ExcludePinControls = {
  center?: boolean
  sides?: boolean
}

const Margin = 3
const Width = 66
const Height = 57

const HorizontalLength = (Width - Margin * 6) / 4
const HorizontalStart = Margin
const VerticalMid = Height / 2
const HorizontalEnd = Margin * 5 + HorizontalLength * 3
const HorizontalDividerStart = HorizontalLength + Margin * 2
const HorizontalDividerWidth = HorizontalLength * 2 + Margin * 2

const VerticalLength = (Height - Margin * 6) / 4
const VerticalStart = Margin
const HorizontalMid = Width / 2
const VerticalEnd = Margin * 5 + VerticalLength * 3
const VerticalDividerStart = VerticalLength + Margin * 2
const VerticalDividerHeight = VerticalLength * 2 + Margin * 2

const MouseCatchmentStrokeWidth =
  Math.min(Width, Height) - Math.min(HorizontalDividerStart, VerticalDividerStart) * 2

function getStrokeColor(
  controlStyles: ControlStyles,
  framePoints: FramePinsInfo,
  mixed: boolean | undefined,
  point: FramePoint,
) {
  const isPrimary = Utils.propOr(false, 'isPrimaryPosition', framePoints[point])

  if (isPrimary && !mixed) {
    return controlStyles.strokePrimaryColor
  } else {
    return controlStyles.strokeTertiaryColor
  }
}

function getStrokeDashArray(
  framePoints: FramePinsInfo,
  mixed: boolean | undefined,
  point: FramePoint,
) {
  const isRelative = Utils.propOr(false, 'isRelativePosition', framePoints[point])

  if (isRelative && !mixed) {
    return '1 2'
  } else {
    return '0'
  }
}

function getTestId(prefix: string, id: string): string {
  return `${prefix}-${id}`
}

export const PinControl = (props: PinControlProps) => {
  const controlStyles: ControlStyles = getControlStyles(props.controlStatus)

  const handlePinMouseDown = (frameProp: LayoutPinnedProp) => () => {
    props.handlePinMouseDown(frameProp)
  }

  const exclude: ExcludePinControls = React.useMemo(
    () =>
      props.exclude ?? {
        center: false,
        sides: false,
      },
    [props.exclude],
  )

  return (
    <div id='pin-control' className={props.className} style={props.style}>
      <svg
        width={Width}
        viewBox={`0 0 ${Width} ${Height}`}
        version='1.1'
        xmlns='http://www.w3.org/2000/svg'
        vectorEffect='non-scaling-stroke'
      >
        <rect
          id={getTestId(props.name, 'box')}
          data-testid={getTestId(props.name, 'box')}
          fill={controlStyles.backgroundColor}
          stroke={controlStyles.borderColor}
          strokeWidth='0'
          x='0.5'
          y='0.5'
          width={Width - 1}
          height={Height - 1}
          rx={UtopiaTheme.inputBorderRadius}
        />
        <rect
          id={getTestId(props.name, 'divider')}
          data-testid={getTestId(props.name, 'divider')}
          fill={controlStyles.backgroundColor}
          stroke={controlStyles.borderColor}
          strokeWidth='1'
          x={HorizontalDividerStart}
          y={VerticalDividerStart}
          width={HorizontalDividerWidth}
          height={VerticalDividerHeight}
          rx={UtopiaTheme.inputBorderRadius}
        />
        <g
          id={getTestId(props.name, 'pins')}
          data-testid={getTestId(props.name, 'pins')}
          strokeWidth='1'
        >
          {unless(
            exclude.sides === true,
            <>
              <path
                d={`M${HorizontalMid},${VerticalStart} l0,${VerticalLength}`}
                className='pin-indicator'
                id={getTestId(props.name, 'pin-top')}
                data-testid={getTestId(props.name, 'pin-top')}
                stroke={getStrokeColor(
                  controlStyles,
                  props.framePoints,
                  props.mixed,
                  FramePoint.Top,
                )}
                strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.Top)}
                strokeLinecap='round'
              />
              <path
                d={`M${HorizontalMid},${VerticalStart} l0,${VerticalLength}`}
                strokeWidth={MouseCatchmentStrokeWidth}
                stroke='transparent'
                strokeLinecap='butt'
                data-testid={getTestId(props.name, 'catcher-pin-top')}
                onMouseDown={handlePinMouseDown('top')}
              />
              <path
                d={`M${HorizontalMid},${VerticalEnd} l0,${VerticalLength}`}
                className='pin-indicator'
                id={getTestId(props.name, 'pin-bottom')}
                data-testid={getTestId(props.name, 'pin-bottom')}
                stroke={getStrokeColor(
                  controlStyles,
                  props.framePoints,
                  props.mixed,
                  FramePoint.Bottom,
                )}
                strokeDasharray={getStrokeDashArray(
                  props.framePoints,
                  props.mixed,
                  FramePoint.Bottom,
                )}
                strokeLinecap='round'
              />
              <path
                d={`M${HorizontalMid},${VerticalEnd} l0,${VerticalLength}`}
                strokeWidth={MouseCatchmentStrokeWidth}
                stroke='transparent'
                strokeLinecap='butt'
                data-testid={getTestId(props.name, 'catcher-pin-bottom')}
                onMouseDown={handlePinMouseDown('bottom')}
              />
              <path
                d={`M${HorizontalEnd},${VerticalMid} l${HorizontalLength},0`}
                className='pin-indicator'
                id={getTestId(props.name, 'pin-right')}
                data-testid={getTestId(props.name, 'pin-right')}
                stroke={getStrokeColor(
                  controlStyles,
                  props.framePoints,
                  props.mixed,
                  FramePoint.Right,
                )}
                strokeDasharray={getStrokeDashArray(
                  props.framePoints,
                  props.mixed,
                  FramePoint.Right,
                )}
                strokeLinecap='round'
              />
              <path
                d={`M${HorizontalEnd},${VerticalMid} l${HorizontalLength},0`}
                strokeWidth={MouseCatchmentStrokeWidth}
                stroke='transparent'
                strokeLinecap='butt'
                data-testid={getTestId(props.name, 'catcher-pin-right')}
                onMouseDown={handlePinMouseDown('right')}
              />
              <path
                d={`M${HorizontalStart},${VerticalMid} l${HorizontalLength},0`}
                className='pin-indicator'
                id={getTestId(props.name, 'pin-left')}
                data-testid={getTestId(props.name, 'pin-left')}
                stroke={getStrokeColor(
                  controlStyles,
                  props.framePoints,
                  props.mixed,
                  FramePoint.Left,
                )}
                strokeDasharray={getStrokeDashArray(
                  props.framePoints,
                  props.mixed,
                  FramePoint.Left,
                )}
                strokeLinecap='round'
              />
              <path
                d={`M${HorizontalStart},${VerticalMid} l${HorizontalLength},0`}
                strokeWidth={MouseCatchmentStrokeWidth}
                stroke='transparent'
                strokeLinecap='butt'
                data-testid={getTestId(props.name, 'catcher-pin-left')}
                onMouseDown={handlePinMouseDown('left')}
              />
            </>,
          )}
          {unless(
            exclude.center === true,
            <>
              <path
                d={`M${HorizontalMid - (HorizontalLength - 6)},${VerticalMid} l${
                  (HorizontalLength - 6) * 2
                },0`}
                className='pin-indicator'
                id={getTestId(props.name, 'pin-centerx')}
                data-testid={getTestId(props.name, 'pin-centerx')}
                stroke={getStrokeColor(
                  controlStyles,
                  props.framePoints,
                  props.mixed,
                  FramePoint.CenterX,
                )}
                strokeDasharray={getStrokeDashArray(
                  props.framePoints,
                  props.mixed,
                  FramePoint.CenterX,
                )}
                strokeLinecap='round'
              />
              <path
                d={`M${HorizontalMid},${VerticalMid - (VerticalLength - 4)} l0,${
                  (VerticalLength - 4) * 2
                }`}
                className='pin-indicator'
                id={getTestId(props.name, 'pin-centery')}
                data-testid={getTestId(props.name, 'pin-centery')}
                stroke={getStrokeColor(
                  controlStyles,
                  props.framePoints,
                  props.mixed,
                  FramePoint.CenterY,
                )}
                strokeDasharray={getStrokeDashArray(
                  props.framePoints,
                  props.mixed,
                  FramePoint.CenterY,
                )}
                strokeLinecap='round'
              />
              <g transform={`translate(${HorizontalDividerStart},${VerticalDividerStart})`}>
                <path
                  d={`M 0,0 0,${VerticalDividerHeight} ${HorizontalDividerWidth},0 ${HorizontalDividerWidth},${VerticalDividerHeight} z`}
                  className='pin-indicator'
                  id={getTestId(props.name, 'pin-centerx-transparent')}
                  data-testid={getTestId(props.name, 'pin-centerx-transparent')}
                  stroke='transparent'
                  fill='transparent'
                  onMouseDown={Utils.NO_OP}
                />
                <path
                  d={`M 0,0 ${HorizontalDividerWidth},0 0,${VerticalDividerHeight} ${HorizontalDividerWidth},${VerticalDividerHeight} z`}
                  className='pin-indicator'
                  id={getTestId(props.name, 'pin-centery-transparent')}
                  data-testid={getTestId(props.name, 'pin-centery-transparent')}
                  stroke='transparent'
                  fill='transparent'
                  onMouseDown={Utils.NO_OP}
                />
              </g>
            </>,
          )}
        </g>
      </svg>
    </div>
  )
}

const DimensionWidth = 20
const DimensionHeight = 20
const DimensionVerticalMid = DimensionHeight / 2
const DimensionHorizontalMid = DimensionWidth / 2

const DimensionInset = 5
const DimensionStart = DimensionInset
const HorizontalDimensionEnd = DimensionWidth - DimensionInset
const VerticalDimensionEnd = DimensionHeight - DimensionInset
const DimensionButt = 4
const HorizontalDimensionButtStart = DimensionHorizontalMid - DimensionButt / 2
const VerticalDimensionButtStart = DimensionVerticalMid - DimensionButt / 2

interface PinWidthControlProps {
  controlStatus: ControlStatus
  framePins: FramePinsInfo
  mixed?: boolean
  toggleWidth: () => void
}

export const PinWidthControl = React.memo((props: PinWidthControlProps) => {
  const controlStyles: ControlStyles = getControlStyles(props.controlStatus)
  return (
    <SquareButton onClick={props.toggleWidth} highlight={true} spotlight={true}>
      <svg width='20' height='20'>
        <g
          id='dimensioncontrols-pin-width'
          stroke={getStrokeColor(controlStyles, props.framePins, props.mixed, FramePoint.Width)}
        >
          <path
            d={`M${DimensionStart},${VerticalDimensionButtStart} l0,${DimensionButt}`}
            id='dimensioncontrols-pin-width-EdgeEnd-l'
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalDimensionEnd},${VerticalDimensionButtStart} l0,${DimensionButt}`}
            id='dimensioncontrols-pin-width-EdgeEnd-r'
            strokeLinecap='round'
          />
          <path
            d={`M${DimensionStart},${DimensionVerticalMid} L${HorizontalDimensionEnd},${DimensionVerticalMid}`}
            id='dimensioncontrols-pin-width-line'
            strokeDasharray={getStrokeDashArray(props.framePins, props.mixed, FramePoint.Width)}
            strokeLinecap='round'
          />
          <path
            d={`M 0,0 0,${DimensionHeight} ${DimensionWidth},0 ${DimensionWidth},${DimensionHeight} z`}
            strokeLinecap='butt'
            id='dimensioncontrols-pin-width-transparent'
            stroke='transparent'
            fill='transparent'
          />
        </g>
      </svg>
    </SquareButton>
  )
})

interface PinHeightControlProps {
  controlStatus: ControlStatus
  framePins: FramePinsInfo
  mixed?: boolean
  toggleHeight: () => void
}

export const PinHeightControl = React.memo((props: PinHeightControlProps) => {
  const controlStyles: ControlStyles = getControlStyles(props.controlStatus)
  return (
    <SquareButton onClick={props.toggleHeight} highlight={true} spotlight={true}>
      <svg width='20' height='20'>
        <g
          id='dimensioncontrols-pin-height'
          stroke={getStrokeColor(controlStyles, props.framePins, props.mixed, FramePoint.Height)}
        >
          <path
            d={`M${HorizontalDimensionButtStart},${DimensionStart} l${DimensionButt},0`}
            id='dimensioncontrols-pin-height-EdgeEnd-t'
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalDimensionButtStart},${VerticalDimensionEnd} l${DimensionButt},0`}
            id='dimensioncontrols-pin-height-EdgeEnd-b'
            strokeLinecap='round'
          />
          <path
            d={`M${DimensionHorizontalMid},${DimensionStart} L${DimensionHorizontalMid},${VerticalDimensionEnd}`}
            id='dimensioncontrols-pin-height-line'
            strokeDasharray={getStrokeDashArray(props.framePins, props.mixed, FramePoint.Height)}
            strokeLinecap='round'
          />
          <path
            d={`M 0,0 ${DimensionWidth},0 0,${DimensionHeight} ${DimensionWidth},${DimensionHeight} z`}
            id='dimensioncontrols-pin-width-transparent'
            stroke='transparent'
            fill='transparent'
          />
        </g>
      </svg>
    </SquareButton>
  )
})
