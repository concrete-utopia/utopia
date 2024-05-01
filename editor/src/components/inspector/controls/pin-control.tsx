import React from 'react'
import Utils from '../../../utils/utils'
import type { ControlStyles } from '../common/control-styles'
import type { ControlStatus } from '../common/control-status'
import { getControlStyles } from '../common/control-styles'
import { FramePoint } from 'utopia-api/core'
import type { LayoutPinnedPropIncludingCenter } from '../../../core/layout/layout-helpers-new'
import { type FramePinsInfo } from '../common/layout-property-path-hooks'
import { UtopiaTheme, colorTheme } from '../../../uuiui'
import { unless, when } from '../../../utils/react-conditionals'
import { FlexCol, FlexRow } from 'utopia-api'
import type { DetectedPins } from '../simplified-pinning-helpers'
import { PinHeightControl, PinWidthControl } from '../utility-controls/pin-control'

interface PinControlProps {
  handlePinMouseDown: (
    frameProp: LayoutPinnedPropIncludingCenter,
    event: React.MouseEvent<Element>,
  ) => void
  name: string
  controlStatus: ControlStatus
  framePoints: FramePinsInfo
  mixed?: boolean
  className?: string
  style?: React.CSSProperties
  exclude?: ExcludePinControls
  regularBorder: boolean
}

export type ExcludePinControls = {
  center?: boolean
  sides?: boolean
}

const Margin = 2
const Width = 52
const Height = 52

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
    return colorTheme.fg1.value
  } else {
    return colorTheme.fg8.value
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

  const handlePinMouseDown =
    (frameProp: LayoutPinnedPropIncludingCenter) => (e: React.MouseEvent<Element>) => {
      props.handlePinMouseDown(frameProp, e)
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
    <div id='pin-control' className={props.className} style={{ ...props.style, height: 54 }}>
      <svg
        width={Width}
        viewBox={`0 0 ${Width} ${Height}`}
        version='1.1'
        xmlns='http://www.w3.org/2000/svg'
        vectorEffect='non-scaling-stroke'
        style={
          props.regularBorder
            ? { border: `1px solid ${colorTheme.fg8.value}`, borderRadius: 2 }
            : {}
        }
      >
        <rect
          id={getTestId(props.name, 'box')}
          data-testid={getTestId(props.name, 'box')}
          fill={colorTheme.bg1.value}
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
          fill={colorTheme.bg1.value}
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
                d={`M${HorizontalMid - (HorizontalLength - 4)},${VerticalMid} l${
                  (HorizontalLength - 4) * 2
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
                  onMouseDown={handlePinMouseDown('centerX')}
                />
                <path
                  d={`M 0,0 ${HorizontalDividerWidth},0 0,${VerticalDividerHeight} ${HorizontalDividerWidth},${VerticalDividerHeight} z`}
                  className='pin-indicator'
                  id={getTestId(props.name, 'pin-centery-transparent')}
                  data-testid={getTestId(props.name, 'pin-centery-transparent')}
                  stroke='transparent'
                  fill='transparent'
                  onMouseDown={handlePinMouseDown('centerY')}
                />
              </g>
            </>,
          )}
        </g>
      </svg>
    </div>
  )
}

export interface CombinedPinControlProps {
  pins: DetectedPins
  framePinsInfo: FramePinsInfo
  handlePinMouseDown: (
    frameProp: LayoutPinnedPropIncludingCenter,
    event: React.MouseEvent<Element, MouseEvent>,
  ) => void
  isGroupChild: 'group-child' | 'frame-child'
}

export const CombinedPinControl = React.memo((props: CombinedPinControlProps) => {
  return (
    <FlexRow css={{ border: `1px solid ${colorTheme.fg8.value}`, borderRadius: 2 }}>
      <PinControl
        handlePinMouseDown={props.handlePinMouseDown}
        name={'pin-control'}
        controlStatus={'simple'}
        framePoints={props.framePinsInfo}
        regularBorder={false}
        exclude={{ center: props.isGroupChild === 'group-child' }}
      />
      <FlexCol
        css={{
          justifyContent: 'space-evenly',
          borderLeft: `1px solid ${colorTheme.fg8.value}`,
        }}
      >
        <FlexRow css={{ flexGrow: 1, alignItems: 'center' }}>
          <PinWidthControl
            controlStatus={'simple'}
            framePins={props.framePinsInfo}
            handlePinMouseDown={props.handlePinMouseDown}
            isGroupChild={props.isGroupChild}
          />
        </FlexRow>
        <FlexRow css={{ flexGrow: 1, alignItems: 'center' }}>
          <PinHeightControl
            controlStatus={'simple'}
            framePins={props.framePinsInfo}
            handlePinMouseDown={props.handlePinMouseDown}
            isGroupChild={props.isGroupChild}
          />
        </FlexRow>
      </FlexCol>
    </FlexRow>
  )
})
