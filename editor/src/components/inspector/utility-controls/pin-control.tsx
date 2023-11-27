import React from 'react'
import Utils from '../../../utils/utils'
import type { ControlStyles } from '../common/control-styles'
import type { ControlStatus } from '../common/control-status'
import { getControlStyles } from '../common/control-styles'
import { FramePoint } from 'utopia-api/core'
import type {
  LayoutPinnedProp,
  LayoutPinnedPropIncludingCenter,
} from '../../../core/layout/layout-helpers-new'
import type { FramePinsInfo } from '../common/layout-property-path-hooks'
import { UtopiaTheme, SquareButton, colorTheme } from '../../../uuiui'

interface PinControlProps {
  handlePinMouseDown: (frameProp: LayoutPinnedProp) => void
  controlStatus: ControlStatus
  framePoints: FramePinsInfo
  mixed?: boolean
  className?: string
  style?: React.CSSProperties
}

const Margin = 3
const Width = 66
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
  isGroupChild?: 'group-child' | 'frame-child',
) {
  const isPrimary = Utils.propOr(false, 'isPrimaryPosition', framePoints[point])

  if (isGroupChild === 'group-child') {
    // only set width/height is using main color
    const isRelative = Utils.propOr(false, 'isRelativePosition', framePoints[point])

    return (isPrimary && isRelative) || !isPrimary
      ? controlStyles.secondaryColor
      : controlStyles.mainColor
  }
  if (isPrimary && !mixed) {
    return controlStyles.mainColor
  } else {
    return controlStyles.secondaryColor
  }
}

function getStrokeDashArray(
  framePoints: FramePinsInfo,
  mixed: boolean | undefined,
  point: FramePoint,
  isGroupChild?: 'group-child' | 'frame-child',
) {
  // unset width/height for group children are not using dashed style
  if (isGroupChild === 'group-child') {
    return '0'
  }

  const isRelative = Utils.propOr(false, 'isRelativePosition', framePoints[point])

  if (isRelative && !mixed) {
    return '1 2'
  } else {
    return '0'
  }
}

export const PinControl = (props: PinControlProps) => {
  const controlStyles: ControlStyles = getControlStyles(props.controlStatus)

  const handlePinMouseDown = (frameProp: LayoutPinnedProp) => () => {
    props.handlePinMouseDown(frameProp)
  }

  return (
    <div id='pin-control' className={props.className} style={props.style}>
      <svg
        width='100%'
        viewBox={`0 0 ${Width} ${Height}`}
        version='1.1'
        xmlns='http://www.w3.org/2000/svg'
        vectorEffect='non-scaling-stroke'
      >
        <rect
          id='positioncontrols-box'
          fill={controlStyles.backgroundColor}
          stroke={controlStyles.borderColor}
          strokeWidth='1'
          x='0.5'
          y='0.5'
          width={Width - 1}
          height={Height - 2}
          rx={UtopiaTheme.inputBorderRadius}
        />
        <rect
          id='positioncontrols-divider'
          fill={controlStyles.backgroundColor}
          stroke={controlStyles.borderColor}
          strokeWidth='1'
          x={HorizontalDividerStart}
          y={VerticalDividerStart}
          width={HorizontalDividerWidth}
          height={VerticalDividerHeight}
          rx={UtopiaTheme.inputBorderRadius}
        />
        <g id='positioncontrols-pins' strokeWidth='1'>
          <path
            d={`M${HorizontalMid},${VerticalStart} l0,${VerticalLength}`}
            className='pin-indicator'
            id='positioncontrols-pin-top'
            stroke={getStrokeColor(controlStyles, props.framePoints, props.mixed, FramePoint.Top)}
            strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.Top)}
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalMid},${VerticalStart} l0,${VerticalLength}`}
            strokeWidth={MouseCatchmentStrokeWidth}
            stroke='transparent'
            strokeLinecap='butt'
            onMouseDown={handlePinMouseDown('top')}
          />
          <path
            d={`M${HorizontalMid},${VerticalMid - VerticalLength} l0,${VerticalLength * 2}`}
            className='pin-indicator'
            id='positioncontrols-pin-centery'
            stroke={getStrokeColor(
              controlStyles,
              props.framePoints,
              props.mixed,
              FramePoint.CenterY,
            )}
            strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.CenterY)}
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalMid},${VerticalEnd} l0,${VerticalLength}`}
            className='pin-indicator'
            id='positioncontrols-pin-bottom'
            stroke={getStrokeColor(
              controlStyles,
              props.framePoints,
              props.mixed,
              FramePoint.Bottom,
            )}
            strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.Bottom)}
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalMid},${VerticalEnd} l0,${VerticalLength}`}
            strokeWidth={MouseCatchmentStrokeWidth}
            stroke='transparent'
            strokeLinecap='butt'
            onMouseDown={handlePinMouseDown('bottom')}
          />
          <path
            d={`M${HorizontalEnd},${VerticalMid} l${HorizontalLength},0`}
            className='pin-indicator'
            id='positioncontrols-pin-right'
            stroke={getStrokeColor(controlStyles, props.framePoints, props.mixed, FramePoint.Right)}
            strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.Right)}
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalEnd},${VerticalMid} l${HorizontalLength},0`}
            strokeWidth={MouseCatchmentStrokeWidth}
            stroke='transparent'
            strokeLinecap='butt'
            onMouseDown={handlePinMouseDown('right')}
          />
          <path
            d={`M${HorizontalMid - HorizontalLength},${VerticalMid} l${HorizontalLength * 2},0`}
            className='pin-indicator'
            id='positioncontrols-pin-centerx'
            stroke={getStrokeColor(
              controlStyles,
              props.framePoints,
              props.mixed,
              FramePoint.CenterX,
            )}
            strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.CenterX)}
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalStart},${VerticalMid} l${HorizontalLength},0`}
            className='pin-indicator'
            id='positioncontrols-pin-left'
            stroke={getStrokeColor(controlStyles, props.framePoints, props.mixed, FramePoint.Left)}
            strokeDasharray={getStrokeDashArray(props.framePoints, props.mixed, FramePoint.Left)}
            strokeLinecap='round'
          />
          <path
            d={`M${HorizontalStart},${VerticalMid} l${HorizontalLength},0`}
            strokeWidth={MouseCatchmentStrokeWidth}
            stroke='transparent'
            strokeLinecap='butt'
            onMouseDown={handlePinMouseDown('left')}
          />
          <g transform={`translate(${HorizontalDividerStart},${VerticalDividerStart})`}>
            <path
              d={`M 0,0 0,${VerticalDividerHeight} ${HorizontalDividerWidth},0 ${HorizontalDividerWidth},${VerticalDividerHeight} z`}
              className='pin-indicator'
              id='positioncontrols-pin-centerx-transparent'
              stroke='transparent'
              fill='transparent'
              onMouseDown={Utils.NO_OP}
            />
            <path
              d={`M 0,0 ${HorizontalDividerWidth},0 0,${VerticalDividerHeight} ${HorizontalDividerWidth},${VerticalDividerHeight} z`}
              className='pin-indicator'
              id='positioncontrols-pin-centery-transparent'
              stroke='transparent'
              fill='transparent'
              onMouseDown={Utils.NO_OP}
            />
          </g>
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
  handlePinMouseDown: (
    frameProp: LayoutPinnedPropIncludingCenter,
    event: React.MouseEvent<Element, MouseEvent>,
  ) => void
  isGroupChild: 'group-child' | 'frame-child'
}

export const PinWidthSVG = React.memo(() => {
  return (
    <svg width='20' height='20'>
      <g id='dimensioncontrols-pin-width' stroke={colorTheme.fg1.value}>
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
  )
})

export const PinHeightSVG = React.memo(() => {
  return (
    <svg width='20' height='20'>
      <g id='dimensioncontrols-pin-height' stroke={colorTheme.fg1.value}>
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
  )
})

export const PinWidthControl = React.memo((props: PinWidthControlProps) => {
  const { handlePinMouseDown } = props

  const onClick = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
      handlePinMouseDown('width', event)
    },
    [handlePinMouseDown],
  )

  const controlStyles: ControlStyles = getControlStyles(props.controlStatus)

  return (
    <SquareButton data-testid={'pin-width-control-button'} onClick={onClick}>
      <svg width='20' height='20'>
        <g
          id='dimensioncontrols-pin-width'
          stroke={getStrokeColor(
            controlStyles,
            props.framePins,
            props.mixed,
            FramePoint.Width,
            props.isGroupChild,
          )}
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
            strokeDasharray={getStrokeDashArray(
              props.framePins,
              props.mixed,
              FramePoint.Width,
              props.isGroupChild,
            )}
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
  handlePinMouseDown: (
    frameProp: LayoutPinnedPropIncludingCenter,
    event: React.MouseEvent<Element, MouseEvent>,
  ) => void
  isGroupChild: 'group-child' | 'frame-child'
}

export const PinHeightControl = React.memo((props: PinHeightControlProps) => {
  const controlStyles: ControlStyles = getControlStyles(props.controlStatus)
  const { handlePinMouseDown } = props

  const onClick = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
      handlePinMouseDown('height', event)
    },
    [handlePinMouseDown],
  )

  return (
    <SquareButton data-testid={'pin-height-control-button'} onClick={onClick}>
      <svg width='20' height='20'>
        <g
          id='dimensioncontrols-pin-height'
          stroke={getStrokeColor(
            controlStyles,
            props.framePins,
            props.mixed,
            FramePoint.Height,
            props.isGroupChild,
          )}
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
            strokeDasharray={getStrokeDashArray(
              props.framePins,
              props.mixed,
              FramePoint.Height,
              props.isGroupChild,
            )}
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
