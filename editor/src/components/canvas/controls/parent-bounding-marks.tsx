import * as React from 'react'
import { CanvasPoint, CanvasRectangle, Vector } from '../../../core/shared/math-utils'
import { Utils } from '../../../uuiui-deps'
import { UtopiaTheme } from '../../../uuiui'

const BoundsHighlightOutlineID = 'parent-highlight-outline'

const BoundingMarkLength = 5
const BoundingMarkGap = 1

function createBoundingMark(
  x: number,
  y: number,
  directions: Vector<any>,
  idSuffix: string,
  scale: number,
  strokeColor: string,
) {
  const { x: xDirection, y: yDirection } = directions
  return (
    <g key={idSuffix} transform={`translate(${x}, ${y})`}>
      <path
        style={{
          stroke: strokeColor,
        }}
        d={`M 0 ${yDirection * (BoundingMarkGap / scale)} L 0 ${
          yDirection * (BoundingMarkLength / scale)
        }`}
        strokeWidth={1 / scale}
        id={`${BoundsHighlightOutlineID}-${idSuffix}-1`}
      />
      <path
        style={{
          stroke: strokeColor,
        }}
        d={`M ${xDirection * (BoundingMarkGap / scale)} 0 L ${
          xDirection * (BoundingMarkLength / scale)
        } 0`}
        id={`${BoundsHighlightOutlineID}-${idSuffix}-2`}
        strokeWidth={1 / scale}
      />
    </g>
  )
}

interface BoundingMarksProps {
  boundsType: 'immediateParent' | 'coordinateSystem'
  canvasOffset: CanvasPoint
  scale: number
  rect: CanvasRectangle
}

export const BoundingMarks: React.FunctionComponent<BoundingMarksProps> = (props) => {
  const frame = {
    x: Utils.roundToNearestHalf(props.rect.x + props.canvasOffset.x),
    y: Utils.roundToNearestHalf(props.rect.y + props.canvasOffset.y),
    width: Utils.roundToNearestHalf(props.rect.width),
    height: Utils.roundToNearestHalf(props.rect.height),
  }

  const strokeColor =
    props.boundsType === 'immediateParent'
      ? UtopiaTheme.color.canvasControlsImmediateParentMarks.value
      : UtopiaTheme.color.canvasControlsCoordinateSystemMarks.value

  return (
    <svg
      width={frame.width}
      height={frame.height}
      style={{
        position: 'absolute',
        pointerEvents: 'none',
        left: props.rect.x + props.canvasOffset.x,
        top: props.rect.y + props.canvasOffset.y,
      }}
    >
      {[
        createBoundingMark(0, 0, { x: -1, y: -1 }, 'tl', props.scale, strokeColor),
        createBoundingMark(frame.width, 0, { x: 1, y: -1 }, 'tr', props.scale, strokeColor),
        createBoundingMark(0, frame.height, { x: -1, y: 1 }, 'bl', props.scale, strokeColor),
        createBoundingMark(
          frame.width,
          frame.height,
          { x: 1, y: 1 },
          'br',
          props.scale,
          strokeColor,
        ),
      ]}
    </svg>
  )
}
