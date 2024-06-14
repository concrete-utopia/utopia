import React from 'react'
import Utils from '../../../utils/utils'
import type { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../core/shared/math-utils'
import type { Guideline, XAxisGuideline, YAxisGuideline } from '../guideline'
import { Guidelines } from '../guideline'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme } from '../../../uuiui'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

const StrokeColor = colorTheme.brandNeonPink.value
const LineEndSegmentSize = 3.5

type GuidelineWithDistance = {
  guideline: Guideline
  distance: number
}

function guidelinesClosestToDragOrFrame(
  frame: CanvasRectangle,
  guidelines: Array<Guideline>,
  includeCentre: boolean,
): Array<Guideline> {
  let xAxisGuideline: GuidelineWithDistance | null = null
  let yAxisGuideline: GuidelineWithDistance | null = null
  for (const guideline of guidelines) {
    const { distance } = Guidelines.distanceFromFrameToGuideline(frame, guideline, includeCentre)
    switch (guideline.type) {
      case 'XAxisGuideline':
        if (xAxisGuideline == null) {
          xAxisGuideline = { guideline: guideline, distance: distance }
        } else if (distance < xAxisGuideline.distance) {
          xAxisGuideline = { guideline: guideline, distance: distance }
        }
        break
      case 'YAxisGuideline':
        if (yAxisGuideline == null) {
          yAxisGuideline = { guideline: guideline, distance: distance }
        } else if (distance < yAxisGuideline.distance) {
          yAxisGuideline = { guideline: guideline, distance: distance }
        }
        break
      case 'CornerGuideline':
        // Skip these for now.
        break
      default:
        const _exhaustiveCheck: never = guideline
        throw new Error(`Unhandled guideline ${JSON.stringify(guideline)}`)
    }
  }
  // We want corner guidelines to take precendence over the edge guidelines.
  // This means we don't awkward snap to a corner and an edge.
  Utils.fastForEach(guidelines, (guideline) => {
    switch (guideline.type) {
      case 'CornerGuideline':
        const { distance } = Guidelines.distanceFromFrameToGuideline(
          frame,
          guideline,
          includeCentre,
        )
        const willSetToX = xAxisGuideline == null || distance < xAxisGuideline.distance
        const willSetToY = yAxisGuideline == null || distance < yAxisGuideline.distance
        if (willSetToX || willSetToY) {
          xAxisGuideline = { guideline: guideline, distance: distance }
          yAxisGuideline = { guideline: guideline, distance: distance }
        }
        break
      case 'XAxisGuideline':
      case 'YAxisGuideline':
        // We've already handled these.
        break
      default:
        const _exhaustiveCheck: never = guideline
        throw new Error(`Unhandled guideline ${JSON.stringify(guideline)}`)
    }
  })
  if (xAxisGuideline == null) {
    if (yAxisGuideline == null) {
      return []
    } else {
      return [yAxisGuideline.guideline]
    }
  } else {
    if (yAxisGuideline == null) {
      return [xAxisGuideline.guideline]
    } else {
      return [xAxisGuideline.guideline, yAxisGuideline.guideline]
    }
  }
}

interface DistanceGuidelineProps {
  canvasOffset: CanvasVector
  scale: number
  boundingBox: CanvasRectangle
  guidelines: Array<Guideline>
}
export class DistanceGuideline extends React.Component<DistanceGuidelineProps> {
  getNewControlForDistance(
    distance: number,
    from: CanvasPoint,
    to: CanvasPoint,
    id: string,
  ): Array<JSX.Element> {
    const horizontal = from.y === to.y
    const isFromBottomRight = from.x > to.x || from.y > to.y

    // it looks like this: |---|
    // the closing end is shifted with 1px to make it appear on the edge not after
    return [
      this.getLineEndControl(from, horizontal, `${id}-start`, isFromBottomRight),
      this.getLineControl(from, to, id, horizontal),
      this.getLineEndControl(to, horizontal, `${id}-end`, !isFromBottomRight),
      this.getDistanceTextControl(from, to, distance, horizontal, `${id}-label`),
    ]
  }

  getDistanceTextControl(
    from: CanvasPoint,
    to: CanvasPoint,
    distance: number,
    isHorizontal: boolean,
    id: string,
  ): JSX.Element {
    return (
      <CanvasOffsetWrapper>
        <div
          data-testid={`distance-text-container-${id}`}
          style={{
            visibility: distance > 0 ? 'visible' : 'hidden',
            position: 'absolute',
            top: Math.min(from.y, to.y) + (isHorizontal ? 4 : 0), // Offset by 4px to keep the control off the line.
            left: Math.min(from.x, to.x) + (isHorizontal ? 0 : 4), // Offset by 4px to keep the control off the line.
            width: isHorizontal ? Math.abs(from.x - to.x) : `max-content`,
            height: isHorizontal ? `max-content` : Math.abs(from.y - to.y),
            pointerEvents: 'none',
            display: 'flex',
            flexDirection: isHorizontal ? 'row' : 'column',
            justifyContent: 'center',
          }}
        >
          <div
            style={{
              padding: `0 ${2 / this.props.scale}`,
              borderRadius: 2 / this.props.scale,
              color: colorTheme.white.value,
              backgroundColor: StrokeColor,
              fontSize: 11 / this.props.scale,
              width: 'min-content',
              display: 'flex',
              alignItems: 'center',
              height: `${16 / this.props.scale}px`,
            }}
            data-testid={id}
          >
            {`${distance.toFixed(0)}`}
          </div>
        </div>
      </CanvasOffsetWrapper>
    )
  }

  getLineControl(from: CanvasPoint, to: CanvasPoint, id: string, horizontal: boolean): any {
    const topLeft = {
      x: Math.min(from.x, to.x),
      y: Math.min(from.y, to.y),
    } as CanvasPoint
    const strokeWidth = 1 / this.props.scale

    // offset with half of the strokewidth to make the line centered
    const strokeWidthOffset = {
      x: horizontal ? 0 : -strokeWidth / 2,
      y: horizontal ? -strokeWidth / 2 : 0,
    } as CanvasPoint
    const position = Utils.offsetPoint(topLeft, strokeWidthOffset)

    return (
      <div
        key={id}
        style={{
          left: this.props.canvasOffset.x + position.x,
          top: this.props.canvasOffset.y + position.y,
          position: 'absolute',
          width: horizontal ? Math.abs(to.x - from.x) : strokeWidth,
          height: horizontal ? strokeWidth : Math.abs(to.y - from.y),
          backgroundColor: StrokeColor,
          pointerEvents: 'none',
        }}
      />
    )
  }

  getLineEndControl(
    origin: CanvasPoint,
    horizontal: boolean,
    id: string,
    needsStrokeWidthOffset: boolean,
  ): any {
    const segmentSize = LineEndSegmentSize
    const horizontalSize = horizontal ? 0 : segmentSize / this.props.scale
    const verticalSize = horizontal ? segmentSize / this.props.scale : 0
    const topLeft = {
      x: -horizontalSize,
      y: -verticalSize,
    } as CanvasPoint

    const strokeWidth = 1 / this.props.scale
    const strokeWidthOffset = {
      x: horizontal ? -strokeWidth : 0,
      y: horizontal ? 0 : -strokeWidth,
    } as CanvasPoint

    const position = needsStrokeWidthOffset
      ? Utils.offsetPoint(Utils.offsetPoint(origin, topLeft), strokeWidthOffset)
      : Utils.offsetPoint(origin, topLeft)
    return (
      <div
        key={id}
        style={{
          left: this.props.canvasOffset.x + position.x,
          top: this.props.canvasOffset.y + position.y,
          position: 'absolute',
          width: horizontal ? strokeWidth : horizontalSize * 2,
          height: horizontal ? verticalSize * 2 : strokeWidth,
          backgroundColor: StrokeColor,
          pointerEvents: 'none',
        }}
      />
    )
  }

  distanceControlsFor(
    guidelines: Array<Guideline>,
    closestGuidelines: Array<Guideline>,
    isDragging: boolean,
    frame: CanvasRectangle,
    bothAxesOverlap: boolean,
  ): Array<JSX.Element> {
    if (bothAxesOverlap) {
      const xAxisGuidelines = guidelines.filter(
        (g) => g.type === 'XAxisGuideline',
      ) as Array<XAxisGuideline>
      const yAxisGuidelines = guidelines.filter(
        (g) => g.type === 'YAxisGuideline',
      ) as Array<YAxisGuideline>
      const guidelineXs = xAxisGuidelines.map((g) => g.x)
      const guidelineYs = yAxisGuidelines.map((g) => g.y)
      const leftGuidelineX = Math.min(...guidelineXs)
      const rightGuidelineX = Math.max(...guidelineXs)
      const topGuidelineY = Math.min(...guidelineYs)
      const bottomGuidelineY = Math.max(...guidelineYs)

      const midX = frame.x + frame.width / 2
      const midY = frame.y + frame.height / 2
      const leftStart = { x: leftGuidelineX, y: midY } as CanvasPoint
      const leftEnd = { x: frame.x, y: midY } as CanvasPoint
      const rightStart = { x: rightGuidelineX, y: midY } as CanvasPoint
      const rightEnd = { x: frame.x + frame.width, y: midY } as CanvasPoint
      const topStart = { x: midX, y: topGuidelineY } as CanvasPoint
      const topEnd = { x: midX, y: frame.y } as CanvasPoint
      const bottomStart = { x: midX, y: bottomGuidelineY } as CanvasPoint
      const bottomEnd = { x: midX, y: frame.y + frame.height } as CanvasPoint

      return [
        ...this.getNewControlForDistance(
          Math.abs(frame.x - leftGuidelineX),
          leftStart,
          leftEnd,
          'distance-left',
        ),
        ...this.getNewControlForDistance(
          Math.abs(frame.x + frame.width - rightGuidelineX),
          rightStart,
          rightEnd,
          'distance-right',
        ),
        ...this.getNewControlForDistance(
          Math.abs(frame.y - topGuidelineY),
          topStart,
          topEnd,
          'distance-top',
        ),
        ...this.getNewControlForDistance(
          Math.abs(frame.y + frame.height - bottomGuidelineY),
          bottomStart,
          bottomEnd,
          'distance-bottom',
        ),
      ]
    } else {
      return closestGuidelines.flatMap((guideline, i) => {
        const { distance, from, to } = Guidelines.distanceFromFrameToGuideline(
          frame,
          guideline,
          isDragging,
        )
        return this.getNewControlForDistance(distance, from, to, `distance-${i}`)
      })
    }
  }

  static doBothAxesOverlap(guidelines: Array<Guideline>, boundingRectangle: CanvasRectangle) {
    function leftOfRightEdge(guideline: Guideline) {
      return (
        guideline.type === 'XAxisGuideline' &&
        guideline.x < boundingRectangle.x + boundingRectangle.width
      )
    }

    function rightOfLeftEdge(guideline: Guideline) {
      return guideline.type === 'XAxisGuideline' && guideline.x > boundingRectangle.x
    }

    function belowTopEdge(guideline: Guideline) {
      return guideline.type === 'YAxisGuideline' && guideline.y > boundingRectangle.y
    }

    function aboveBottomEdge(guideline: Guideline) {
      return (
        guideline.type === 'YAxisGuideline' &&
        guideline.y < boundingRectangle.y + boundingRectangle.height
      )
    }

    const xAxisOverlaps = guidelines.some(leftOfRightEdge) && guidelines.some(rightOfLeftEdge)
    const yAxisOverlaps = guidelines.some(belowTopEdge) && guidelines.some(aboveBottomEdge)
    return xAxisOverlaps && yAxisOverlaps
  }

  render() {
    const closestGuidelines = guidelinesClosestToDragOrFrame(
      this.props.boundingBox,
      this.props.guidelines,
      false,
    )
    const bothAxesOverlap = DistanceGuideline.doBothAxesOverlap(
      this.props.guidelines,
      this.props.boundingBox,
    )
    const distanceControls = this.distanceControlsFor(
      this.props.guidelines,
      closestGuidelines,
      false,
      this.props.boundingBox,
      bothAxesOverlap,
    )

    return <div className='role-distance-guidelines'>{distanceControls}</div>
  }
}
