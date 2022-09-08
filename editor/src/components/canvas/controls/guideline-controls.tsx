import React from 'react'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { Utils } from '../../../uuiui-deps'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  lineIntersection,
  rectanglesEqual,
} from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { EditorStorePatched } from '../../editor/store/editor-state'
import {
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { Guideline } from '../guideline'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'

// STRATEGY GUIDELINE CONTROLS
export const GuidelineControls = React.memo(() => {
  const scale = useEditorState(scaleSelector, 'Guideline scale')
  const strategyMovedSuccessfully = useEditorState((store) => {
    return (
      store.editor.canvas.controls.strategyIntendedBounds.length > 0 &&
      store.editor.canvas.controls.strategyIntendedBounds.every(({ target, frame }) => {
        const measuredFrame = MetadataUtils.getFrameInCanvasCoords(target, store.editor.jsxMetadata)
        if (measuredFrame == null) {
          return false
        } else {
          return rectanglesEqual(measuredFrame, frame)
        }
      })
    )
  }, 'GuidelineControls strategyMovedSuccessfully')

  const metadata = useEditorState((store) => store.editor.jsxMetadata, 'JSX metadata')

  const { strategyIntendedBounds, snappingGuidelines } = useEditorState(
    (store) => store.editor.canvas.controls,
    'Strategy intended bounds and snapping guidelines',
  )

  const parentAndSiblings = MetadataUtils.collectParentsAndSiblings(
    metadata,
    strategyIntendedBounds.map((bound) => bound.target),
  )

  const intersectionFrames = [
    ...strategyIntendedBounds.map((bound) => bound.frame),
    ...framesFromMetadata(metadata, parentAndSiblings),
  ]

  const snappingGuidelinesPrefix = snappingGuidelines.slice(0, 4)

  const intersectionPoints = intersectionFrames.flatMap((bound) =>
    snappingGuidelinesPrefix.flatMap(
      (guideLine) =>
        Utils.optionalMap(
          (line) => segmentRectangleIntersections(line, bound),
          guidelineToSegment(guideLine.guideline),
        ) ?? [],
    ),
  )

  const guidelineEndpointss = snappingGuidelines.flatMap(({ guideline }) =>
    guidelineEndpoints(guideline),
  )
  const xMarkPoints = Utils.deduplicateBy(
    ({ x, y }) => `${x}${y}`,
    [...guidelineEndpointss, ...intersectionPoints],
  )

  if (!strategyMovedSuccessfully) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <GuidelineControl index={0} />
        <GuidelineControl index={1} />
        <GuidelineControl index={2} />
        <GuidelineControl index={3} />
        {xMarkPoints.map((point, idx) => (
          <XMarkControl key={idx} point={point} scale={scale} index={idx} />
        ))}
      </CanvasOffsetWrapper>
    )
  }
})

interface GuidelineProps {
  index: number
}

const LineWidth = 1
const scaleSelector = (store: EditorStorePatched) => store.editor.canvas.scale

const GuidelineControl = React.memo<GuidelineProps>((props) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(scaleSelector, 'Guideline scale')
  const controlRef = useGuideline(props.index, (result: { frame: CanvasRectangle } | null) => {
    if (controlRef.current != null) {
      if (result == null) {
        controlRef.current.style.setProperty('display', 'none')
      } else {
        controlRef.current.style.setProperty('display', 'block')
        controlRef.current.style.setProperty('left', `${result.frame.x - 0.5 / scale}px`)
        controlRef.current.style.setProperty('top', `${result.frame.y - 0.5 / scale}px`)
        controlRef.current.style.setProperty('width', `${result.frame.width}px`)
        controlRef.current.style.setProperty('height', `${result.frame.height}px`)
        controlRef.current.style.setProperty('border-style', 'solid')
      }
    }
  })

  const key = `guideline-${props.index}`
  return (
    <div
      id={key}
      key={key}
      data-testid={key}
      ref={controlRef}
      style={{
        position: 'absolute',
        pointerEvents: 'none',
        borderWidth: 0,
        borderLeftWidth: LineWidth / scale,
        borderTopWidth: LineWidth / scale,
        borderColor: colorTheme.canvasLayoutStroke.value,
      }}
    />
  )
})

function useGuideline<T = HTMLDivElement>(
  index: number,
  onChangeCallback: (result: { frame: CanvasRectangle } | null) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)

  const guidelineCallback = React.useCallback(
    (result: { frame: CanvasRectangle } | null) => {
      if (controlRef.current != null) {
        onChangeCallback(result)
      }
    },
    [onChangeCallback],
  )

  const guidelineCallbackRef = React.useRef(guidelineCallback)
  guidelineCallbackRef.current = guidelineCallback

  const guidelineRef = useRefEditorState((store) => store.editor.canvas.controls.snappingGuidelines)

  const innerCallback = React.useCallback(() => {
    if (guidelineRef.current[index] != null) {
      const guidelineWithSnapping = guidelineRef.current[index]
      const guideline = guidelineWithSnapping.guideline
      switch (guideline.type) {
        case 'XAxisGuideline': {
          const frame = canvasRectangle({
            x: guideline.x,
            y: guideline.yTop,
            width: 0,
            height: guideline.yBottom - guideline.yTop,
          })
          guidelineCallbackRef.current({
            frame: frame,
          })
          break
        }
        case 'YAxisGuideline': {
          const frame = canvasRectangle({
            x: guideline.xLeft,
            y: guideline.y,
            width: guideline.xRight - guideline.xLeft,
            height: 0,
          })
          guidelineCallbackRef.current({
            frame: frame,
          })
          break
        }
        case 'CornerGuideline': // TODO missing corner guideline
        default:
          break
      }
    } else {
      guidelineCallbackRef.current(null)
    }
  }, [guidelineRef, guidelineCallbackRef, index])
  useSelectorWithCallback(
    (store) => store.editor.canvas.controls.snappingGuidelines[index],
    innerCallback,
  )
  useSelectorWithCallback(
    (store) => store.editor.canvas.controls.snappingGuidelines.length,
    innerCallback,
  )
  React.useEffect(() => {
    innerCallback()
  }, [innerCallback])
  return controlRef
}

function framesFromMetadata(
  metadataMap: ElementInstanceMetadataMap,
  paths: Array<ElementPath>,
): Array<CanvasRectangle> {
  return mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadataMap, path)?.globalFrame ?? null,
    paths,
  )
}

interface CanvasSegment {
  a: CanvasPoint
  b: CanvasPoint
}

function canvasSegment(a: CanvasPoint, b: CanvasPoint): CanvasSegment {
  return { a: a, b: b }
}

function guidelineToSegment(guideline: Guideline): CanvasSegment | null {
  switch (guideline.type) {
    case 'XAxisGuideline': {
      const a = canvasPoint({ x: guideline.x, y: guideline.yBottom })
      const b = canvasPoint({ x: guideline.x, y: guideline.yTop })
      return canvasSegment(a, b)
    }
    case 'YAxisGuideline': {
      const a = canvasPoint({ x: guideline.xLeft, y: guideline.y })
      const b = canvasPoint({ x: guideline.xRight, y: guideline.y })
      return canvasSegment(a, b)
    }
    case 'CornerGuideline':
      return null
    default:
      return Utils.assertNever(guideline)
  }
}

function guidelineEndpoints(guideline: Guideline): Array<CanvasPoint> {
  switch (guideline.type) {
    case 'XAxisGuideline': {
      const a = canvasPoint({ x: guideline.x, y: guideline.yBottom })
      const b = canvasPoint({ x: guideline.x, y: guideline.yTop })
      return [a, b]
    }
    case 'YAxisGuideline': {
      const a = canvasPoint({ x: guideline.xLeft, y: guideline.y })
      const b = canvasPoint({ x: guideline.xRight, y: guideline.y })
      return [a, b]
    }
    case 'CornerGuideline':
      return []
    default:
      return Utils.assertNever(guideline)
  }
}

function rectangleBoundingLines(rectangle: CanvasRectangle): CanvasSegment[] {
  return [
    canvasSegment(
      canvasPoint({ x: rectangle.x, y: rectangle.y }),
      canvasPoint({ x: rectangle.x, y: rectangle.y + rectangle.height }),
    ),
    canvasSegment(
      canvasPoint({ x: rectangle.x, y: rectangle.y }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y }),
    ),
    canvasSegment(
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y + rectangle.height }),
    ),
    canvasSegment(
      canvasPoint({ x: rectangle.x, y: rectangle.y + rectangle.height }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y + rectangle.height }),
    ),
  ]
}

// https://algs4.cs.princeton.edu/91primitives/
function segmentsIntersect(a: CanvasSegment, b: CanvasSegment): boolean {
  function counterClockwise(p1: CanvasPoint, p2: CanvasPoint, p3: CanvasPoint): number {
    return (p2.y - p1.y) * (p3.x - p1.x) - (p3.y - p1.y) * (p2.x - p1.x)
  }

  if (counterClockwise(a.a, a.b, b.a) * counterClockwise(a.a, a.b, b.b) > 0) {
    return false
  }
  if (counterClockwise(b.a, b.b, a.a) * counterClockwise(b.a, b.b, a.b) > 0) {
    return false
  }

  return true
}

function segmentIntersection(left: CanvasSegment, right: CanvasSegment): CanvasPoint | null {
  const point = lineIntersection(left.a, left.b, right.a, right.b)
  if (segmentsIntersect(left, right)) {
    return point
  }
  return null
}

function segmentRectangleIntersections(
  line: CanvasSegment,
  rectangle: CanvasRectangle,
): CanvasPoint[] {
  const boundingLines = rectangleBoundingLines(rectangle)
  const intersectionPoints = mapDropNulls(
    (boundingLine) => segmentIntersection(line, boundingLine),
    boundingLines,
  )
  return intersectionPoints
}

interface XMarkControlProps {
  point: CanvasPoint
  scale: number
  index: number
}

const XMarkControlSize = 5

const XMarkControl = React.memo<XMarkControlProps>(({ point, scale, index }) => {
  const width = XMarkControlSize
  const height = XMarkControlSize

  return (
    <div
      data-testid={`xmark-${index}`}
      style={{
        position: 'absolute',
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        left: point.x - width / 2,
        top: point.y - height / 2,
        width,
        height,
        transform: `scale(${1 / scale})`,
      }}
    >
      <XMark />
    </div>
  )
})

const XMark = React.memo(() => (
  <svg
    width='5px'
    height='5px'
    viewBox='0 0 5.0 5.0'
    version='1.1'
    xmlns='http://www.w3.org/2000/svg'
  >
    <path d='M0.5,4.5 L4.5,0.5' stroke='#FF00aa' strokeWidth='0.66' fill='none' />
    <path d='M0.5,0.5 L4.5,4.5' stroke='#FF00aa' strokeWidth='0.66' fill='none' />
  </svg>
))
