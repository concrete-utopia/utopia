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
import { collectParentsAndSiblings } from './guideline-helpers'
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

  const parentAndSiblings = collectParentsAndSiblings(
    metadata,
    strategyIntendedBounds.map((bound) => bound.target),
  )

  const intersectionFrames = [
    ...strategyIntendedBounds.map((_) => _.frame),
    ...framesFromMetadata(metadata, parentAndSiblings),
  ]

  const snappingGuidelinesPrefix = snappingGuidelines.slice(0, 4)

  const intersectionPoints = intersectionFrames.flatMap((bound) =>
    snappingGuidelinesPrefix.flatMap(
      (guideLine) =>
        Utils.optionalMap(
          (line) => spanRectangleIntersections(line, bound),
          guidelineToSpan(guideLine.guideline),
        ) ?? [],
    ),
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
        {intersectionPoints.map((point, idx) => (
          <XMarkControl data-testid={`xmark-${idx}`} key={idx} point={point} scale={scale} />
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

interface CanvasSpan {
  a: CanvasPoint
  b: CanvasPoint
}

function canvasSpan(a: CanvasPoint, b: CanvasPoint): CanvasSpan {
  return { a: a, b: b }
}

function guidelineToSpan(guideline: Guideline): CanvasSpan | null {
  switch (guideline.type) {
    case 'XAxisGuideline': {
      const a = canvasPoint({ x: guideline.x, y: guideline.yBottom })
      const b = canvasPoint({ x: guideline.x, y: guideline.yTop })
      return canvasSpan(a, b)
    }
    case 'YAxisGuideline': {
      const a = canvasPoint({ x: guideline.xLeft, y: guideline.y })
      const b = canvasPoint({ x: guideline.xRight, y: guideline.y })
      return canvasSpan(a, b)
    }
    case 'CornerGuideline':
      return null
    default:
      return Utils.assertNever(guideline)
  }
}

function rectangleBoundingLines(rectangle: CanvasRectangle): CanvasSpan[] {
  return [
    canvasSpan(
      canvasPoint({ x: rectangle.x, y: rectangle.y }),
      canvasPoint({ x: rectangle.x, y: rectangle.y + rectangle.height }),
    ),
    canvasSpan(
      canvasPoint({ x: rectangle.x, y: rectangle.y }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y }),
    ),
    canvasSpan(
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y + rectangle.height }),
    ),
    canvasSpan(
      canvasPoint({ x: rectangle.x, y: rectangle.y + rectangle.height }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y + rectangle.height }),
    ),
  ]
}

// https://stackoverflow.com/a/9997374
function spansIntersect(a: CanvasSpan, b: CanvasSpan): boolean {
  function ccw(p1: CanvasPoint, p2: CanvasPoint, p3: CanvasPoint): boolean {
    return (p2.y - p1.y) * (p3.x - p1.x) - (p3.y - p1.y) * (p2.x - p1.x) >= 0
  }

  return ccw(a.a, b.a, b.b) !== ccw(a.b, b.a, b.b) && ccw(a.a, a.b, b.a) !== ccw(a.a, a.b, b.b)
}

function spanIntersection(left: CanvasSpan, right: CanvasSpan): CanvasPoint | null {
  const point = lineIntersection(left.a, left.b, right.a, right.b)
  if (spansIntersect(left, right)) {
    return point
  }
  return null
}

function spanRectangleIntersections(line: CanvasSpan, rectangle: CanvasRectangle): CanvasPoint[] {
  const boundingLines = rectangleBoundingLines(rectangle)
  const intersectionPoints = mapDropNulls(
    (boundingLine) => spanIntersection(line, boundingLine),
    boundingLines,
  )
  return intersectionPoints
}

interface XMarkControlProps {
  point: CanvasPoint
  scale: number
}

const XMarkControlSize = 5

const XMarkControl = React.memo<XMarkControlProps>(({ point, scale }) => {
  const width = XMarkControlSize
  const height = XMarkControlSize

  return (
    <div
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
