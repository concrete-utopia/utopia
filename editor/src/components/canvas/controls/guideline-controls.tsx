import React from 'react'
import * as EP from '../../../core/shared/element-path'
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
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
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
    strategyIntendedBounds.map((_) => _.target),
  )

  const intersectionFrames = [
    ...strategyIntendedBounds.map((_) => _.frame),
    ...framesFromMetadata(metadata, parentAndSiblings),
  ]

  const intersectionPoints = intersectionFrames.flatMap((bound) =>
    snappingGuidelines
      .slice(0, 4)
      .flatMap(
        (guideLine) =>
          Utils.optionalMap(
            (line) => lineRectangleIntersections(line, bound),
            guidelineToLine(guideLine.guideline),
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
  metadata: ElementInstanceMetadataMap,
  paths: Array<ElementPath>,
): Array<CanvasRectangle> {
  return mapDropNulls((path) => metadata[EP.toString(path)]?.globalFrame ?? null, paths)
}

interface CanvasLine {
  a: CanvasPoint
  b: CanvasPoint
}

function canvasLine(a: CanvasPoint, b: CanvasPoint): CanvasLine {
  return { a, b }
}

function guidelineToLine(guideline: Guideline): CanvasLine | null {
  switch (guideline.type) {
    case 'XAxisGuideline':
      return {
        a: canvasPoint({ x: guideline.x, y: guideline.yBottom }),
        b: canvasPoint({ x: guideline.x, y: guideline.yTop }),
      }
    case 'YAxisGuideline':
      return {
        a: canvasPoint({ x: guideline.xLeft, y: guideline.y }),
        b: canvasPoint({ x: guideline.xRight, y: guideline.y }),
      }
    case 'CornerGuideline':
      return null
    default:
      const _: never = guideline
      throw new Error(`Unknown guideline found`)
  }
}

function rectangleBoundingLines(rectangle: CanvasRectangle): CanvasLine[] {
  return [
    canvasLine(
      canvasPoint({ x: rectangle.x, y: rectangle.y }),
      canvasPoint({ x: rectangle.x, y: rectangle.y + rectangle.height }),
    ),
    canvasLine(
      canvasPoint({ x: rectangle.x, y: rectangle.y }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y }),
    ),
    canvasLine(
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y + rectangle.height }),
    ),
    canvasLine(
      canvasPoint({ x: rectangle.x, y: rectangle.y + rectangle.height }),
      canvasPoint({ x: rectangle.x + rectangle.width, y: rectangle.y + rectangle.height }),
    ),
  ]
}

function lineRectangleIntersections(line: CanvasLine, rectangle: CanvasRectangle): CanvasPoint[] {
  const boundingLines = rectangleBoundingLines(rectangle)
  const intersectionPoints = mapDropNulls(
    (boundingLine) => lineIntersection(line.a, line.b, boundingLine.a, boundingLine.b),
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
