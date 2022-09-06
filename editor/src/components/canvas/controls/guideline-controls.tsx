import React from 'react'
import { mapDropNulls } from 'src/core/shared/array-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  CanvasVector,
  CoordinateMarker,
  lineIntersection,
  Point,
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

// STRATEGY GUIDELINE CONTROLS
export const GuidelineControls = React.memo(() => {
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

  if (!strategyMovedSuccessfully) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <GuidelineControl index={0} />
        <GuidelineControl index={1} />
        <GuidelineControl index={2} />
        <GuidelineControl index={3} />
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

interface CanvasLine {
  a: CanvasPoint
  b: CanvasPoint
}

function guidelineToLinee(guideline: Guideline): CanvasLine | null {
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
    // top
    // right
    // left
    // bottom
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
