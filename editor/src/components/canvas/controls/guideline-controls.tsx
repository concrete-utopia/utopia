import React from 'react'
import { canvasRectangle, CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import {
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

interface GuidelineControlProps {
  localSelectedElements: Array<ElementPath>
}

// STRATEGY GUIDELINE CONTROLS
export const GuidelineControls = React.memo<GuidelineControlProps>((props) => {
  return (
    <CanvasOffsetWrapper>
      <Guideline index={0} />
      <Guideline index={1} />
      <Guideline index={2} />
      <Guideline index={3} />
    </CanvasOffsetWrapper>
  )
})

interface GuidelineProps {
  index: number
}

const LineWidth = 1
const Guideline = React.memo<GuidelineProps>((props) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'Guideline scale')
  const controlRef = useGuideline(props.index, (guidelineFrame: CanvasRectangle | null) => {
    if (controlRef.current != null) {
      if (guidelineFrame == null) {
        controlRef.current.style.setProperty('display', 'none')
      } else {
        const width = guidelineFrame.width === 0 ? LineWidth / scale : guidelineFrame.width
        const height = guidelineFrame.height === 0 ? LineWidth / scale : guidelineFrame.height
        controlRef.current.style.setProperty('display', 'block')
        controlRef.current.style.setProperty('left', `${guidelineFrame.x}px`)
        controlRef.current.style.setProperty('top', `${guidelineFrame.y}px`)
        controlRef.current.style.setProperty('width', `${width}px`)
        controlRef.current.style.setProperty('height', `${height}px`)
      }
    }
  })
  return (
    <div
      ref={controlRef}
      style={{
        position: 'absolute',
        backgroundColor: colorTheme.canvasLayoutStroke.value,
      }}
    ></div>
  )
})

function useGuideline<T = HTMLDivElement>(
  index: number,
  onChangeCallback: (frame: CanvasRectangle | null) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)

  const guidelineCallback = React.useCallback(
    (guidelineFrame: CanvasRectangle | null) => {
      if (controlRef.current != null) {
        onChangeCallback(guidelineFrame)
      }
    },
    [onChangeCallback],
  )

  const guidelineCallbackRef = React.useRef(guidelineCallback)
  guidelineCallbackRef.current = guidelineCallback

  const guidelineRef = useRefEditorState(
    (store) => store.editor.canvas.controls.snappingGuidelines[index],
  )

  const innerCallback = React.useCallback(() => {
    if (guidelineRef.current != null) {
      switch (guidelineRef.current.type) {
        case 'XAxisGuideline': {
          const frame = canvasRectangle({
            x: guidelineRef.current.x,
            y: guidelineRef.current.yTop,
            width: 0,
            height: guidelineRef.current.yBottom,
          })
          guidelineCallbackRef.current(frame)
          break
        }
        case 'YAxisGuideline': {
          const frame = canvasRectangle({
            x: guidelineRef.current.xLeft,
            y: guidelineRef.current.y,
            width: guidelineRef.current.xRight,
            height: 0,
          })
          guidelineCallbackRef.current(frame)
          break
        }
        case 'CornerGuideline':
        default:
          break
      }
    } else {
      guidelineCallbackRef.current(null)
    }
  }, [guidelineRef, guidelineCallbackRef])
  useSelectorWithCallback(
    (store) => store.editor.canvas.controls.snappingGuidelines[index],
    innerCallback,
  )
  return controlRef
}
