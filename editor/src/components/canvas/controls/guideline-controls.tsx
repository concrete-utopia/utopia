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

// STRATEGY GUIDELINE CONTROLS
export const GuidelineControls = React.memo(() => {
  return (
    <CanvasOffsetWrapper>
      <GuidelineControl index={0} />
      <GuidelineControl index={1} />
      <GuidelineControl index={2} />
      <GuidelineControl index={3} />
    </CanvasOffsetWrapper>
  )
})

interface GuidelineProps {
  index: number
}

const LineWidth = 1
const GuidelineControl = React.memo<GuidelineProps>((props) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'Guideline scale')
  const controlRef = useGuideline(
    props.index,
    (result: { frame: CanvasRectangle; activateSnap: boolean } | null) => {
      if (controlRef.current != null) {
        if (result == null) {
          controlRef.current.style.setProperty('display', 'none')
        } else {
          controlRef.current.style.setProperty('display', 'block')
          controlRef.current.style.setProperty('left', `${result.frame.x}px`)
          controlRef.current.style.setProperty('top', `${result.frame.y}px`)
          controlRef.current.style.setProperty('width', `${result.frame.width}px`)
          controlRef.current.style.setProperty('height', `${result.frame.height}px`)
          controlRef.current.style.setProperty(
            'border-style',
            `${result.activateSnap ? 'solid' : 'dashed'}`,
          )
        }
      }
    },
  )
  return (
    <div
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
  onChangeCallback: (result: { frame: CanvasRectangle; activateSnap: boolean } | null) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)

  const guidelineCallback = React.useCallback(
    (result: { frame: CanvasRectangle; activateSnap: boolean } | null) => {
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
            activateSnap: guidelineWithSnapping.activateSnap,
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
            activateSnap: guidelineWithSnapping.activateSnap,
          })
          break
        }
        case 'CornerGuideline':
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
  return controlRef
}
