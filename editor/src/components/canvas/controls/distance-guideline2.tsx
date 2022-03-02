import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { useDistanceGuidelineX, useDistanceGuidelineY } from './distance-guideline-hooks'

interface DistanceGuidelineProps {
  localSelectedElements: Array<ElementPath>
}

const FontSize = 11
const LineEndSegmentSize = 3.5
const StrokeWidth = 1
export const DistanceGuideline = React.memo<DistanceGuidelineProps>((props) => {
  const isInteraction = useEditorState(
    (store) =>
      store.editor.canvas.dragState != null || store.editor.canvas.interactionSession != null,
    'DistanceGuideline isInteraction',
  )
  const isAltPressed = useEditorState(
    (store) => store.editor.keysPressed['alt'],
    'DistanceGuideline isAltPressed',
  )

  if (props.localSelectedElements.length > 0) {
    return (
      <>
        <style>{`
          .utopia-canvas-guideline: {
            --utopia-canvas-guideline-x: 0px;
            --utopia-canvas-guideline-y: 0px;
            --utopia-canvas-guideline-width: 0px;
            --utopia-canvas-guideline-height: 0px;
          }
        `}</style>
        <div
          className='role-distance-guideline'
          style={{
            position: 'absolute',
            transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
            // display: isAltPressed && !isInteraction ? 'block' : 'none',
            display: 'block',
          }}
        >
          <GuidelineHorizontal localSelectedElements={props.localSelectedElements} />
          <GuidelineVertical localSelectedElements={props.localSelectedElements} />
        </div>
      </>
    )
  } else {
    return null
  }
})

const GuidelineHorizontal = React.memo<DistanceGuidelineProps>((props) => {
  const colorTheme = useColorTheme()
  const guidelineHorizontal = useDistanceGuidelineX(
    props.localSelectedElements,
    (ref, guidelineRect) => {
      ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-width`, `${guidelineRect.width}px`)
    },
  )
  const guidelineHorizontalText = useDistanceGuidelineX(
    props.localSelectedElements,
    (ref, guidelineRect, distance) => {
      ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-width`, `${guidelineRect.width}px`)
      ref.current.innerHTML = `${distance}`
    },
  )

  return (
    <>
      <div
        ref={guidelineHorizontal}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          left: `calc(var(--utopia-canvas-guideline-x) + ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale))`,
          top: `calc(var(--utopia-canvas-guideline-y) + ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale))`,
          width: `calc(var(--utopia-canvas-guideline-width) - ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale) * 3)`,
        }}
      >
        <div
          style={{
            position: 'relative',
            display: 'inline-block',
            top: `${-LineEndSegmentSize}px / var(--utopia-canvas-scale))`,
            height: `${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale))`,
            width: `calc(${StrokeWidth}px / var(--utopia-canvas-scale))`,
            backgroundColor: colorTheme.canvasLayoutStroke.value,
          }}
        />
        <div
          style={{
            position: 'relative',
            top: `${-LineEndSegmentSize * 2}px / var(--utopia-canvas-scale))`,
            display: 'inline-block',
            backgroundColor: colorTheme.canvasLayoutStroke.value,
            width: '100%',
            height: `calc(${StrokeWidth}px / var(--utopia-canvas-scale))`,
          }}
        />
        <div
          style={{
            display: 'inline-block',
            position: 'relative',
            top: `${-LineEndSegmentSize}px / var(--utopia-canvas-scale))`,
            height: `${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale))`,
            width: `calc(${StrokeWidth}px / var(--utopia-canvas-scale))`,
            backgroundColor: colorTheme.canvasLayoutStroke.value,
          }}
        />
      </div>
      <div
        ref={guidelineHorizontalText}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          textAlign: 'center',
          fontSize: `calc(${FontSize}px / var(--utopia-canvas-scale))`,
          color: colorTheme.canvasLayoutStroke.value,
          left: `calc(var(--utopia-canvas-guideline-x) + ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale))`,
          top: `calc(var(--utopia-canvas-guideline-y) + ${
            FontSize / 2
          }px / var(--utopia-canvas-scale))`,
          width: `calc(var(--utopia-canvas-guideline-width) -${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale) * 3)`,
        }}
      />
    </>
  )
})

const GuidelineVertical = React.memo<DistanceGuidelineProps>((props) => {
  const colorTheme = useColorTheme()

  const guidelineVertical = useDistanceGuidelineY(
    props.localSelectedElements,
    (ref, guidelineRect) => {
      ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-height`, `${guidelineRect.height}px`)
    },
  )
  const guidelineVerticalText = useDistanceGuidelineY(
    props.localSelectedElements,
    (ref, guidelineRect, distance) => {
      ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
      ref.current.style.setProperty(`--utopia-canvas-guideline-height`, `${guidelineRect.height}px`)
      ref.current.innerHTML = `${distance}`
    },
  )

  return (
    <>
      <div
        ref={guidelineVertical}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          left: `calc(var(--utopia-canvas-guideline-x) + ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale))`,
          top: `calc(var(--utopia-canvas-guideline-y) + ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale))`,
          height: `calc(var(--utopia-canvas-guideline-height) - ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale) * 3)`,
        }}
      >
        <div
          style={{
            position: 'relative',
            left: `${-LineEndSegmentSize}px / var(--utopia-canvas-scale))`,
            width: `${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale))`,
            height: `calc(${StrokeWidth}px / var(--utopia-canvas-scale))`,
            backgroundColor: colorTheme.canvasLayoutStroke.value,
          }}
        />
        <div
          style={{
            position: 'relative',
            backgroundColor: colorTheme.canvasLayoutStroke.value,
            height: '100%',
            width: `calc(${StrokeWidth}px / var(--utopia-canvas-scale))`,
          }}
        />
        <div
          style={{
            position: 'relative',
            left: `${-LineEndSegmentSize}px / var(--utopia-canvas-scale))`,
            width: `${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale))`,
            height: `calc(${StrokeWidth}px / var(--utopia-canvas-scale))`,
            backgroundColor: colorTheme.canvasLayoutStroke.value,
          }}
        />
      </div>
      <div
        ref={guidelineVerticalText}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          textAlign: 'center',
          fontSize: `calc(${FontSize}px / var(--utopia-canvas-scale))`,
          color: colorTheme.canvasLayoutStroke.value,
          left: `calc(var(--utopia-canvas-guideline-x) + ${
            FontSize / 2
          }px / var(--utopia-canvas-scale))`,
          top: `calc(var(--utopia-canvas-guideline-y) + ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale))`,
          height: `calc(var(--utopia-canvas-guideline-height) - ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale) * 3)`,
          lineHeight: `calc(var(--utopia-canvas-guideline-height) - ${
            StrokeWidth / 2
          }px / var(--utopia-canvas-scale) * 3)`,
        }}
      />
    </>
  )
})
