import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { useDistanceGuidelineX, useDistanceGuidelineY } from './distance-guideline-hooks'

interface DistanceGuidelineProps {
  localSelectedElements: Array<ElementPath>
}

export const DistanceGuideline = React.memo<DistanceGuidelineProps>((props) => {
  const colorTheme = useColorTheme()
  const isInteraction = useEditorState(
    (store) => store.editor.canvas.dragState != null,
    'DistanceGuideline isInteraction',
  )
  const isAltPressed = useEditorState(
    (store) => store.editor.keysPressed['alt'],
    'DistanceGuideline isAltPressed',
  )

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
          }}
        >
          <div ref={guidelineHorizontal}>
            <div
              style={{
                position: 'absolute',
                pointerEvents: 'none',
                // display: isAltPressed && !isInteraction ? 'block' : 'none',
                display: 'block',
                left: `calc(var(--utopia-canvas-guideline-x) + 0.5px / var(--utopia-canvas-scale))`,
                top: `calc(var(--utopia-canvas-guideline-y) + 0.5px / var(--utopia-canvas-scale))`,
                width: `calc(var(--utopia-canvas-guideline-width) - 0.5px / var(--utopia-canvas-scale) * 3)`,
              }}
            >
              <div
                style={{
                  position: 'relative',
                  display: 'inline-block',
                  top: -7 / 2,
                  height: 7,
                  width: `calc(1px / var(--utopia-canvas-scale))`,
                  backgroundColor: colorTheme.canvasLayoutStroke.value,
                }}
              />
              <div
                style={{
                  position: 'relative',
                  top: -7,
                  display: 'inline-block',
                  backgroundColor: colorTheme.canvasLayoutStroke.value,
                  width: '100%',
                  height: `calc(1px / var(--utopia-canvas-scale))`,
                }}
              />
              <div
                style={{
                  display: 'inline-block',
                  position: 'relative',
                  top: -7 / 2,
                  height: 7,
                  width: `calc(1px / var(--utopia-canvas-scale))`,
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
                fontSize: `calc(11px / var(--utopia-canvas-scale))`,
                color: colorTheme.canvasLayoutStroke.value,
                // display: isAltPressed && !isInteraction ? 'block' : 'none',
                display: 'block',
                left: `calc(var(--utopia-canvas-guideline-x) + 0.5px / var(--utopia-canvas-scale))`,
                top: `calc(var(--utopia-canvas-guideline-y) + 5.5px / var(--utopia-canvas-scale))`,
                width: `calc(var(--utopia-canvas-guideline-width) - 0.5px / var(--utopia-canvas-scale) * 3)`,
              }}
            />
          </div>
          <div
            ref={guidelineVertical}
            style={{
              position: 'absolute',
              pointerEvents: 'none',
              // display: isAltPressed && !isInteraction ? 'block' : 'none',
              display: 'block',
              left: `calc(var(--utopia-canvas-guideline-x) + 0.5px / var(--utopia-canvas-scale))`,
              top: `calc(var(--utopia-canvas-guideline-y) + 0.5px / var(--utopia-canvas-scale))`,
              height: `calc(var(--utopia-canvas-guideline-height) - 0.5px / var(--utopia-canvas-scale) * 3)`,
            }}
          >
            <div
              style={{
                position: 'relative',
                left: -7 / 2,
                width: 7,
                height: `calc(1px / var(--utopia-canvas-scale))`,
                backgroundColor: colorTheme.canvasLayoutStroke.value,
              }}
            />
            <div
              style={{
                position: 'relative',
                backgroundColor: colorTheme.canvasLayoutStroke.value,
                height: '100%',
                width: `calc(1px / var(--utopia-canvas-scale))`,
              }}
            />
            <div
              style={{
                position: 'relative',
                left: -7 / 2,
                width: 7,
                height: `calc(1px / var(--utopia-canvas-scale))`,
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
              fontSize: `calc(11px / var(--utopia-canvas-scale))`,
              color: colorTheme.canvasLayoutStroke.value,
              // display: isAltPressed && !isInteraction ? 'block' : 'none',
              display: 'block',
              left: `calc(var(--utopia-canvas-guideline-x) + 5.5px / var(--utopia-canvas-scale))`,
              top: `calc(var(--utopia-canvas-guideline-y) + 0.5px / var(--utopia-canvas-scale))`,
              height: `calc(var(--utopia-canvas-guideline-height) - 0.5px / var(--utopia-canvas-scale) * 3)`,
              lineHeight: `calc(var(--utopia-canvas-guideline-height) - 0.5px / var(--utopia-canvas-scale) * 3)`,
            }}
          />
        </div>
      </>
    )
  } else {
    return null
  }
})
