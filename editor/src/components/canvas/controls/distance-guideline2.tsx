import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { useBoundingBox, useDistanceGuidelineX, useDistanceGuidelineY } from './bounding-box-hooks'

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
    (ref, boundingBox, distance) => {
      ref.current.style.left = `calc(${boundingBox.x}px + 0.5px / var(--utopia-canvas-scale))`
      ref.current.style.top = `calc(${boundingBox.y}px + 0.5px / var(--utopia-canvas-scale))`
      ref.current.style.width = `calc(${boundingBox.width}px - 0.5px / var(--utopia-canvas-scale) * 3)`
    },
  )
  const guidelineHorizontalText = useDistanceGuidelineX(
    props.localSelectedElements,
    (ref, boundingBox, distance) => {
      ref.current.style.left = `calc(${boundingBox.x}px + 0.5px / var(--utopia-canvas-scale))`
      ref.current.style.top = `calc(${boundingBox.y}px + 11px / var(--utopia-canvas-scale))`
      ref.current.style.width = `calc(${boundingBox.width}px - 0.5px / var(--utopia-canvas-scale) * 3)`
      ref.current.innerHTML = `${distance}`
    },
  )
  const guidelineVertical = useDistanceGuidelineY(
    props.localSelectedElements,
    (ref, boundingBox) => {
      ref.current.style.left = `calc(${boundingBox.x}px + 0.5px / var(--utopia-canvas-scale))`
      ref.current.style.top = `calc(${boundingBox.y}px + 0.5px / var(--utopia-canvas-scale))`
      ref.current.style.height = `calc(${boundingBox.height}px - 0.5px / var(--utopia-canvas-scale) * 3)`
    },
  )
  const guidelineVerticalText = useDistanceGuidelineY(
    props.localSelectedElements,
    (ref, boundingBox, distance) => {
      ref.current.style.left = `calc(${boundingBox.x}px + 11px / var(--utopia-canvas-scale))`
      ref.current.style.top = `calc(${boundingBox.y}px + 0.5px / var(--utopia-canvas-scale))`
      ref.current.style.height = `calc(${boundingBox.height}px - 0.5px / var(--utopia-canvas-scale) * 3)`
      ref.current.style.lineHeight = `calc(${boundingBox.height}px - 0.5px / var(--utopia-canvas-scale) * 3)`
      ref.current.innerHTML = `${distance}`
    },
  )

  if (props.localSelectedElements.length > 0) {
    return (
      <div
        className='role-distance-guideline'
        style={{
          position: 'absolute',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      >
        <div
          ref={guidelineHorizontal}
          style={{
            position: 'absolute',
            pointerEvents: 'none',
            // display: isAltPressed && !isInteraction ? 'block' : 'none',
            display: 'block',
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
          }}
        />
        <div
          ref={guidelineVertical}
          style={{
            position: 'absolute',
            pointerEvents: 'none',
            // display: isAltPressed && !isInteraction ? 'block' : 'none',
            display: 'block',
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
          }}
        />
      </div>
    )
  } else {
    return null
  }
})
