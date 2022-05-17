import React from 'react'
import { CanvasVector, offsetPoint } from '../../../../core/shared/math-utils'
import {
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const AnimationTimer = 2000
const DelayTimer = 200

const AnimationStyle = `
:root {
  --animation-duration: ${(AnimationTimer - DelayTimer) / 1000}s;
  --size: 15px;
  --background-color: black;
  --animation-iteration-count: 1;
}

.wrapper {
  width: var(--size);
  height: var(--size);
  position: relative;
  background: white;
  border-radius: 50%;
  border: 1px solid black;
}

.pie {
  width: 50%;
  height: 100%;
  transform-origin: 100% 50%;
  position: absolute;
  background: var(--background-color);
}

.spinner {
  border-radius: 100% 0 0 100% / 50% 0 0 50%;
  z-index: 200;
  border-right: none;
  animation: rota var(--animation-duration) linear var(--animation-iteration-count);
  animation-fill-mode: forwards;
}

.filler {
  border-radius: 0 30px 30px 0;
  background-color: var(--background-color);
  left: 50%;
  opacity: 0;
  z-index: 100;
  animation: fill var(--animation-duration) steps(1, end)
    var(--animation-iteration-count);
  border-left: none;
}

.mask {
  width: 50%;
  height: 100%;
  position: absolute;
  background: inherit;
  opacity: 1;
  z-index: 300;
  animation: mask var(--animation-duration) steps(1, end)
    var(--animation-iteration-count);

  border-radius: 30px 0 0 30px;
}

@keyframes rota {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}

@keyframes mask {
  0% {
    opacity: 1;
  }
  50%,
  100% {
    opacity: 0;
  }
}

@keyframes fill {
  0% {
    opacity: 0;
  }
  50%,
  100% {
    opacity: 1;
  }
}
`

const PieTimer = React.memo(() => {
  return (
    <>
      <style>{AnimationStyle}</style>
      <div className='wrapper'>
        <div className='pie spinner'></div>
        <div className='pie filler'></div>
        <div className='mask'></div>
      </div>
    </>
  )
})

export const PieTimerControl = React.memo(() => {
  const elementRef = useApplyCursorPositionToStyle()
  const showControl = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return (
        store.editor.canvas.interactionSession.interactionData.globalTime -
          store.editor.canvas.interactionSession.lastInteractionTime >
        DelayTimer
      )
    } else {
      return false
    }
  }, 'FlowMoveControlTimer showControl')

  return (
    <CanvasOffsetWrapper>
      <div
        ref={elementRef}
        style={{
          position: 'absolute',
        }}
      >
        {showControl ? <PieTimer /> : null}
      </div>
    </CanvasOffsetWrapper>
  )
})

const CursorCompanionOffset = { x: 20, y: 0 }
export function useApplyCursorPositionToStyle(): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const applyCursorPosition = React.useCallback(
    (cursorPosition: CanvasVector | null) => {
      if (elementRef.current != null) {
        if (cursorPosition != null) {
          elementRef.current.style.setProperty(
            'left',
            `${cursorPosition.x + CursorCompanionOffset.x}px`,
          )
          elementRef.current.style.setProperty(
            'top',
            `${cursorPosition.y + CursorCompanionOffset.y}px`,
          )
          elementRef.current.style.setProperty('display', 'block')
        } else {
          elementRef.current.style.setProperty('display', 'none')
        }
      }
    },
    [elementRef],
  )

  useSelectorWithCallback((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      if (store.editor.canvas.interactionSession.interactionData.drag != null) {
        return offsetPoint(
          store.editor.canvas.interactionSession.interactionData.dragStart,
          store.editor.canvas.interactionSession.interactionData.drag,
        )
      } else {
        return store.editor.canvas.interactionSession.interactionData.dragStart
      }
    } else {
      return null
    }
  }, applyCursorPosition)

  const applyCursorPositionEffect = React.useCallback(() => {
    applyCursorPosition(null)
  }, [applyCursorPosition])
  React.useLayoutEffect(applyCursorPositionEffect, [applyCursorPositionEffect])

  return elementRef
}
