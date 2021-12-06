import React from 'react'
import styled from '@emotion/styled'
import { CanvasVector } from '../../core/shared/math-utils'
import { betterReactMemo } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import CanvasActions from './canvas-actions'
import { CanvasComponentEntry } from './canvas-component-entry'
import { CanvasControlsLegacyLayer } from './controls/new-canvas-controls'

function useScroll(ref: React.RefObject<HTMLDivElement>): void {
  const dispatch = useEditorState((store) => store.dispatch, 'useScroll dispatch')
  React.useEffect(() => {
    function onWheel(this: HTMLDivElement, event: WheelEvent) {
      event.stopPropagation()
      event.stopImmediatePropagation()
      event.preventDefault()
      dispatch([CanvasActions.scrollCanvas({ x: event.deltaX, y: event.deltaY } as CanvasVector)])
    }

    ref.current?.addEventListener('wheel', onWheel)

    return function cleanup() {
      ref.current?.removeEventListener('wheel', onWheel)
    }
  }, [dispatch])
}

const CanvasRootDiv = styled.div({
  flex: 1,
  overflow: 'hidden',
})

export const InteractiveCanvasRoot = betterReactMemo('InteractiveCanvasRoot', () => {
  const canvasRootRef = React.useRef<HTMLDivElement>(null)
  useScroll(canvasRootRef)

  return (
    <CanvasRootDiv id='canvas-root' className='interactive-canvas-root' ref={canvasRootRef}>
      <CanvasComponentEntry />
      {/* TODO: remove the legacy layer from here */}
      <CanvasControlsLegacyLayer />
    </CanvasRootDiv>
  )
})
