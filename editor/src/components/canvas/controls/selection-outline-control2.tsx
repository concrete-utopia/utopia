import React, { useEffect } from 'react'
import * as EP from '../../../core/shared/element-path'
import { windowPoint } from '../../../core/shared/math-utils'
import {
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { windowToCanvasCoordinatesGlobal } from '../dom-lookup'

export const SelectionOutlineControl2 = React.memo(() => {
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'SelectionOutlineControl2',
  )
  const controlRef = React.useRef<HTMLDivElement>(null)
  const observerRef = React.useRef<MutationObserver | null>()
  const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)

  const observerCallback = React.useCallback(() => {
    if (selectedElementsRef.current.length === 1) {
      const target = selectedElementsRef.current[0]
      const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(target)}"]`)
      const frame = htmlElement?.getBoundingClientRect()
      if (frame != null && controlRef.current != null) {
        const frameInCanvasCoords = windowToCanvasCoordinatesGlobal(
          windowPoint({ x: frame.left, y: frame.top }),
        ).canvasPositionRounded
        controlRef.current.style.left = frameInCanvasCoords.x + 'px'
        controlRef.current.style.top = frameInCanvasCoords.y + 'px'
        controlRef.current.style.width = frame.width + 'px'
        controlRef.current.style.height = frame.height + 'px'
      }
    }
  }, [selectedElementsRef])

  useEffect(() => {
    const observer = new MutationObserver(observerCallback)
    observerRef.current = observer
    return function cleanup() {
      observer.disconnect()
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    if (selectedElements.length === 1) {
      // this is a total hack that I found #node-connectors is still changing attributes on scroll. TODO use a HTMLElement that is deliberately for this
      const thisElementRerendersOnScroll = document.getElementById('node-connectors')
      const htmlElement = document.querySelector(
        `*[data-paths~="${EP.toString(selectedElements[0])}"]`,
      )
      if (thisElementRerendersOnScroll != null && observerRef.current != null) {
        observerRef.current.observe(thisElementRerendersOnScroll, {
          attributes: true,
        })
      }
      if (htmlElement != null && observerRef.current != null) {
        observerRef.current.observe(htmlElement, {
          attributes: true,
          childList: true,
          subtree: true,
        })
      }

      observerCallback()
    }

    return function cleanup() {
      observerRef.current?.disconnect()
    }
  }, [selectedElements, observerCallback])

  if (selectedElements.length === 1) {
    return (
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
          backgroundColor: 'hotpink',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      ></div>
    )
  }
  return null
})
