import React, { useEffect } from 'react'
import * as EP from '../../../core/shared/element-path'
import { windowPoint } from '../../../core/shared/math-utils'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { removeCanvasOffset, windowToCanvasCoordinates } from '../dom-lookup'

export const SelectionOutlineControl2 = React.memo(() => {
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'SelectionOutlineControl2',
  )
  const controlRef = React.useRef<HTMLDivElement>(null)
  const observerRef = React.useRef<MutationObserver | null>()
  const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)
  useEffect(() => {
    const observerCallback = () => {
      if (selectedElementsRef.current.length === 1) {
        const target = selectedElementsRef.current[0]
        const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(target)}"]`)
        const frame = htmlElement?.getBoundingClientRect()
        if (frame != null && controlRef.current != null) {
          const frameInCanvasCoords = removeCanvasOffset(windowPoint({ x: frame.x, y: frame.y }))
          controlRef.current.style.left = frameInCanvasCoords.x + 'px'
          controlRef.current.style.top = frameInCanvasCoords.y + 'px'
          controlRef.current.style.width = frame.width + 'px'
          controlRef.current.style.height = frame.height + 'px'
        }
      }
    }
    const observer = new MutationObserver(observerCallback)
    observerRef.current = observer
    return function cleanup() {
      observer.disconnect()
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    if (selectedElements.length === 1) {
      const canvasWrapper = document.getElementsByClassName('utopia-css-var-container')[0]
      const htmlElement = document.querySelector(
        `*[data-paths~="${EP.toString(selectedElements[0])}"]`,
      )
      if (canvasWrapper != null && observerRef.current != null) {
        observerRef.current.observe(canvasWrapper, {
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
    }
    return function cleanup() {
      observerRef.current?.disconnect()
    }
  }, [selectedElements])

  if (selectedElements.length === 1) {
    return (
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
          backgroundColor: 'hotpink',
        }}
      ></div>
    )
  }
  return null
})
