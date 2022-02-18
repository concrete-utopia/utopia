import React, { useEffect } from 'react'
import * as EP from '../../../../core/shared/element-path'
import {
  boundingRectangleArray,
  windowPoint,
  WindowRectangle,
} from '../../../../core/shared/math-utils'
import { fastForEach } from '../../../../core/shared/utils'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { windowToCanvasCoordinatesGlobal } from '../../dom-lookup'
import { getSelectionColor } from '../outline-control'

const OutlineOffset = 0.5
const OutlineWidthHeightOffset = -OutlineOffset * 3
export const OutlineControl = React.memo(() => {
  const colorTheme = useColorTheme()
  const selectedElements = useEditorState((store) => store.editor.selectedViews, 'OutlineControl')
  const outlineRef = React.useRef<HTMLDivElement>(null)
  const observerRef = React.useRef<MutationObserver | null>()
  const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)

  const colors = useEditorState((store) => {
    return store.editor.selectedViews.map((path) =>
      getSelectionColor(
        path,
        store.editor.jsxMetadata,
        store.editor.focusedElementPath,
        colorTheme,
      ),
    )
  }, 'OutlineControl colors')

  const observerCallback = React.useCallback(() => {
    const frames: Array<WindowRectangle> = []
    fastForEach(selectedElementsRef.current, (target) => {
      const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(target)}"]`)
      const frame = htmlElement?.getBoundingClientRect()
      if (frame != null) {
        frames.push({
          x: frame.x,
          y: frame.y,
          width: frame.width,
          height: frame.height,
        } as WindowRectangle)
      }
    })

    const boundingBox = boundingRectangleArray(frames)
    if (boundingBox != null && outlineRef.current != null) {
      const boundingBoxInCanvasCoords = windowToCanvasCoordinatesGlobal(
        windowPoint({ x: boundingBox.x, y: boundingBox.y }),
      ).canvasPositionRounded
      outlineRef.current.style.left = boundingBoxInCanvasCoords.x + OutlineOffset + 'px'
      outlineRef.current.style.top = boundingBoxInCanvasCoords.y + OutlineOffset + 'px'
      outlineRef.current.style.width = boundingBox.width + OutlineWidthHeightOffset + 'px'
      outlineRef.current.style.height = boundingBox.height + OutlineWidthHeightOffset + 'px'
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
    if (selectedElements.length > 0) {
      // TODO FIND SOMETHING NICER
      // this is a total hack that I found #node-connectors is still changing attributes on scroll. TODO use a HTMLElement that is deliberately for this
      const thisElementRerendersOnScroll = document.getElementById('node-connectors')
      if (thisElementRerendersOnScroll != null && observerRef.current != null) {
        observerRef.current.observe(thisElementRerendersOnScroll, {
          attributes: true,
        })
      }

      fastForEach(selectedElements, (path) => {
        const htmlElement = document.querySelector(`*[data-paths~="${EP.toString(path)}"]`)
        if (htmlElement != null && observerRef.current != null) {
          observerRef.current.observe(htmlElement, {
            attributes: true,
            childList: true,
            subtree: true,
          })
        }
      })

      observerCallback()
    }

    return function cleanup() {
      observerRef.current?.disconnect()
    }
  }, [selectedElements, observerCallback])

  if (selectedElements.length > 0) {
    return (
      <div
        ref={outlineRef}
        className='role-outline'
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          boxShadow: `0px 0px 0px 1px ${colors[0]}`,
          pointerEvents: 'none',
          transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
        }}
      />
    )
  }
  return null
})
