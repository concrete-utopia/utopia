import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasRectangle, negate, offsetRect, windowPoint } from '../../../core/shared/math-utils'
import { getCanvasRectangleFromElement } from '../../../core/shared/dom-utils'
import { CanvasScale } from '../../../utils/global-positions'
import { CanvasContainerID } from '../canvas-types'

function findTargetHtmlElement(path: ElementPath): HTMLElement | null {
  return document.querySelector(`*[data-paths~="${EP.toString(path)}"]`)
}

export function findFramesFromDOM(targets: Array<ElementPath>): Array<CanvasRectangle> {
  let frames: Array<CanvasRectangle> = []
  const canvasWrapper = document.getElementById(CanvasContainerID)

  let canvasWrapperRect: CanvasRectangle | null
  if (canvasWrapper != null) {
    canvasWrapperRect = getCanvasRectangleFromElement(canvasWrapper, CanvasScale.current)
  }
  fastForEach(targets, (path) => {
    const htmlElement = findTargetHtmlElement(path)
    if (htmlElement != null && canvasWrapperRect != null) {
      frames.push(globalFrameForElement(htmlElement, CanvasScale.current, canvasWrapperRect))
    }
  })

  return frames
}

function globalFrameForElement(
  element: HTMLElement,
  scale: number,
  containerRect: CanvasRectangle,
) {
  const elementRect = getCanvasRectangleFromElement(element, scale)
  return offsetRect(elementRect, negate(containerRect))
}

export function useMutationObserver(
  selectedElements: Array<ElementPath>,
  observerCallback: () => void,
) {
  const observerRef = React.useRef<MutationObserver | null>()
  React.useEffect(() => {
    const observer = new MutationObserver(observerCallback)
    observerRef.current = observer
    return function cleanup() {
      observer.disconnect()
      observerRef.current = null
    }
  }, [observerCallback])

  React.useEffect(() => {
    if (selectedElements.length > 0) {
      fastForEach(selectedElements, (path) => {
        const htmlElement = findTargetHtmlElement(path)
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
  return observerRef
}
