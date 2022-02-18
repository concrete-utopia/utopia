import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasRectangle, windowPoint } from '../../../core/shared/math-utils'
import { windowToCanvasCoordinatesGlobal } from '../dom-lookup'

function findTargetHtmlElement(path: ElementPath): HTMLElement | null {
  return document.querySelector(`*[data-paths~="${EP.toString(path)}"]`)
}

export function findFramesFromDOM(targets: Array<ElementPath>): Array<CanvasRectangle> {
  const frames: Array<CanvasRectangle> = []
  fastForEach(targets, (path) => {
    const htmlElement = findTargetHtmlElement(path)
    const frame = htmlElement?.getBoundingClientRect()
    if (frame != null) {
      const frameInCanvasCoords = windowToCanvasCoordinatesGlobal(
        windowPoint({ x: frame.x, y: frame.y }),
      ).canvasPositionRounded
      frames.push({
        x: frameInCanvasCoords.x,
        y: frameInCanvasCoords.y,
        width: frame.width,
        height: frame.height,
      } as CanvasRectangle)
    }
  })

  return frames
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
