import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import * as EP from '../../../core/shared/element-path'
import {
  boundingRectangleArray,
  CanvasRectangle,
  negate,
  offsetRect,
  windowPoint,
} from '../../../core/shared/math-utils'
import { getCanvasRectangleFromElement } from '../../../core/shared/dom-utils'
import { CanvasScale } from '../../../utils/global-positions'
import { CanvasContainerID } from '../canvas-types'
import { useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getMetadata } from '../../editor/store/editor-state'

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
  observerCallback: (boundingRectangle: CanvasRectangle | null) => void,
): void {
  const observerRef = React.useRef<MutationObserver | null>()

  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))

  const observerCallbackRef = React.useRef(observerCallback)
  observerCallbackRef.current = observerCallback

  const innerCallback = React.useCallback(() => {
    // const frames: Array<CanvasRectangle> = findFramesFromDOM(selectedElementsRef.current)

    let frames: Array<CanvasRectangle> = []

    fastForEach(selectedElements, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, metadataRef.current)
      if (frame != null) {
        frames.push(frame)
      }
    })

    const boundingBox = boundingRectangleArray(frames)

    observerCallbackRef.current(boundingBox)
  }, [selectedElements, metadataRef])

  React.useEffect(() => {
    const observer = new MutationObserver(innerCallback)
    observerRef.current = observer
    return function cleanup() {
      observer.disconnect()
      observerRef.current = null
    }
  }, [innerCallback])

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

      innerCallback()
    }

    return function cleanup() {
      observerRef.current?.disconnect()
    }
  }, [selectedElements, innerCallback])
}
