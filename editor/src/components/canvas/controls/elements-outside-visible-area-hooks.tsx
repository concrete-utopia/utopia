import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import type { CanvasRectangle, WindowPoint, WindowRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
  scaleRect,
  windowPoint,
  windowRectangle,
} from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

export type ElementOutsideVisibleAreaDirection = 'top' | 'left' | 'bottom' | 'right'

type ElementOutsideVisibleArea = {
  path: ElementPath
  rect: WindowRectangle
}

export type ElementOutsideVisibleAreaIndicator = {
  elements: number
  rect: CanvasRectangle
  position: WindowPoint
  selected: boolean
}

export function useElementsOutsideVisibleArea(): ElementOutsideVisibleAreaIndicator | null {
  const canvasBounds = document.getElementById('canvas-root')?.getBoundingClientRect()

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'useElementsOutsideVisibleArea selectedViews',
  )

  const highlightedViews = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.highlightedViews,
    'useElementsOutsideVisibleArea highlightedViews',
  )

  const storeRef = useRefEditorState((store) => ({
    jsxMetadata: store.editor.jsxMetadata,
  }))

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useElementsOutsideVisibleArea canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'useElementsOutsideVisibleArea canvasOffset',
  )

  const elements = React.useMemo(() => {
    return uniqBy([...selectedViews, ...highlightedViews], EP.pathsEqual)
  }, [selectedViews, highlightedViews])

  const framesByPathString = React.useMemo(() => {
    const frames: { [key: string]: CanvasRectangle } = {}
    for (const path of elements) {
      const metadata = MetadataUtils.findElementByElementPath(storeRef.current.jsxMetadata, path)
      if (
        metadata != null &&
        metadata.globalFrame != null &&
        isFiniteRectangle(metadata.globalFrame)
      ) {
        frames[EP.toString(path)] = metadata.globalFrame
      }
    }
    return frames
  }, [storeRef, elements])

  const scaledCanvasArea = React.useMemo(() => {
    if (canvasBounds == null) {
      return null
    }
    const scaleRatio = canvasScale > 1 ? canvasScale : 1
    return windowRectangle({
      x: canvasBounds.x * scaleRatio,
      y: canvasBounds.y * scaleRatio,
      width: canvasBounds.width * scaleRatio,
      height: canvasBounds.height * scaleRatio,
    })
  }, [canvasBounds, canvasScale])

  const elementsOutsideVisibleArea = React.useMemo(() => {
    return mapDropNulls((path: ElementPath): ElementOutsideVisibleArea | null => {
      if (scaledCanvasArea == null) {
        return null
      }
      const frame = framesByPathString[EP.toString(path)]
      if (frame == null) {
        return null
      }

      const topLeftSkew = windowPoint({ x: 0, y: 0 })
      const topLeftPoint = offsetPoint(
        canvasPointToWindowPoint(frame, canvasScale, canvasOffset),
        topLeftSkew,
      )
      const elementRect = windowRectangle({
        x: topLeftPoint.x,
        y: topLeftPoint.y,
        width: frame.width * canvasScale,
        height: frame.height * canvasScale,
      })

      const directions = getOutsideDirections(scaledCanvasArea, elementRect)
      if (directions.length === 0) {
        return null
      }

      return {
        path: path,
        rect: elementRect,
      }
    }, elements)
  }, [elements, canvasOffset, canvasScale, scaledCanvasArea, framesByPathString])

  return React.useMemo((): ElementOutsideVisibleAreaIndicator | null => {
    if (elementsOutsideVisibleArea.length === 0) {
      return null
    }
    const windowRect = boundingRectangleArray(elementsOutsideVisibleArea.map((a) => a.rect))
    if (windowRect == null) {
      return null
    }
    const canvasRect = boundingRectangleArray(
      elementsOutsideVisibleArea.map((a) => framesByPathString[EP.toString(a.path)]),
    )
    if (canvasRect == null) {
      return null
    }
    return {
      elements: elementsOutsideVisibleArea.length,
      rect: canvasRect,
      position: getRectCenter(scaleRect(windowRect, canvasScale)),
      selected: elementsOutsideVisibleArea.some((e) => EP.containsPath(e.path, selectedViews)),
    }
  }, [elementsOutsideVisibleArea, framesByPathString, selectedViews, canvasScale])
}

export function getIndicatorAngleToTarget(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}

function getOutsideDirections(
  container: WindowRectangle,
  rect: WindowRectangle,
): ElementOutsideVisibleAreaDirection[] {
  if (container == null) {
    return []
  }
  const directions: ElementOutsideVisibleAreaDirection[] = []
  // Directions will be sorted as [top | bottom], [left | right]

  if (rect.y + rect.height < container.y) {
    directions.push('top')
  }
  if (rect.y > container.y + container.height) {
    directions.push('bottom')
  }
  if (rect.x + rect.width < container.x) {
    directions.push('left')
  }
  if (rect.x > container.x + container.width) {
    directions.push('right')
  }
  return directions
}
