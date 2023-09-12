import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import type { CanvasRectangle, WindowPoint, WindowRectangle } from '../../../core/shared/math-utils'
import {
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
  windowPoint,
  windowRectangle,
} from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import {
  CanvasToolbarId,
  ToolbarIndicatorElementsOutsideVisibleAreaId,
} from '../../editor/canvas-toolbar'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

export type ElementOutsideVisibleAreaDirection = 'top' | 'left' | 'bottom' | 'right'

type ElementOutsideVisibleArea = {
  path: ElementPath
  rect: WindowRectangle
}

export type ElementOutsideVisibleAreaIndicator = {
  path: ElementPath
  angle: number
}

export function useElementsOutsideVisibleArea(
  localHighlightedViews: ElementPath[],
  localSelectedViews: ElementPath[],
): ElementOutsideVisibleAreaIndicator[] {
  const bounds = document.getElementById('canvas-root')?.getBoundingClientRect()
  const canvasToolbar = document.getElementById(CanvasToolbarId)?.getBoundingClientRect() ?? null

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
    return uniqBy([...localSelectedViews, ...localHighlightedViews], EP.pathsEqual)
  }, [localSelectedViews, localHighlightedViews])

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
    if (bounds == null) {
      return null
    }
    const scaleRatio = canvasScale > 1 ? canvasScale : 1
    return windowRectangle({
      x: bounds.x * scaleRatio,
      y: bounds.y * scaleRatio,
      width: bounds.width * scaleRatio,
      height: bounds.height * scaleRatio,
    })
  }, [bounds, canvasScale])

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

  const getOriginPoint = React.useCallback((): WindowPoint => {
    const fromRect = document
      .getElementById(ToolbarIndicatorElementsOutsideVisibleAreaId)
      ?.getBoundingClientRect()
    return fromRect != null
      ? windowPoint({
          x: (fromRect.x + fromRect.width / 2) * canvasScale,
          y: (fromRect.y + fromRect.height / 2) * canvasScale,
        })
      : windowPoint({ x: 0, y: 0 })
  }, [canvasScale])

  return React.useMemo((): ElementOutsideVisibleAreaIndicator[] => {
    if (scaledCanvasArea == null || bounds == null || canvasToolbar == null) {
      return []
    }
    const origin = getOriginPoint()
    const indicators: ElementOutsideVisibleAreaIndicator[] = []
    for (const { rect, path } of elementsOutsideVisibleArea) {
      // Map element to indicator
      const indicator: ElementOutsideVisibleAreaIndicator = {
        path: path,
        angle: angleBetweenPoints(origin, getRectCenter(rect)),
      }
      indicators.push(indicator)
    }
    return indicators
  }, [elementsOutsideVisibleArea, scaledCanvasArea, bounds, canvasToolbar, getOriginPoint])
}

function angleBetweenPoints(from: WindowPoint, to: WindowPoint): number {
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
