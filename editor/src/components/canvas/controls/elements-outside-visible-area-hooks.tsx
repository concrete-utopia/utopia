import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { CanvasRectangle, WindowPoint, WindowRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  getRectCenter,
  rectangleIntersection,
  windowRectangle,
} from '../../../core/shared/math-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

export type ElementOutsideVisibleAreaIndicator = {
  position: WindowPoint
}

export function useElementsOutsideVisibleArea(): ElementOutsideVisibleAreaIndicator | null {
  const canvasBounds = getCanvasBounds()

  const selectedElementsFrames: CanvasRectangle[] = useEditorState(
    Substores.metadata,
    (store) => {
      return mapDropNulls(
        (view) => MetadataUtils.getFrameOrZeroRectInCanvasCoords(view, store.editor.jsxMetadata),
        store.editor.selectedViews,
      )
    },
    'useElementsOutsideVisibleArea selectedElementsFrames',
  )

  const isOutsideVisibleArea = useGetFrameWhenOutsideBounds(canvasBounds)

  return React.useMemo(() => {
    const framesOutsideVisibleArea = mapDropNulls(isOutsideVisibleArea, selectedElementsFrames)
    const windowRect = boundingRectangleArray(framesOutsideVisibleArea)
    if (windowRect == null) {
      return null
    }

    return {
      position: getRectCenter(windowRect),
    }
  }, [selectedElementsFrames, isOutsideVisibleArea])
}

export function getIndicatorAngleToTarget(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}

export function useGetFrameWhenOutsideBounds(outerBounds: WindowRectangle | null) {
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useIsOutsideVisibleArea canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'useIsOutsideVisibleArea canvasOffset',
  )

  const scaledCanvasArea = React.useMemo(() => {
    if (outerBounds == null) {
      return null
    }
    const scaleRatio = canvasScale > 1 ? canvasScale : 1
    return windowRectangle({
      x: outerBounds.x * scaleRatio,
      y: outerBounds.y * scaleRatio,
      width: outerBounds.width,
      height: outerBounds.height,
    })
  }, [outerBounds, canvasScale])

  return React.useCallback(
    (frame: CanvasRectangle) => {
      if (scaledCanvasArea == null) {
        return null
      }

      if (frame.width === 0 || frame.height === 0) {
        return null
      }

      const topLeftPoint = canvasPointToWindowPoint(frame, canvasScale, canvasOffset)
      const elementWindowRect = windowRectangle({
        x: topLeftPoint.x,
        y: topLeftPoint.y,
        width: frame.width * canvasScale,
        height: frame.height * canvasScale,
      })
      if (rectangleIntersection(scaledCanvasArea, elementWindowRect) != null) {
        return null
      }

      return elementWindowRect
    },
    [scaledCanvasArea, canvasOffset, canvasScale],
  )
}

export function getCanvasBounds(): WindowRectangle | null {
  const canvasBounds = document.getElementById('canvas-root')?.getBoundingClientRect()
  if (canvasBounds == null) {
    return null
  }
  return windowRectangle({
    x: canvasBounds.x,
    y: canvasBounds.y,
    width: canvasBounds.width,
    height: canvasBounds.height,
  })
}
