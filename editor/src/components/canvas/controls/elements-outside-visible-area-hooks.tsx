import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { CanvasRectangle, WindowPoint } from '../../../core/shared/math-utils'
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
  const canvasBounds = document.getElementById('canvas-root')?.getBoundingClientRect()

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

  const scaledCanvasArea = React.useMemo(() => {
    if (canvasBounds == null) {
      return null
    }
    const scaleRatio = canvasScale > 1 ? canvasScale : 1
    return windowRectangle({
      x: canvasBounds.x * scaleRatio,
      y: canvasBounds.y * scaleRatio,
      width: canvasBounds.width,
      height: canvasBounds.height,
    })
  }, [canvasBounds, canvasScale])

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

  return React.useMemo(() => {
    if (scaledCanvasArea == null) {
      return null
    }

    const framesOutsideVisibleArea = mapDropNulls((frame) => {
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
    }, selectedElementsFrames)

    const windowRect = boundingRectangleArray(framesOutsideVisibleArea)
    if (windowRect == null) {
      return null
    }

    return {
      position: getRectCenter(windowRect),
    }
  }, [selectedElementsFrames, canvasOffset, canvasScale, scaledCanvasArea])
}

export function getIndicatorAngleToTarget(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}
