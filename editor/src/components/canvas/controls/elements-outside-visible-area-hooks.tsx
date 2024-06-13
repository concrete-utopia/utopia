import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import type { CanvasRectangle, WindowPoint, WindowRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  getRectCenter,
  isFiniteRectangle,
  rectangleIntersection,
  windowRectangle,
} from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

const topBarHeight = 40 // px

type ElementOutsideVisibleArea = {
  path: ElementPath
  rect: WindowRectangle
}

export type ElementOutsideVisibleAreaIndicator = {
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
      y: (canvasBounds.y - topBarHeight) * scaleRatio,
      width: canvasBounds.width,
      height: canvasBounds.height,
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

      const topLeftPoint = canvasPointToWindowPoint(frame, canvasScale, canvasOffset)
      const elementRect = windowRectangle({
        x: topLeftPoint.x,
        y: topLeftPoint.y - topBarHeight,
        width: frame.width * canvasScale,
        height: frame.height * canvasScale,
      })

      const isOutsideVisibleArea = rectangleIntersection(scaledCanvasArea, elementRect) == null
      if (!isOutsideVisibleArea) {
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
      position: getRectCenter(windowRect),
      selected: elementsOutsideVisibleArea.some((e) => EP.containsPath(e.path, selectedViews)),
    }
  }, [elementsOutsideVisibleArea, framesByPathString, selectedViews])
}

export function getIndicatorAngleToTarget(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}
