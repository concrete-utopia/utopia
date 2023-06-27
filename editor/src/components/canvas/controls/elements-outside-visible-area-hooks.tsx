import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasRectangle,
  WindowPoint,
  WindowRectangle,
  clamp,
  distance,
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasToolbarId } from '../../editor/canvas-toolbar'
import { LeftPaneDefaultWidth } from '../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

export const ElementOutisdeVisibleAreaIndicatorSize = 22 // px
const minClusterDistance = 17 // px
const topBarHeight = 40 // px
const canvasToolbarSkew = topBarHeight + ElementOutisdeVisibleAreaIndicatorSize

type ElementOutsideVisibleAreaDirection = 'top' | 'left' | 'bottom' | 'right'

type ElementOutsideVisibleArea = {
  path: ElementPath
  rect: WindowRectangle
  directions: ElementOutsideVisibleAreaDirection[]
}

export type ElementOutsideVisibleAreaIndicator = {
  id: string
  path: ElementPath
  position: WindowPoint
  angle: number
  cluster: number
}

export function useElementsOutsideVisibleArea(
  ref: React.MutableRefObject<HTMLDivElement | null>,
  localHighlightedViews: ElementPath[],
  localSelectedViews: ElementPath[],
): ElementOutsideVisibleAreaIndicator[] {
  const bounds = ref.current?.getBoundingClientRect() ?? null
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
  const navigatorWidth = useEditorState(
    Substores.restOfEditor,
    (store) => (store.editor.navigator.minimised ? 0 : LeftPaneDefaultWidth),
    'useElementsOutsideVisibleArea navigatorMinimised',
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
    return {
      x: bounds.x * scaleRatio,
      y: bounds.y * scaleRatio,
      width: bounds.width * scaleRatio - navigatorWidth,
      height: bounds.height * scaleRatio,
    } as WindowRectangle
  }, [bounds, navigatorWidth, canvasScale])

  const scaledCanvasAreaCenter = React.useMemo(() => {
    if (scaledCanvasArea == null) {
      return null
    }
    return getRectCenter(scaledCanvasArea)
  }, [scaledCanvasArea])

  const elementsOutsideVisibleArea = React.useMemo(() => {
    return mapDropNulls((path: ElementPath): ElementOutsideVisibleArea | null => {
      if (scaledCanvasArea == null) {
        return null
      }
      const frame = framesByPathString[EP.toString(path)]
      if (frame == null) {
        return null
      }

      const topLeftSkew = { x: -navigatorWidth, y: 0 } as WindowPoint
      const topLeftPoint = offsetPoint(
        canvasPointToWindowPoint(frame, canvasScale, canvasOffset),
        topLeftSkew,
      )
      const elementRect = {
        x: topLeftPoint.x,
        y: topLeftPoint.y,
        width: frame.width * canvasScale,
        height: frame.height * canvasScale,
      } as WindowRectangle

      const directions = getOutsideDirections(scaledCanvasArea, elementRect)
      if (directions.length === 0) {
        return null
      }

      return {
        path: path,
        rect: elementRect,
        directions: directions,
      }
    }, elements)
  }, [elements, canvasOffset, canvasScale, scaledCanvasArea, framesByPathString, navigatorWidth])

  return React.useMemo((): ElementOutsideVisibleAreaIndicator[] => {
    if (
      scaledCanvasArea == null ||
      scaledCanvasAreaCenter == null ||
      bounds == null ||
      canvasToolbar == null
    ) {
      return []
    }

    const indicators: ElementOutsideVisibleAreaIndicator[] = []
    for (const { rect, path, directions } of elementsOutsideVisibleArea) {
      // Map element to indicator
      const indicator: ElementOutsideVisibleAreaIndicator = {
        id: getIndicatorId(path, directions),
        path: path,
        cluster: 1,
        angle: angleBetweenPoints(scaledCanvasAreaCenter, getRectCenter(rect)),
        position: adjustPosition(
          offsetPoint(rect, {
            x: rect.width / 2,
            y: rect.height / 2,
          } as WindowPoint),
          directions,
          scaledCanvasArea,
          navigatorWidth,
          windowRectangleFromDOMRect(canvasToolbar),
        ),
      }

      // Group the indicators into clusters
      const index = indicators.findIndex((other) => {
        const distanceBetween = distance(indicator.position, other.position)
        return distanceBetween < minClusterDistance
      })
      if (index >= 0) {
        indicators[index].cluster++
      } else {
        indicators.push(indicator)
      }
    }
    return indicators
  }, [
    elementsOutsideVisibleArea,
    scaledCanvasArea,
    scaledCanvasAreaCenter,
    navigatorWidth,
    bounds,
    canvasToolbar,
  ])
}

function angleBetweenPoints(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}

type ElementOutsideVisibleAreaDirectionBaseValue = {
  direction: ElementOutsideVisibleAreaDirection
  baseValue: number
}

function getPositionAxisRelativeToDirection(
  directions: ElementOutsideVisibleAreaDirection[],
  currentValue: number,
  min: ElementOutsideVisibleAreaDirectionBaseValue,
  max: ElementOutsideVisibleAreaDirectionBaseValue,
): number {
  if (directions.includes(min.direction)) {
    return min.baseValue
  } else if (directions.includes(max.direction)) {
    return max.baseValue
  } else {
    return clamp(min.baseValue, max.baseValue, currentValue)
  }
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

function adjustPosition(
  position: WindowPoint,
  directions: ElementOutsideVisibleAreaDirection[],
  bounds: WindowRectangle,
  navigatorWidth: number,
  canvasToolbar: WindowRectangle | null,
): WindowPoint {
  const canvasToolbarOffset =
    canvasToolbar != null &&
    position.y <= canvasToolbar.height + canvasToolbarSkew &&
    position.x < canvasToolbar.x + canvasToolbar.width
      ? canvasToolbar.width + minClusterDistance
      : 0

  return {
    x: getPositionAxisRelativeToDirection(
      directions,
      position.x - bounds.x - ElementOutisdeVisibleAreaIndicatorSize / 2 + navigatorWidth,
      {
        direction: 'left',
        baseValue:
          (navigatorWidth > 0 ? navigatorWidth + ElementOutisdeVisibleAreaIndicatorSize : 0) +
          canvasToolbarOffset,
      },
      {
        direction: 'right',
        baseValue: bounds.width - ElementOutisdeVisibleAreaIndicatorSize + navigatorWidth,
      },
    ),
    y: getPositionAxisRelativeToDirection(
      directions,
      position.y - topBarHeight - ElementOutisdeVisibleAreaIndicatorSize / 2,
      {
        direction: 'top',
        baseValue: 0,
      },
      {
        direction: 'bottom',
        baseValue: bounds.height - ElementOutisdeVisibleAreaIndicatorSize,
      },
    ),
  } as WindowPoint
}

function windowRectangleFromDOMRect(rect: DOMRect): WindowRectangle {
  return {
    x: rect.x,
    y: rect.y,
    width: rect.width,
    height: rect.height,
  } as WindowRectangle
}

export function getIndicatorId(
  path: ElementPath,
  directions: ElementOutsideVisibleAreaDirection[],
): string {
  return `indicator-${EP.toVarSafeComponentId(path)}-${directions.join('_')}`
}

export function getIndicatorClusterLabel(indicator: ElementOutsideVisibleAreaIndicator): string {
  return indicator.cluster > 10 ? '10+' : `${indicator.cluster}`
}
