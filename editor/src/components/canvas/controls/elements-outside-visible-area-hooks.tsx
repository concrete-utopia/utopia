import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasRectangle,
  WindowPoint,
  WindowRectangle,
  canvasPoint,
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

export type ElementOutsideVisibleAreaDirection = 'top' | 'left' | 'bottom' | 'right'

type ElementOutsideVisibleAreaStatus = 'selected' | 'highlighted'

type ElementOutsideVisibleArea = {
  status: ElementOutsideVisibleAreaStatus
  path: ElementPath
  rect: WindowRectangle
  directions: ElementOutsideVisibleAreaDirection[]
}

export type ElementOutsideVisibleAreaIndicator = {
  status: ElementOutsideVisibleAreaStatus
  path: ElementPath
  position: WindowPoint
  angle: number
  cluster: number
}

const minClusterDistance = 17
const topBarHeight = 40
const indicatorSize = 22
const canvasToolbarSkew = topBarHeight + indicatorSize

export function useElementsOutsideVisibleArea(
  ref: React.MutableRefObject<HTMLDivElement | null>,
  localHighlightedViews: ElementPath[],
  localSelectedViews: ElementPath[],
): ElementOutsideVisibleAreaIndicator[] {
  const storeRef = useRefEditorState((store) => ({
    jsxMetadata: store.editor.jsxMetadata,
  }))

  const { canvasScale, canvasOffset } = useEditorState(
    Substores.canvasOffset,
    (store) => ({
      canvasScale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }),
    'useOutsideElements canvasOffset',
  )
  const navigatorWidth = useEditorState(
    Substores.restOfEditor,
    (store) => (store.editor.navigator.minimised ? 0 : LeftPaneDefaultWidth),
    'useOutsideElements navigatorMinimised',
  )

  const framesByPathString = React.useMemo(() => {
    return uniqBy(localSelectedViews.concat(localHighlightedViews), EP.pathsEqual).reduce(
      (acc, path) => {
        const metadata = MetadataUtils.findElementByElementPath(storeRef.current.jsxMetadata, path)
        if (
          metadata != null &&
          metadata.globalFrame != null &&
          isFiniteRectangle(metadata.globalFrame)
        ) {
          acc[EP.toString(path)] = metadata.globalFrame
        }
        return acc
      },
      {} as { [key: string]: CanvasRectangle },
    )
  }, [storeRef, localSelectedViews, localHighlightedViews])

  const bounds = ref.current?.getBoundingClientRect() ?? null

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
    const maybeOutsideElement =
      (status: ElementOutsideVisibleAreaStatus) =>
      (path: ElementPath): ElementOutsideVisibleArea | null => {
        if (scaledCanvasArea == null) {
          return null
        }

        const frame = framesByPathString[EP.toString(path)]
        if (frame == null) {
          return null
        }

        const elementTopLeftPoint = offsetPoint(
          canvasPointToWindowPoint(
            canvasPoint({ x: frame.x, y: frame.y }),
            canvasScale,
            canvasOffset,
          ),
          { x: -navigatorWidth, y: 0 } as WindowPoint,
        )
        const elementRect = {
          x: elementTopLeftPoint.x,
          y: elementTopLeftPoint.y,
          width: frame.width * canvasScale,
          height: frame.height * canvasScale,
        } as WindowRectangle

        function getDirections(): ElementOutsideVisibleAreaDirection[] {
          if (scaledCanvasArea == null) {
            return []
          }
          const directions: ElementOutsideVisibleAreaDirection[] = []
          if (elementRect.x > scaledCanvasArea.x + scaledCanvasArea.width) {
            directions.push('right')
          }
          if (elementRect.x + elementRect.width < scaledCanvasArea.x) {
            directions.push('left')
          }
          if (elementRect.y > scaledCanvasArea.y + scaledCanvasArea.height) {
            directions.push('bottom')
          }
          if (elementRect.y + elementRect.height < scaledCanvasArea.y) {
            directions.push('top')
          }
          return directions
        }
        const directions = getDirections()
        if (directions.length === 0) {
          return null
        }

        return {
          status,
          path: path,
          rect: elementRect,
          directions,
        }
      }
    return [
      ...mapDropNulls(maybeOutsideElement('highlighted'), localHighlightedViews),
      ...mapDropNulls(maybeOutsideElement('selected'), localSelectedViews),
    ]
  }, [
    localHighlightedViews,
    localSelectedViews,
    canvasOffset,
    canvasScale,
    scaledCanvasArea,
    framesByPathString,
    navigatorWidth,
  ])

  const indicators = React.useMemo(() => {
    if (scaledCanvasArea == null || scaledCanvasAreaCenter == null || bounds == null) {
      return []
    }

    const canvasToolbar = document.getElementById(CanvasToolbarId)?.getBoundingClientRect()

    return (
      elementsOutsideVisibleArea
        // Map elements to indicators
        .map((element): ElementOutsideVisibleAreaIndicator => {
          const basePosition = {
            x: element.rect.x + element.rect.width / 2,
            y: element.rect.y + element.rect.height / 2,
          } as WindowPoint

          const canvasToolbarOffset =
            canvasToolbar != null &&
            basePosition.y <= canvasToolbar.height + canvasToolbarSkew &&
            basePosition.x < canvasToolbar.x + canvasToolbar.width
              ? canvasToolbar.width + minClusterDistance
              : 0

          const adjustedPosition = {
            x: getPositionAxisRelativeToDirection(
              element.directions,
              basePosition.x - bounds.x - indicatorSize / 2 + navigatorWidth,
              {
                direction: 'left',
                baseValue:
                  (navigatorWidth > 0 ? navigatorWidth + indicatorSize : 0) + canvasToolbarOffset,
              },
              { direction: 'right', baseValue: bounds.width - indicatorSize },
            ),
            y: getPositionAxisRelativeToDirection(
              element.directions,
              basePosition.y - topBarHeight - indicatorSize / 2,
              { direction: 'top', baseValue: 0 },
              { direction: 'bottom', baseValue: bounds.height - indicatorSize },
            ),
          } as WindowPoint

          const angleFromCenter = angleBetweenPoints(
            scaledCanvasAreaCenter,
            getRectCenter(element.rect),
          )

          return {
            status: element.status,
            path: element.path,
            angle: angleFromCenter,
            position: adjustedPosition,
            cluster: 1,
          }
        })
        // Group the indicators into clusters
        .reduce((arr, indicator) => {
          const index = arr.findIndex((other) => {
            const distanceBetween = distance(indicator.position, other.position)
            return distanceBetween < minClusterDistance
          })
          if (index >= 0) {
            arr[index].cluster++
          } else {
            arr.push(indicator)
          }
          return arr
        }, [] as ElementOutsideVisibleAreaIndicator[])
    )
  }, [elementsOutsideVisibleArea, scaledCanvasArea, scaledCanvasAreaCenter, navigatorWidth, bounds])

  return indicators
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
) {
  if (directions.includes(min.direction)) {
    return min.baseValue
  } else if (directions.includes(max.direction)) {
    return max.baseValue
  } else {
    return Math.max(Math.min(currentValue, max.baseValue), min.baseValue)
  }
}
