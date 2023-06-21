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
  rectangleDifference,
  rectangleIntersection,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasToolbarId } from '../../editor/canvas-toolbar'
import { LeftPaneDefaultWidth } from '../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

type ElementOutsideVisibleArea = {
  type: 'selected' | 'highlighted'
  path: ElementPath
  rect: WindowRectangle
  diff: WindowRectangle
}

export type ElementOutsideVisibleAreaIndicator = {
  type: 'selected' | 'highlighted'
  path: ElementPath
  position: WindowPoint
  angle: number
  cluster: number
}

const minClusterDistance = 13
const canvasToolbarSkew = 13

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
    (store) => (store.editor.navigator.minimised ? 0 : LeftPaneDefaultWidth + 20),
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
  const canvasToolbar = document.getElementById(CanvasToolbarId)?.getBoundingClientRect()

  const canvasArea = React.useMemo(() => {
    if (bounds == null) {
      return null
    }
    return {
      x: bounds.left + navigatorWidth,
      y: bounds.top,
      width: bounds.width - navigatorWidth,
      height: bounds.height,
    } as WindowRectangle
  }, [bounds, navigatorWidth])

  const canvasAreaCenter = React.useMemo(() => {
    if (canvasArea == null) {
      return null
    }
    return getRectCenter(canvasArea)
  }, [canvasArea])

  const elementsOutsideVisibleArea = React.useMemo(() => {
    const maybeOutsideElement =
      (type: 'selected' | 'highlighted') =>
      (path: ElementPath): ElementOutsideVisibleArea | null => {
        if (canvasArea == null) {
          return null
        }

        const frame = framesByPathString[EP.toString(path)]
        if (frame == null) {
          return null
        }

        const elementTopLeftPoint = canvasPointToWindowPoint(
          canvasPoint({ x: frame.x, y: frame.y }),
          canvasScale,
          canvasOffset,
        )
        const elementRect = {
          x: elementTopLeftPoint.x,
          y: elementTopLeftPoint.y,
          width: frame.width,
          height: frame.height,
        } as WindowRectangle

        const diff = rectangleDifference(canvasArea, elementRect)

        if (rectangleIntersection(canvasArea, elementRect) != null) {
          return null
        }

        return {
          type,
          path: path,
          rect: elementRect,
          diff: diff,
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
    canvasArea,
    framesByPathString,
  ])

  const indicators = React.useMemo(() => {
    if (canvasArea == null || canvasAreaCenter == null) {
      return []
    }

    return (
      elementsOutsideVisibleArea
        // Map elements to indicators
        .map((element): ElementOutsideVisibleAreaIndicator => {
          const position = {
            x: indicatorPositionCoord(
              element.rect.x + element.rect.width / 2,
              canvasArea.x,
              canvasArea.width - 22, // 22 = size of the indicator
            ),
            y: indicatorPositionCoord(
              element.rect.y + element.rect.height / 2,
              0,
              canvasArea.height,
            ),
          } as WindowPoint

          const canvasToolbarOffset =
            canvasToolbar != null &&
            position.y >= 0 &&
            position.y <= canvasToolbar.height + canvasToolbarSkew &&
            position.x < canvasToolbar.x + canvasToolbar.width
              ? canvasToolbar.x + canvasToolbar.width
              : 0
          if (canvasToolbarOffset > 0) {
            position.x = canvasToolbarOffset
          }

          return {
            type: element.type,
            path: element.path,
            position: position,
            angle: angleBetweenPoints(canvasAreaCenter, getRectCenter(element.rect)),
            cluster: 1,
          }
        })
        // Group indicator clusters
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
  }, [elementsOutsideVisibleArea, canvasArea, canvasAreaCenter, canvasToolbar])

  return indicators
}

function indicatorPositionCoord(position: number, min: number, boundsSize: number) {
  return Math.min(Math.max(min, position), min + boundsSize)
}

function angleBetweenPoints(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}
