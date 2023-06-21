import React from 'react'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasPoint,
  CanvasRectangle,
  canvasPoint,
  canvasRectangle,
  distance,
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
  rectangleDifference,
  rectangleIntersection,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import { LeftPaneDefaultWidth } from '../../editor/store/editor-state'
import { canvasPointToWindowPoint } from '../dom-lookup'
import { CanvasToolbarId } from '../../editor/canvas-toolbar'

type ElementOutsideVisibleArea = {
  type: 'selected' | 'highlighted'
  path: ElementPath
  rect: CanvasRectangle
  diff: CanvasRectangle
}

export type ElementOutsideVisibleAreaIndicator = {
  type: 'selected' | 'highlighted'
  path: ElementPath
  position: CanvasPoint
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
  const xOffset = useEditorState(
    Substores.restOfEditor,
    (store) => (store.editor.navigator.minimised ? 0 : LeftPaneDefaultWidth + 20),
    'useOutsideElements navigatorMinimised',
  )
  const inspectorWidth = useEditorState(
    Substores.restOfEditor,
    (store) => (store.editor.inspector.visible ? 300 : 0),
    'OutsideElements inspectoWidth',
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
    return canvasRectangle({
      x: bounds.left + xOffset,
      y: bounds.top,
      width: bounds.width - xOffset,
      height: bounds.height,
    })
  }, [bounds, xOffset])

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
        const elementRect = canvasRectangle({
          x: elementTopLeftPoint.x,
          y: elementTopLeftPoint.y,
          width: frame.width,
          height: frame.height,
        })

        if (rectangleIntersection(canvasArea, elementRect) != null) {
          return null
        }

        return {
          type,
          path: path,
          rect: elementRect,
          diff: rectangleDifference(canvasArea, elementRect),
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
    if (bounds == null) {
      return []
    }

    const boundsCenter = getRectCenter(
      canvasRectangle({
        x: bounds.x + xOffset,
        y: bounds.y,
        width: bounds.width - xOffset,
        height: bounds.height,
      }),
    )

    return (
      elementsOutsideVisibleArea
        // Map elements to indicators
        .map((element): ElementOutsideVisibleAreaIndicator => {
          const position = canvasPoint({
            x: indicatorPositionCoord(
              canvasScale,
              element.diff.x,
              element.rect.width,
              (bounds.width ?? Infinity) - inspectorWidth,
            ),
            y: indicatorPositionCoord(
              canvasScale,
              element.diff.y,
              element.rect.height,
              (bounds.height ?? Infinity) - bounds.y / 2,
            ),
          })

          const canvasToolbarOffset =
            canvasToolbar != null &&
            position.y >= 0 &&
            position.y <= canvasToolbar.height + canvasToolbarSkew &&
            position.x < canvasToolbar.width + canvasToolbarSkew
              ? canvasToolbar.width + canvasToolbarSkew
              : 0
          if (canvasToolbarOffset > 0) {
            position.x = canvasToolbarOffset
          }

          return {
            type: element.type,
            path: element.path,
            position: offsetPoint(position, canvasPoint({ x: xOffset, y: 0 })),
            angle: angleBetweenPoints(boundsCenter, getRectCenter(element.rect)),
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
  }, [elementsOutsideVisibleArea, canvasToolbar, bounds, canvasScale, inspectorWidth, xOffset])

  return indicators
}

function indicatorPositionCoord(scale: number, diff: number, rectSize: number, boundsSize: number) {
  const minSkew = 2
  return (Math.min(Math.max(minSkew, diff + rectSize / 2), boundsSize) * 1) / scale
}

function angleBetweenPoints(from: CanvasPoint, to: CanvasPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}
