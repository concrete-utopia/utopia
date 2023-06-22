import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasRectangle,
  WindowPoint,
  WindowRectangle,
  canvasPoint,
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { LeftPaneDefaultWidth } from '../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { canvasPointToWindowPoint } from '../dom-lookup'

export type ElementOutsideVisibleAreaDirection = 'top' | 'left' | 'bottom' | 'right'

type ElementOutsideVisibleArea = {
  type: 'selected' | 'highlighted'
  path: ElementPath
  rect: WindowRectangle
  directions: ElementOutsideVisibleAreaDirection[]
}

export type ElementOutsideVisibleAreaIndicator = {
  type: 'selected' | 'highlighted'
  path: ElementPath
  position: WindowPoint
  angle: number
  cluster: number
  directions: ElementOutsideVisibleAreaDirection[]
}

const minClusterDistance = 13

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

  const canvasArea = React.useMemo(() => {
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
          if (canvasArea == null) {
            return []
          }
          const directions: ElementOutsideVisibleAreaDirection[] = []
          if (elementRect.x > canvasArea.x + canvasArea.width) {
            directions.push('right')
          }
          if (elementRect.x + elementRect.width < canvasArea.x) {
            directions.push('left')
          }
          if (elementRect.y > canvasArea.y + canvasArea.height) {
            directions.push('bottom')
          }
          if (elementRect.y + elementRect.height < canvasArea.y) {
            directions.push('top')
          }
          return directions
        }
        const directions = getDirections()
        if (directions.length === 0) {
          return null
        }

        return {
          type,
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
    canvasArea,
    framesByPathString,
    navigatorWidth,
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
            x: element.rect.x + element.rect.width / 2,
            y: element.rect.y + element.rect.height / 2,
          } as WindowPoint

          return {
            type: element.type,
            path: element.path,
            position: position,
            angle: angleBetweenPoints(canvasAreaCenter, getRectCenter(element.rect)),
            cluster: 1,
            directions: element.directions,
          }
        })
    )
  }, [elementsOutsideVisibleArea, canvasArea, canvasAreaCenter])

  return indicators
}

function angleBetweenPoints(from: WindowPoint, to: WindowPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}
