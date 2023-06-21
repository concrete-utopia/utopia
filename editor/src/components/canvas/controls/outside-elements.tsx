import React from 'react'
import {
  CanvasPoint,
  CanvasRectangle,
  canvasPoint,
  canvasRectangle,
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
  rectangleDifference,
  rectangleIntersection,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { LeftPaneDefaultWidth } from '../../editor/store/editor-state'
import { canvasPointToWindowPoint } from '../dom-lookup'

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
}

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

  const bounds = ref.current?.getBoundingClientRect() ?? null

  const elementsOutsideVisibleArea = React.useMemo(() => {
    const maybeOutsideElement =
      (type: 'selected' | 'highlighted') =>
      (path: ElementPath): ElementOutsideVisibleArea | null => {
        const meta = MetadataUtils.findElementByElementPath(storeRef.current.jsxMetadata, path)
        if (meta == null || meta.globalFrame == null || !isFiniteRectangle(meta.globalFrame)) {
          return null
        }

        const box = ref.current?.getBoundingClientRect() ?? null
        if (box == null) {
          return null
        }

        const origin = canvasPointToWindowPoint(
          canvasPoint({ x: meta.globalFrame.x, y: meta.globalFrame.y }),
          canvasScale,
          canvasOffset,
        )
        const windowRect = canvasRectangle({
          x: origin.x,
          y: origin.y,
          width: meta.globalFrame.width,
          height: meta.globalFrame.height,
        })

        const canvasArea = canvasRectangle({
          x: box.left + xOffset,
          y: box.top,
          width: box.width - xOffset,
          height: box.height,
        })
        if (rectangleIntersection(canvasArea, windowRect) != null) {
          return null
        }

        return {
          type,
          path: meta.elementPath,
          rect: windowRect,
          diff: rectangleDifference(canvasArea, windowRect),
        }
      }
    return [
      ...mapDropNulls(maybeOutsideElement('highlighted'), localHighlightedViews),
      ...mapDropNulls(maybeOutsideElement('selected'), localSelectedViews),
    ]
  }, [localHighlightedViews, localSelectedViews, storeRef, ref, canvasOffset, canvasScale, xOffset])

  return React.useMemo(() => {
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

    return elementsOutsideVisibleArea.map((element): ElementOutsideVisibleAreaIndicator => {
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
      return {
        type: element.type,
        path: element.path,
        position: offsetPoint(position, canvasPoint({ x: xOffset, y: 0 })),
        angle: angleBetweenPoints(boundsCenter, getRectCenter(element.rect)),
      }
    })
  }, [elementsOutsideVisibleArea, bounds, canvasScale, inspectorWidth, xOffset])
}

function indicatorPositionCoord(scale: number, diff: number, rectSize: number, boundsSize: number) {
  const minSkew = 2
  return (Math.min(Math.max(minSkew, diff + rectSize / 2), boundsSize) * 1) / scale
}

function angleBetweenPoints(from: CanvasPoint, to: CanvasPoint): number {
  return Math.atan2(to.y - from.y, to.x - from.x) + Math.PI
}
