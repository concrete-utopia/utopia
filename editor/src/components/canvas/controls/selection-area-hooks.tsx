import React from 'react'
import {
  CanvasPoint,
  CanvasRectangle,
  rectangleFromTLBR,
  windowPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorAction } from '../../editor/action-types'
import {
  clearSelection,
  selectComponents,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes, isSelectMode } from '../../editor/editor-modes'
import { useDispatch } from '../../editor/store/dispatch-context'
import { useRefEditorState } from '../../editor/store/store-hook'
import Canvas, { TargetSearchType } from '../canvas'
import { getAllTargetsUnderAreaAABB, windowToCanvasCoordinates } from '../dom-lookup'
import {
  filterUnderSelectionArea,
  getSelectionAreaRenderedRect,
  isValidMouseEventForSelectionArea,
  makeSelectionArea,
} from './selection-area-helpers'

export function useSelectionArea(
  ref: React.MutableRefObject<HTMLDivElement | null>,
  localHighlightedViews: ElementPath[],
  localSelectedViews: ElementPath[],
  setSelectionAreaRectangle: (area: CanvasRectangle | null) => void,
  setLocalHighlightedViews: (views: ElementPath[]) => void,
): {
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => boolean
} {
  const dispatch = useDispatch()

  const storeRef = useRefEditorState((store) => {
    return {
      jsxMetadata: store.editor.jsxMetadata,
      hiddenInstances: store.editor.hiddenInstances,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      mode: store.editor.mode,
      elementPathTree: store.editor.elementPathTree,
      allElementProps: store.editor.allElementProps,
    }
  })

  const getCanvasPoint = React.useCallback(
    (x: number, y: number): CanvasPoint => {
      return windowToCanvasCoordinates(
        storeRef.current.scale,
        storeRef.current.canvasOffset,
        windowPoint({ x, y }),
      ).canvasPositionRounded
    },
    [storeRef],
  )

  const onMouseDown = React.useCallback(
    (mouseDownEvent: React.MouseEvent<HTMLDivElement>): boolean => {
      const selectionAreaStart = windowPoint({
        x: mouseDownEvent.clientX,
        y: mouseDownEvent.clientY,
      })

      const mouseArea = Canvas.getMousePositionCanvasArea(
        getCanvasPoint(selectionAreaStart.x, selectionAreaStart.y),
      )

      const areaSelectionCanStart =
        isValidMouseEventForSelectionArea(mouseDownEvent) &&
        isSelectMode(storeRef.current.mode) &&
        localHighlightedViews.length === 0 &&
        filterUnderSelectionArea(
          getAllTargetsUnderAreaAABB(
            storeRef.current.jsxMetadata,
            localSelectedViews,
            storeRef.current.hiddenInstances,
            'no-filter',
            mouseArea,
            storeRef.current.elementPathTree,
            storeRef.current.allElementProps,
            false,
            [TargetSearchType.SelectedElements],
          ),
          storeRef.current.jsxMetadata,
          mouseArea,
        ).length === 0

      if (!areaSelectionCanStart) {
        return false
      }

      dispatch([switchEditorMode(EditorModes.selectMode(null, true)), clearSelection()])

      function getElementsUnderSelectionArea(mouseEvent: MouseEvent) {
        const moveMousePoint = windowPoint({
          x: mouseEvent.clientX,
          y: mouseEvent.clientY,
        })
        const selectionArea = makeSelectionArea(selectionAreaStart, moveMousePoint)

        // the rectangle displayed on the canvas
        const selectionAreaRectangle = getSelectionAreaRenderedRect(
          selectionArea,
          ref.current?.getBoundingClientRect() ?? null,
        )

        // the canvas area for selecting elements
        const selectionAreaCanvasRect = rectangleFromTLBR(
          getCanvasPoint(selectionArea.x, selectionArea.y),
          getCanvasPoint(
            selectionArea.x + selectionArea.width,
            selectionArea.y + selectionArea.height,
          ),
          true,
        )
        const elementsUnderSelectionArea = getAllTargetsUnderAreaAABB(
          storeRef.current.jsxMetadata,
          localSelectedViews,
          storeRef.current.hiddenInstances,
          'no-filter',
          selectionAreaCanvasRect,
          storeRef.current.elementPathTree,
          storeRef.current.allElementProps,
          false,
        )
        return {
          selectionAreaRectangle,
          newHighlightedViews: filterUnderSelectionArea(
            elementsUnderSelectionArea,
            storeRef.current.jsxMetadata,
            selectionAreaCanvasRect,
          ),
        }
      }

      function onWindowMouseMove(mouseMoveEvent: MouseEvent) {
        const { newHighlightedViews, selectionAreaRectangle } =
          getElementsUnderSelectionArea(mouseMoveEvent)

        setSelectionAreaRectangle(selectionAreaRectangle)
        setLocalHighlightedViews(newHighlightedViews)
      }

      function onWindowMouseUp(mouseUpEvent: MouseEvent) {
        const { newHighlightedViews } = getElementsUnderSelectionArea(mouseUpEvent)

        setSelectionAreaRectangle(null)
        setLocalHighlightedViews([])

        let actions: EditorAction[] = [switchEditorMode(EditorModes.selectMode())]
        if (newHighlightedViews.length > 0 && isValidMouseEventForSelectionArea(mouseUpEvent)) {
          actions.push(selectComponents(newHighlightedViews, false))
        }
        dispatch(actions)

        window.removeEventListener('mousemove', onWindowMouseMove, { capture: true })
        window.removeEventListener('mouseup', onWindowMouseUp, { capture: true })
      }

      window.addEventListener('mousemove', onWindowMouseMove, { capture: true })
      window.addEventListener('mouseup', onWindowMouseUp, { capture: true })

      return true
    },
    [
      dispatch,
      getCanvasPoint,
      localHighlightedViews,
      localSelectedViews,
      ref,
      setLocalHighlightedViews,
      setSelectionAreaRectangle,
      storeRef,
    ],
  )

  return {
    onMouseDown: onMouseDown,
  }
}
