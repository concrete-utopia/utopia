import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { CanvasPoint, CanvasRectangle, WindowRectangle } from '../../../core/shared/math-utils'
import { rectFromTwoPoints, rectangleFromTLBR, windowPoint } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorAction } from '../../editor/action-types'
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
import { useGetSelectableViewsForSelectModeFromSelectedViews } from './select-mode/select-mode-hooks'
import {
  filterUnderSelectionArea,
  getSelectionAreaRenderedRect,
  isValidMouseEventForSelectionArea,
} from './selection-area-helpers'
import { getAllFocusedPaths } from '../../editor/store/editor-state'
import { getAutofocusedPathsSelector } from '../../editor/store/editor-state-helpers'

export function useSelectionArea(
  ref: React.MutableRefObject<HTMLDivElement | null>,
  localHighlightedViews: ElementPath[],
  localSelectedViews: ElementPath[],
  setSelectionAreaRectangle: (area: WindowRectangle | null) => void,
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
      interactionSession: store.editor.canvas.interactionSession,
      keysPressed: store.editor.keysPressed,
      lockedElements: store.editor.lockedElements,
      focusedPaths: getAllFocusedPaths(
        store.editor.focusedElementPath,
        getAutofocusedPathsSelector(store),
      ),
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

  const getSelectableViews = useGetSelectableViewsForSelectModeFromSelectedViews(localSelectedViews)

  const getValidElementsUnderArea = React.useCallback(
    (area: CanvasRectangle | null): ElementPath[] => {
      // get all possible targets under the selection area
      const allTargetsUnderArea = getAllTargetsUnderAreaAABB(
        storeRef.current.jsxMetadata,
        localSelectedViews,
        storeRef.current.hiddenInstances,
        'no-filter',
        area,
        storeRef.current.elementPathTree,
        storeRef.current.allElementProps,
        true,
        [TargetSearchType.TopLevelElements, TargetSearchType.SelectedElements],
      )

      // filter out the targets that are not selectable
      // and aren't Scenes (which can be selected if fully contained)
      const selectableViews = getSelectableViews(false, true)

      const allTargetsMatchingSelectableViews = allTargetsUnderArea.filter(
        (path) =>
          EP.containsPath(path, selectableViews) ||
          MetadataUtils.isProbablyScene(storeRef.current.jsxMetadata, path),
      )

      // apply the selection-area specific filtering
      return filterUnderSelectionArea(
        allTargetsMatchingSelectableViews,
        storeRef.current.jsxMetadata,
        storeRef.current.scale,
        storeRef.current.canvasOffset,
        area,
        localSelectedViews,
        storeRef.current.lockedElements,
        storeRef.current.focusedPaths,
      )
    },
    [storeRef, localSelectedViews, getSelectableViews],
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
        isValidMouseEventForSelectionArea(
          mouseDownEvent,
          storeRef.current.interactionSession,
          storeRef.current.keysPressed,
        ) &&
        isSelectMode(storeRef.current.mode) &&
        localHighlightedViews.length === 0 &&
        getValidElementsUnderArea(mouseArea).length === 0

      if (!areaSelectionCanStart) {
        return false
      }

      let mouseDownActions: EditorAction[] = [
        switchEditorMode(EditorModes.selectMode(null, true, 'none')),
      ]
      if (!mouseDownEvent.shiftKey) {
        mouseDownActions.push(clearSelection())
      }
      dispatch(mouseDownActions)

      function getElementsUnderSelectionArea(mouseEvent: MouseEvent) {
        const moveMousePoint = windowPoint({
          x: mouseEvent.clientX,
          y: mouseEvent.clientY,
        })
        const selectionArea = rectFromTwoPoints(selectionAreaStart, moveMousePoint)

        // the rectangle displayed on the canvas
        const selectionAreaRectangle = getSelectionAreaRenderedRect(
          selectionArea,
          ref.current?.getBoundingClientRect() ?? null,
          storeRef.current.scale,
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
        return {
          selectionAreaRectangle,
          newHighlightedViews: getValidElementsUnderArea(selectionAreaCanvasRect),
        }
      }

      function onWindowMouseMove(mouseMoveEvent: MouseEvent) {
        if (
          !isValidMouseEventForSelectionArea(
            mouseMoveEvent,
            storeRef.current.interactionSession,
            storeRef.current.keysPressed,
          )
        ) {
          setSelectionAreaRectangle(null)
          setLocalHighlightedViews([])
          return
        }

        const { newHighlightedViews, selectionAreaRectangle } =
          getElementsUnderSelectionArea(mouseMoveEvent)

        setSelectionAreaRectangle(selectionAreaRectangle)
        setLocalHighlightedViews(newHighlightedViews)
      }

      function onWindowMouseUp(mouseUpEvent: MouseEvent) {
        const { newHighlightedViews } = getElementsUnderSelectionArea(mouseUpEvent)

        setSelectionAreaRectangle(null)
        setLocalHighlightedViews([])

        let mouseUpActions: EditorAction[] = [
          switchEditorMode(EditorModes.selectMode(null, false, 'none')),
        ]
        if (
          newHighlightedViews.length > 0 &&
          isValidMouseEventForSelectionArea(
            mouseUpEvent,
            storeRef.current.interactionSession,
            storeRef.current.keysPressed,
          )
        ) {
          const newSelectedComponents = mouseUpEvent.shiftKey
            ? localSelectedViews
                .concat(newHighlightedViews)
                .filter(
                  (path) =>
                    !EP.containsPath(path, localSelectedViews) ||
                    !EP.containsPath(path, newHighlightedViews),
                )
            : newHighlightedViews
          mouseUpActions.push(selectComponents(newSelectedComponents, false))
        }
        dispatch(mouseUpActions)

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
      ref,
      setLocalHighlightedViews,
      setSelectionAreaRectangle,
      storeRef,
      getValidElementsUnderArea,
      localSelectedViews,
    ],
  )

  return {
    onMouseDown: onMouseDown,
  }
}
