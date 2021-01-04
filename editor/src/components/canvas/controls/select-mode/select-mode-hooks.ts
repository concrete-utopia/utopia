import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { uniqBy } from '../../../../core/shared/array-utils'
import { JSXMetadata } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  distance,
  point,
  WindowPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ScenePath, TemplatePath } from '../../../../core/shared/project-file-types'
import * as TP from '../../../../core/shared/template-path'
import { fastForEach, NO_OP } from '../../../../core/shared/utils'
import { KeysPressed } from '../../../../utils/keyboard'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import Utils from '../../../../utils/utils'
import {
  clearHighlightedViews,
  clearSelection,
  selectComponents,
  setHighlightedView,
} from '../../../editor/actions/action-creators'
import {
  EditorState,
  getOpenUtopiaJSXComponentsFromState,
} from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { DragState, moveDragState } from '../../canvas-types'
import { createDuplicationNewUIDs, getOriginalCanvasFrames } from '../../canvas-utils'
import { findFirstParentWithValidUID } from '../../dom-lookup'
import { useWindowToCanvasCoordinates } from '../../dom-lookup-hooks'
import { selectElementsThatRespectLayout } from '../new-canvas-controls'

const DRAG_START_TRESHOLD = 2

export function isResizing(dragState: DragState | null): boolean {
  return dragState != null && dragState.type === 'RESIZE_DRAG_STATE' && dragState.drag != null
}

export function isDragging(dragState: DragState | null): boolean {
  return dragState != null && dragState.type === 'MOVE_DRAG_STATE' && dragState.drag != null
}

export function pickSelectionEnabled(
  canvas: EditorState['canvas'],
  keysPressed: KeysPressed,
): boolean {
  return canvas.selectionControlsVisible && !keysPressed['z'] && canvas.textEditor == null
}

/**
 * maybeHighlightOnHover and maybeClearHighlightsOnHoverEnd are moved here from new-canvas-controls, kept as-is for continuity
 */
export function useMaybeHighlightElement(): {
  maybeHighlightOnHover: (target: TemplatePath) => void
  maybeClearHighlightsOnHoverEnd: () => void
} {
  const stateRef = useRefEditorState((store) => {
    return {
      dispatch: store.dispatch,
      resizing: isResizing(store.editor.canvas.dragState),
      dragging: isDragging(store.editor.canvas.dragState),
      selectionEnabled: pickSelectionEnabled(store.editor.canvas, store.editor.keysPressed),
    }
  })

  const maybeHighlightOnHover = React.useCallback(
    (target: TemplatePath): void => {
      const { dispatch, dragging, resizing, selectionEnabled } = stateRef.current
      if (selectionEnabled && !dragging && !resizing) {
        dispatch([setHighlightedView(target)], 'canvas')
      }
    },
    [stateRef],
  )

  const maybeClearHighlightsOnHoverEnd = React.useCallback((): void => {
    const { dispatch, dragging, resizing, selectionEnabled } = stateRef.current
    if (selectionEnabled && !dragging && !resizing) {
      dispatch([clearHighlightedViews()], 'canvas')
    }
  }, [stateRef])

  return {
    maybeHighlightOnHover: maybeHighlightOnHover,
    maybeClearHighlightsOnHoverEnd: maybeClearHighlightsOnHoverEnd,
  }
}

function filterHiddenInstances(
  hiddenInstances: Array<TemplatePath>,
  paths: Array<TemplatePath>,
): Array<TemplatePath> {
  return paths.filter((path) => hiddenInstances.every((hidden) => !TP.pathsEqual(path, hidden)))
}

export function getSelectableViews(
  componentMetadata: JSXMetadata,
  selectedViews: Array<TemplatePath>,
  hiddenInstances: Array<TemplatePath>,
  allElementsDirectlySelectable: boolean,
): TemplatePath[] {
  let candidateViews: Array<TemplatePath>

  if (allElementsDirectlySelectable) {
    candidateViews = MetadataUtils.getAllPaths(componentMetadata)
  } else {
    const scenes = MetadataUtils.getAllScenePaths(componentMetadata.components)
    let rootElementsToFilter: TemplatePath[] = []
    let dynamicScenesWithFragmentRootViews: ScenePath[] = []
    Utils.fastForEach(scenes, (path) => {
      const scene = MetadataUtils.findSceneByTemplatePath(componentMetadata.components, path)
      const rootElements = scene?.rootElements
      if (
        MetadataUtils.isSceneTreatedAsGroup(scene) &&
        rootElements != null &&
        rootElements.length > 1
      ) {
        rootElementsToFilter.push(...rootElements)
        dynamicScenesWithFragmentRootViews.push(path)
      }
    })
    const allRoots = MetadataUtils.getAllCanvasRootPaths(componentMetadata).filter((rootPath) => {
      return !rootElementsToFilter.some((path) => TP.pathsEqual(rootPath, path))
    })
    let siblings: Array<TemplatePath> = []
    Utils.fastForEach(selectedViews, (view) => {
      Utils.fastForEach(TP.allPaths(view), (ancestor) => {
        const ancestorChildren = MetadataUtils.getImmediateChildren(componentMetadata, ancestor)
        fastForEach(ancestorChildren, (child) => siblings.push(child.templatePath))
      })
    })

    const selectableViews = [...dynamicScenesWithFragmentRootViews, ...allRoots, ...siblings]
    const uniqueSelectableViews = uniqBy<TemplatePath>(selectableViews, TP.pathsEqual)

    const selectableViewsFiltered = uniqueSelectableViews.filter((view) => {
      // I kept the group-like behavior here that the user can't single-click select the parent group, even though it is a view now
      const isGroup = MetadataUtils.isAutoSizingViewFromComponents(componentMetadata, view)
      const isAncestorOfSelected = selectedViews.some((selectedView) =>
        TP.isAncestorOf(selectedView, view, false),
      )
      if (isGroup && isAncestorOfSelected) {
        return false
      } else {
        return true
      }
    })
    candidateViews = selectableViewsFiltered
  }

  return filterHiddenInstances(hiddenInstances, candidateViews)
}

function useFindValidTarget(): (
  targetHtmlElement: HTMLElement,
  allElementsDirectlySelectable: boolean,
) => {
  templatePath: TemplatePath
  selectionMode: 'singleclick' | 'doubleclick'
  isSelected: boolean
} | null {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadataKILLME,
      selectedViews: store.editor.selectedViews,
      hiddenInstances: store.editor.hiddenInstances,
    }
  })

  return React.useCallback(
    (targetHtmlElement: HTMLElement, allElementsDirectlySelectable: boolean) => {
      const { componentMetadata, selectedViews, hiddenInstances } = storeRef.current
      const selectableViews = getSelectableViews(
        componentMetadata,
        selectedViews,
        hiddenInstances,
        allElementsDirectlySelectable,
      )
      const validElementMouseOver: string | null = findFirstParentWithValidUID(
        selectableViews.map(TP.toString),
        targetHtmlElement as HTMLElement,
      )
      const validTemplatePath: TemplatePath | null =
        validElementMouseOver != null ? TP.fromString(validElementMouseOver) : null
      if (validTemplatePath != null) {
        const isSelected = selectedViews.some((selectedView) =>
          TP.pathsEqual(validTemplatePath, selectedView),
        )
        const isChild = selectedViews.some((selectedView) =>
          TP.isChildOf(validTemplatePath, selectedView),
        )
        return {
          templatePath: validTemplatePath,
          selectionMode: isChild ? 'doubleclick' : 'singleclick',
          isSelected: isSelected,
        }
      } else {
        return null
      }
    },
    [storeRef],
  )
}

function useStartDragState(): (
  target: TemplatePath,
  start: CanvasPoint | null,
) => (event: MouseEvent) => void {
  const dispatch = useEditorState((store) => store.dispatch, 'useStartDragState dispatch')
  const entireEditorStoreRef = useRefEditorState((store) => store)

  return React.useCallback(
    (target: TemplatePath, start: CanvasPoint | null) => (event: MouseEvent) => {
      if (start == null) {
        return
      }

      const componentMetadata = entireEditorStoreRef.current.editor.jsxMetadataKILLME
      const selectedViews = entireEditorStoreRef.current.editor.selectedViews

      const rootComponents = getOpenUtopiaJSXComponentsFromState(
        entireEditorStoreRef.current.editor,
      )

      const elementsThatRespectLayout = selectElementsThatRespectLayout(
        entireEditorStoreRef.current,
      )

      const duplicate = event.altKey
      const duplicateNewUIDs = duplicate
        ? createDuplicationNewUIDs(selectedViews, componentMetadata, rootComponents)
        : null

      const selection = TP.areAllElementsInSameScene(selectedViews) ? selectedViews : [target]
      const moveTargets = selection.filter(
        (view) =>
          TP.isScenePath(view) ||
          elementsThatRespectLayout.some((path) => TP.pathsEqual(path, view)),
      )

      let originalFrames = getOriginalCanvasFrames(moveTargets, componentMetadata)
      originalFrames = originalFrames.filter((f) => f.frame != null)

      const selectionArea = boundingRectangleArray(
        selectedViews.map((view) => {
          return MetadataUtils.getFrameInCanvasCoords(view, componentMetadata)
        }),
      )

      dispatch([
        CanvasActions.createDragState(
          moveDragState(
            start,
            null,
            null,
            originalFrames,
            selectionArea,
            !event.metaKey,
            event.shiftKey,
            duplicate,
            event.metaKey,
            duplicateNewUIDs,
            start,
            componentMetadata,
            moveTargets,
          ),
        ),
      ])
    },
    [dispatch, entireEditorStoreRef],
  )
}

function callbackAfterDragExceedsThreshold(
  startEvent: MouseEvent,
  threshold: number,
  callback: (event: MouseEvent) => void,
) {
  const startPoint = windowPoint(point(startEvent.clientX, startEvent.clientY))
  function onMouseMove(event: MouseEvent) {
    if (distance(startPoint, windowPoint(point(event.clientX, event.clientY))) > threshold) {
      callback(event)
      removeListeners()
    }
  }

  function onMouseUp() {
    removeListeners()
  }

  function removeListeners() {
    window.removeEventListener('mousemove', onMouseMove)
    window.removeEventListener('mouseup', onMouseUp)
  }

  window.addEventListener('mousemove', onMouseMove)
  window.addEventListener('mouseup', onMouseUp)
}

export function useSelectModeSelectAndHover(): {
  onMouseOver: (event: React.MouseEvent<HTMLDivElement>) => void
  onMouseOut: () => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement>) => void
} {
  const dispatch = useEditorState((store) => store.dispatch, 'useSelectAndHover dispatch')
  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
  const findValidTarget = useFindValidTarget()
  const startDragState = useStartDragState()
  const windowToCanvasCoordinates = useWindowToCanvasCoordinates()

  const onMouseOver = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const validTemplatePath = findValidTarget(event.target as HTMLDivElement, event.metaKey)
      if (
        validTemplatePath != null &&
        validTemplatePath.selectionMode === 'singleclick' && // we only show highlights for single-click selectable elements
        !validTemplatePath.isSelected // do not highlight selected elements
      ) {
        maybeHighlightOnHover(validTemplatePath.templatePath)
      }
    },
    [maybeHighlightOnHover, findValidTarget],
  )

  const onMouseOut = React.useCallback(() => {
    maybeClearHighlightsOnHoverEnd()
  }, [maybeClearHighlightsOnHoverEnd])

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const foundTarget = findValidTarget(event.target as HTMLDivElement, event.metaKey)
      if (foundTarget != null) {
        callbackAfterDragExceedsThreshold(
          event.nativeEvent,
          DRAG_START_TRESHOLD,
          startDragState(
            foundTarget.templatePath,
            windowToCanvasCoordinates(windowPoint(point(event.clientX, event.clientY)))
              .canvasPositionRounded,
          ),
        )

        if (!foundTarget.isSelected) {
          const doubleClick = event.detail > 1 // we interpret a triple click as two double clicks, a quadruple click as three double clicks, etc  // TODO TEST ME
          if (foundTarget.selectionMode === 'singleclick' || doubleClick) {
            dispatch([selectComponents([foundTarget.templatePath], event.shiftKey)])
            // TODO BALAZS repeat the hack from select-mode-control-container which sets the selected views with a priority on the canvas controls first
          }
        }
      }
    },
    [dispatch, findValidTarget, startDragState, windowToCanvasCoordinates],
  )

  // TODO deselect control under the canvas

  return { onMouseOver, onMouseOut, onMouseDown }
}

function useInsertModeSelectAndHover(): {
  onMouseOver: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseOut: () => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  return useKeepShallowReferenceEquality({
    onMouseOver: NO_OP,
    onMouseOut: NO_OP,
    onMouseDown: NO_OP,
  })
}

function usePreviewModeSelectAndHover(): {
  onMouseOver: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseOut: () => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  return useKeepShallowReferenceEquality({
    onMouseOver: NO_OP,
    onMouseOut: NO_OP,
    onMouseDown: NO_OP,
  })
}

export function useSelectAndHover(): {
  onMouseOver: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseOut: () => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const modeType = useEditorState((store) => store.editor.mode.type, 'useSelectAndHover mode')
  const selectModeCallbacks = useSelectModeSelectAndHover()
  const insertModeCallbacks = useInsertModeSelectAndHover()
  const previewModeCallbacks = usePreviewModeSelectAndHover()
  if (modeType === 'select') {
    return selectModeCallbacks
  } else if (modeType === 'insert') {
    return insertModeCallbacks
  } else if (modeType === 'live') {
    return previewModeCallbacks
  } else {
    throw new Error(`Unhandled editor mode ${modeType}`)
  }
}
