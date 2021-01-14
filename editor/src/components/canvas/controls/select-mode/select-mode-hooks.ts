import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { last, uniqBy } from '../../../../core/shared/array-utils'
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
import {
  findFirstParentWithValidUID,
  getAllTargetsAtPoint,
  getValidTargetAtPoint,
} from '../../dom-lookup'
import { useWindowToCanvasCoordinates } from '../../dom-lookup-hooks'
import { selectElementsThatRespectLayout } from '../new-canvas-controls'
import { useInsertModeSelectAndHover } from './insert-mode-hooks'

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
  childrenSelectable: boolean,
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
      const allPaths = childrenSelectable ? TP.allPaths(view) : TP.allPaths(TP.parentPath(view))
      Utils.fastForEach(allPaths, (ancestor) => {
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
  selectableViews: Array<TemplatePath>,
  mousePoint: WindowPoint | null,
) => {
  templatePath: TemplatePath
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
    (selectableViews: Array<TemplatePath>, mousePoint: WindowPoint | null) => {
      const { selectedViews } = storeRef.current
      const validElementMouseOver: TemplatePath | null = getValidTargetAtPoint(
        selectableViews.map(TP.toString),
        mousePoint,
      )
      const validTemplatePath: TemplatePath | null =
        validElementMouseOver != null ? validElementMouseOver : null
      if (validTemplatePath != null) {
        const isSelected = selectedViews.some((selectedView) =>
          TP.pathsEqual(validTemplatePath, selectedView),
        )
        return {
          templatePath: validTemplatePath,
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

      const isTargetSelected = selectedViews.some((sv) => TP.pathsEqual(sv, target))
      const selection =
        isTargetSelected && TP.areAllElementsInSameScene(selectedViews) ? selectedViews : [target]
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

export function useStartDragStateAfterDragExceedsThreshold(): (
  nativeEvent: MouseEvent,
  foundTarget: TemplatePath,
) => void {
  const startDragState = useStartDragState()
  const windowToCanvasCoordinates = useWindowToCanvasCoordinates()

  const startDragStateAfterDragExceedsThreshold = React.useCallback(
    (nativeEvent: MouseEvent, foundTarget: TemplatePath) => {
      callbackAfterDragExceedsThreshold(
        nativeEvent,
        DRAG_START_TRESHOLD,
        startDragState(
          foundTarget,
          windowToCanvasCoordinates(windowPoint(point(nativeEvent.clientX, nativeEvent.clientY)))
            .canvasPositionRounded,
        ),
      )
    },
    [startDragState, windowToCanvasCoordinates],
  )

  return startDragStateAfterDragExceedsThreshold
}

function useGetSelectableViewsForSelectMode() {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadataKILLME,
      selectedViews: store.editor.selectedViews,
      hiddenInstances: store.editor.hiddenInstances,
    }
  })

  return React.useCallback(
    (allElementsDirectlySelectable: boolean, childrenSelectable: boolean) => {
      const { componentMetadata, selectedViews, hiddenInstances } = storeRef.current
      const selectableViews = getSelectableViews(
        componentMetadata,
        selectedViews,
        hiddenInstances,
        allElementsDirectlySelectable,
        childrenSelectable,
      )
      return selectableViews
    },
    [storeRef],
  )
}

export function useHighlightCallbacks(
  getHighlightableViews: (
    allElementsDirectlySelectable: boolean,
    childrenSelectable: boolean,
  ) => TemplatePath[],
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
  const findValidTarget = useFindValidTarget()
  const previousTargetUnderPoint = React.useRef<TemplatePath | null>(null)

  const onMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const selectableViews: Array<TemplatePath> = getHighlightableViews(event.metaKey, false)
      const validTemplatePath = findValidTarget(
        selectableViews,
        windowPoint(point(event.clientX, event.clientY)),
      )
      if (validTemplatePath == null) {
        maybeClearHighlightsOnHoverEnd()
      } else if (!TP.pathsEqual(validTemplatePath.templatePath, previousTargetUnderPoint.current)) {
        if (!validTemplatePath.isSelected) {
          // do not highlight selected view
          maybeHighlightOnHover(validTemplatePath.templatePath)
        }
      }
      previousTargetUnderPoint.current = validTemplatePath?.templatePath ?? null
    },
    [
      maybeClearHighlightsOnHoverEnd,
      maybeHighlightOnHover,
      getHighlightableViews,
      findValidTarget,
      previousTargetUnderPoint,
    ],
  )

  return { onMouseMove }
}

export function useSelectModeSelectAndHover(
  setSelectedViewsForCanvasControlsOnly: (newSelectedViews: TemplatePath[]) => void,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const dispatch = useEditorState((store) => store.dispatch, 'useSelectAndHover dispatch')
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const findValidTarget = useFindValidTarget()
  const getSelectableViewsForSelectMode = useGetSelectableViewsForSelectMode()
  const startDragStateAfterDragExceedsThreshold = useStartDragStateAfterDragExceedsThreshold()

  const { onMouseMove } = useHighlightCallbacks(getSelectableViewsForSelectMode)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const doubleClick = event.detail > 1 // we interpret a triple click as two double clicks, a quadruple click as three double clicks, etc  // TODO TEST ME
      const selectableViews = getSelectableViewsForSelectMode(event.metaKey, doubleClick)
      const foundTarget = findValidTarget(
        selectableViews,
        windowPoint(point(event.clientX, event.clientY)),
      )

      const isMultiselect = event.shiftKey
      const isDeselect = foundTarget == null && !isMultiselect

      if (foundTarget != null || isDeselect) {
        if (foundTarget != null) {
          startDragStateAfterDragExceedsThreshold(event.nativeEvent, foundTarget.templatePath)
        }

        let updatedSelection: Array<TemplatePath>
        if (isMultiselect) {
          updatedSelection = TP.addPathIfMissing(
            foundTarget!.templatePath,
            selectedViewsRef.current,
          )
        } else {
          updatedSelection = foundTarget != null ? [foundTarget.templatePath] : []
        }

        if (!(foundTarget?.isSelected ?? false)) {
          // first we only set the selected views for the canvas controls
          setSelectedViewsForCanvasControlsOnly(updatedSelection)

          requestAnimationFrame(() => {
            requestAnimationFrame(() => {
              // then we set the selected views for the editor state, 1 frame later
              dispatch([selectComponents(updatedSelection, event.shiftKey)])
            })
          })
        }
      }
    },
    [
      dispatch,
      selectedViewsRef,
      findValidTarget,
      startDragStateAfterDragExceedsThreshold,
      setSelectedViewsForCanvasControlsOnly,
      getSelectableViewsForSelectMode,
    ],
  )

  return { onMouseMove, onMouseDown }
}

function usePreviewModeSelectAndHover(): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  return useKeepShallowReferenceEquality({
    onMouseMove: NO_OP,
    onMouseDown: NO_OP,
  })
}

export function useSelectAndHover(
  setSelectedViewsForCanvasControlsOnly: (newSelectedViews: TemplatePath[]) => void,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const modeType = useEditorState((store) => store.editor.mode.type, 'useSelectAndHover mode')
  const selectModeCallbacks = useSelectModeSelectAndHover(setSelectedViewsForCanvasControlsOnly)
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
