import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { last, uniqBy } from '../../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  distance,
  point,
  WindowPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import * as EP from '../../../../core/shared/element-path'
import { fastForEach, NO_OP } from '../../../../core/shared/utils'
import { KeysPressed } from '../../../../utils/keyboard'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import Utils from '../../../../utils/utils'
import {
  clearHighlightedViews,
  clearSelection,
  selectComponents,
  setFocusedElement,
  setHighlightedView,
} from '../../../editor/actions/action-creators'
import { EditorState } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { DragState, moveDragState } from '../../canvas-types'
import { createDuplicationNewUIDs, getOriginalCanvasFrames } from '../../canvas-utils'
import {
  findFirstParentWithValidElementPath,
  getAllTargetsAtPoint,
  getValidTargetAtPoint,
} from '../../dom-lookup'
import { useWindowToCanvasCoordinates } from '../../dom-lookup-hooks'
import { useInsertModeSelectAndHover } from './insert-mode-hooks'
import { WindowMousePositionRaw } from '../../../../utils/global-positions'

const DRAG_START_TRESHOLD = 2

export function isResizing(dragState: DragState | null): boolean {
  return dragState != null && dragState.type === 'RESIZE_DRAG_STATE' && dragState.drag != null
}

export function isDragging(dragState: DragState | null): boolean {
  return dragState != null && dragState.type === 'MOVE_DRAG_STATE' && dragState.drag != null
}

export function isInserting(dragState: DragState | null): boolean {
  return dragState != null && dragState.type === 'INSERT_DRAG_STATE' && dragState.drag != null
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
  maybeHighlightOnHover: (target: ElementPath) => void
  maybeClearHighlightsOnHoverEnd: () => void
} {
  const stateRef = useRefEditorState((store) => {
    return {
      dispatch: store.dispatch,
      resizing: isResizing(store.editor.canvas.dragState),
      dragging: isDragging(store.editor.canvas.dragState),
      selectionEnabled: pickSelectionEnabled(store.editor.canvas, store.editor.keysPressed),
      inserting: isInserting(store.editor.canvas.dragState),
    }
  })

  const maybeHighlightOnHover = React.useCallback(
    (target: ElementPath): void => {
      const { dispatch, dragging, resizing, selectionEnabled, inserting } = stateRef.current
      if (selectionEnabled && !dragging && !resizing && !inserting) {
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
  hiddenInstances: Array<ElementPath>,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  return paths.filter((path) => hiddenInstances.every((hidden) => !EP.pathsEqual(path, hidden)))
}

export function getSelectableViews(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  allElementsDirectlySelectable: boolean,
  childrenSelectable: boolean,
): ElementPath[] {
  let candidateViews: Array<ElementPath>

  if (allElementsDirectlySelectable) {
    candidateViews = MetadataUtils.getAllPathsIncludingUnfurledFocusedComponents(componentMetadata)
  } else {
    const scenes = MetadataUtils.getAllStoryboardChildrenPaths(componentMetadata)
    let rootElementsToFilter: ElementPath[] = []
    let dynamicScenesWithFragmentRootViews: ElementPath[] = []
    Utils.fastForEach(scenes, (path) => {
      const scene = MetadataUtils.findElementByElementPath(componentMetadata, path)
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
      return !rootElementsToFilter.some((path) => EP.pathsEqual(rootPath, path))
    })
    let siblings: Array<ElementPath> = []
    Utils.fastForEach(selectedViews, (view) => {
      const allPaths = childrenSelectable
        ? EP.allPathsForLastPart(view)
        : EP.allPathsForLastPart(EP.parentPath(view))
      Utils.fastForEach(allPaths, (ancestor) => {
        const {
          children,
          unfurledComponents,
        } = MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(
          ancestor,
          componentMetadata,
        )
        const ancestorChildren = [...children, ...unfurledComponents]
        fastForEach(ancestorChildren, (child) => siblings.push(child))
      })
    })

    const selectableViews = [...dynamicScenesWithFragmentRootViews, ...allRoots, ...siblings]
    const uniqueSelectableViews = uniqBy<ElementPath>(selectableViews, EP.pathsEqual)

    const selectableViewsFiltered = uniqueSelectableViews.filter((view) => {
      // I kept the group-like behavior here that the user can't single-click select the parent group, even though it is a view now
      const isGroup = MetadataUtils.isAutoSizingViewFromComponents(componentMetadata, view)
      const isAncestorOfSelected = selectedViews.some((selectedView) =>
        EP.isDescendantOf(selectedView, view),
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
  selectableViews: Array<ElementPath>,
  mousePoint: WindowPoint | null,
) => {
  elementPath: ElementPath
  isSelected: boolean
} | null {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadata,
      selectedViews: store.editor.selectedViews,
      hiddenInstances: store.editor.hiddenInstances,
      canvasScale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.realCanvasOffset,
      focusedElementPath: store.editor.focusedElementPath,
    }
  })

  return React.useCallback(
    (selectableViews: Array<ElementPath>, mousePoint: WindowPoint | null) => {
      const {
        selectedViews,
        componentMetadata,
        hiddenInstances,
        canvasScale,
        canvasOffset,
      } = storeRef.current
      const validElementMouseOver: ElementPath | null = getValidTargetAtPoint(
        componentMetadata,
        selectedViews,
        hiddenInstances,
        selectableViews,
        mousePoint,
        canvasScale,
        canvasOffset,
      )
      const validElementPath: ElementPath | null =
        validElementMouseOver != null ? validElementMouseOver : null
      if (validElementPath != null) {
        const isSelected = selectedViews.some((selectedView) =>
          EP.pathsEqual(validElementPath, selectedView),
        )
        return {
          elementPath: validElementPath,
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
  target: ElementPath,
  start: CanvasPoint | null,
) => (event: MouseEvent) => void {
  const dispatch = useEditorState((store) => store.dispatch, 'useStartDragState dispatch')
  const entireEditorStoreRef = useRefEditorState((store) => store)

  return React.useCallback(
    (target: ElementPath, start: CanvasPoint | null) => (event: MouseEvent) => {
      if (start == null) {
        return
      }

      const componentMetadata = entireEditorStoreRef.current.editor.jsxMetadata
      const selectedViews = entireEditorStoreRef.current.editor.selectedViews

      const duplicate = event.altKey
      const duplicateNewUIDs = duplicate
        ? createDuplicationNewUIDs(
            selectedViews,
            componentMetadata,
            entireEditorStoreRef.current.editor.projectContents,
          )
        : null

      const isTargetSelected = selectedViews.some((sv) => EP.pathsEqual(sv, target))

      const moveTargets =
        isTargetSelected && EP.areAllElementsInSameInstance(selectedViews)
          ? selectedViews
          : [target]

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
  foundTarget: ElementPath,
) => void {
  const startDragState = useStartDragState()
  const windowToCanvasCoordinates = useWindowToCanvasCoordinates()

  const startDragStateAfterDragExceedsThreshold = React.useCallback(
    (nativeEvent: MouseEvent, foundTarget: ElementPath) => {
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
      componentMetadata: store.editor.jsxMetadata,
      selectedViews: store.editor.selectedViews,
      hiddenInstances: store.editor.hiddenInstances,
      focusedElementPath: store.editor.focusedElementPath,
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
  active: boolean,
  cmdPressed: boolean,
  allowHoverOnSelectedView: boolean,
  getHighlightableViews: (
    allElementsDirectlySelectable: boolean,
    childrenSelectable: boolean,
  ) => ElementPath[],
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
  const findValidTarget = useFindValidTarget()

  const calculateHighlightedViews = React.useCallback(
    (targetPoint: WindowPoint, eventCmdPressed: boolean) => {
      const selectableViews: Array<ElementPath> = getHighlightableViews(eventCmdPressed, false)
      const validElementPath = findValidTarget(selectableViews, targetPoint)
      if (
        validElementPath == null ||
        (!allowHoverOnSelectedView && validElementPath.isSelected) // we remove highlights if the hovered element is selected
      ) {
        maybeClearHighlightsOnHoverEnd()
      } else {
        maybeHighlightOnHover(validElementPath.elementPath)
      }
    },
    [
      allowHoverOnSelectedView,
      maybeClearHighlightsOnHoverEnd,
      maybeHighlightOnHover,
      getHighlightableViews,
      findValidTarget,
    ],
  )

  const onMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      return calculateHighlightedViews(
        windowPoint(point(event.clientX, event.clientY)),
        event.metaKey,
      )
    },
    [calculateHighlightedViews],
  )

  React.useEffect(() => {
    if (active && WindowMousePositionRaw != null) {
      // this useEffect will re-calculate (and update) the highlighted views if the user presses or releases 'cmd' without moving the mouse,
      // or if the user enters a new mode (the `active` flag will change for the modes), this is important when entering insert mode
      calculateHighlightedViews(WindowMousePositionRaw, cmdPressed)
    }
  }, [calculateHighlightedViews, active, cmdPressed])

  return { onMouseMove }
}

function useSelectOrLiveModeSelectAndHover(
  active: boolean,
  draggingAllowed: boolean,
  cmdPressed: boolean,
  setSelectedViewsForCanvasControlsOnly: (newSelectedViews: ElementPath[]) => void,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const dispatch = useEditorState((store) => store.dispatch, 'useSelectAndHover dispatch')
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const findValidTarget = useFindValidTarget()
  const getSelectableViewsForSelectMode = useGetSelectableViewsForSelectMode()
  const startDragStateAfterDragExceedsThreshold = useStartDragStateAfterDragExceedsThreshold()

  const { onMouseMove } = useHighlightCallbacks(
    active,
    cmdPressed,
    false,
    getSelectableViewsForSelectMode,
  )

  const editorStoreRef = useRefEditorState((store) => ({
    editor: store.editor,
    derived: store.derived,
  }))

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
        if (foundTarget != null && draggingAllowed) {
          startDragStateAfterDragExceedsThreshold(event.nativeEvent, foundTarget.elementPath)
        }

        let updatedSelection: Array<ElementPath>
        if (isMultiselect) {
          updatedSelection = EP.addPathIfMissing(foundTarget!.elementPath, selectedViewsRef.current)
        } else {
          updatedSelection = foundTarget != null ? [foundTarget.elementPath] : []
        }

        if (foundTarget != null && doubleClick) {
          // for components without passed children doubleclicking enters focus mode
          const isFocusableLeaf = MetadataUtils.isFocusableLeafComponent(
            foundTarget.elementPath,
            editorStoreRef.current.editor.jsxMetadata,
          )
          if (isFocusableLeaf) {
            dispatch([setFocusedElement(foundTarget.elementPath)])
          }
        }

        if (!(foundTarget?.isSelected ?? false)) {
          // first we only set the selected views for the canvas controls
          setSelectedViewsForCanvasControlsOnly(updatedSelection)

          requestAnimationFrame(() => {
            requestAnimationFrame(() => {
              // then we set the selected views for the editor state, 1 frame later
              if (updatedSelection.length === 0) {
                dispatch([clearSelection(), setFocusedElement(null)])
              } else {
                dispatch([selectComponents(updatedSelection, event.shiftKey)])
              }
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
      editorStoreRef,
      draggingAllowed,
    ],
  )

  return { onMouseMove, onMouseDown }
}

export function useSelectAndHover(
  cmdPressed: boolean,
  setSelectedViewsForCanvasControlsOnly: (newSelectedViews: ElementPath[]) => void,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const modeType = useEditorState((store) => store.editor.mode.type, 'useSelectAndHover mode')
  const selectModeCallbacks = useSelectOrLiveModeSelectAndHover(
    modeType === 'select' || modeType === 'select-lite' || modeType === 'live',
    modeType === 'select' || modeType === 'live',
    cmdPressed,
    setSelectedViewsForCanvasControlsOnly,
  )
  const insertModeCallbacks = useInsertModeSelectAndHover(modeType === 'insert', cmdPressed)

  switch (modeType) {
    case 'select':
      return selectModeCallbacks
    case 'select-lite':
      return selectModeCallbacks
    case 'insert':
      return insertModeCallbacks
    case 'live':
      return selectModeCallbacks
    default:
      const _exhaustiveCheck: never = modeType
      throw new Error(`Unhandled editor mode ${JSON.stringify(modeType)}`)
  }
}
