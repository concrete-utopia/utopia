import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapArrayToDictionary, mapDropNulls, uniqBy } from '../../../../core/shared/array-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  CanvasPoint,
  distance,
  isInfinityRectangle,
  point,
  windowPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as EP from '../../../../core/shared/element-path'
import { NO_OP, assertNever, fastForEach } from '../../../../core/shared/utils'
import type { KeysPressed } from '../../../../utils/keyboard'
import Keyboard, { isDigit } from '../../../../utils/keyboard'
import Utils from '../../../../utils/utils'
import {
  clearHighlightedViews,
  clearSelection,
  setFocusedElement,
  setHighlightedView,
  selectComponents,
  setHoveredView,
  clearHoveredViews,
} from '../../../editor/actions/action-creators'
import { cancelInsertModeActions } from '../../../editor/actions/meta-actions'
import type {
  EditorState,
  EditorStorePatched,
  LockedElements,
} from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { getSelectionOrValidTargetAtPoint, getValidTargetAtPoint } from '../../dom-lookup'
import { useWindowToCanvasCoordinates } from '../../dom-lookup-hooks'
import { useInsertModeSelectAndHover } from '../insert-mode/insert-mode-hooks'
import { WindowMousePositionRaw } from '../../../../utils/global-positions'
import type { InteractionSession } from '../../canvas-strategies/interaction-state'
import {
  boundingArea,
  createInteractionViaMouse,
  isDragToPan,
  KeyboardInteractionTimeout,
} from '../../canvas-strategies/interaction-state'
import { Modifier } from '../../../../utils/modifiers'
import { pathsEqual } from '../../../../core/shared/element-path'
import type { EditorAction } from '../../../../components/editor/action-types'
import { EditorDispatch } from '../../../../components/editor/action-types'
import { EditorModes, isInsertMode, isSelectModeWithArea } from '../../../editor/editor-modes'
import {
  scheduleTextEditForNextFrame,
  useTextEditModeSelectAndHover,
} from '../text-edit-mode/text-edit-mode-hooks'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { useSetAtom } from 'jotai'
import type { CanvasControlWithProps } from '../../../inspector/common/inspector-atoms'
import { InspectorHoveredCanvasControls } from '../../../inspector/common/inspector-atoms'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { getAllLockedElementPaths } from '../../../../core/shared/element-locking'
import { treatElementAsGroupLike } from '../../canvas-strategies/strategies/group-helpers'
import { useCommentModeSelectAndHover } from '../comment-mode/comment-mode-hooks'

const DRAG_START_THRESHOLD = 2

export function isDragInteractionActive(editorState: EditorState): boolean {
  return editorState.canvas.interactionSession?.interactionData.type === 'DRAG'
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
  maybeHoverOnHover: (target: ElementPath) => void
  maybeClearHoveredViewsOnHoverEnd: () => void
} {
  const dispatch = useDispatch()

  const stateRef = useRefEditorState((store) => {
    return {
      dragInteractionActive: isDragInteractionActive(store.editor),
      selectionEnabled: pickSelectionEnabled(store.editor.canvas, store.editor.keysPressed),
      highlightedViews: store.editor.highlightedViews,
      hoveredViews: store.editor.hoveredViews,
      mode: store.editor.mode,
    }
  })

  const maybeHighlightOnHover = React.useCallback(
    (target: ElementPath): void => {
      const { dragInteractionActive, selectionEnabled, highlightedViews } = stateRef.current

      const alreadyHighlighted = pathsEqual(target, highlightedViews?.[0])

      if (
        selectionEnabled &&
        !dragInteractionActive &&
        !alreadyHighlighted &&
        !isSelectModeWithArea(stateRef.current.mode)
      ) {
        dispatch([setHighlightedView(target)], 'canvas')
      }
    },
    [dispatch, stateRef],
  )

  const maybeClearHighlightsOnHoverEnd = React.useCallback((): void => {
    const { dragInteractionActive, selectionEnabled, highlightedViews } = stateRef.current

    if (
      selectionEnabled &&
      !dragInteractionActive &&
      highlightedViews.length > 0 &&
      !isSelectModeWithArea(stateRef.current.mode)
    ) {
      dispatch([clearHighlightedViews()], 'canvas')
    }
  }, [dispatch, stateRef])

  const maybeHoverOnHover = React.useCallback(
    (target: ElementPath): void => {
      const { dragInteractionActive, hoveredViews } = stateRef.current

      const alreadyHovered = pathsEqual(target, hoveredViews?.[0])

      if (!dragInteractionActive && !alreadyHovered) {
        dispatch([setHoveredView(target)], 'canvas')
      }
    },
    [dispatch, stateRef],
  )

  const maybeClearHoveredViewsOnHoverEnd = React.useCallback((): void => {
    const { dragInteractionActive, hoveredViews } = stateRef.current

    if (!dragInteractionActive && hoveredViews.length > 0) {
      dispatch([clearHoveredViews()], 'canvas')
    }
  }, [dispatch, stateRef])

  return {
    maybeHighlightOnHover: maybeHighlightOnHover,
    maybeClearHighlightsOnHoverEnd: maybeClearHighlightsOnHoverEnd,
    maybeHoverOnHover: maybeHoverOnHover,
    maybeClearHoveredViewsOnHoverEnd: maybeClearHoveredViewsOnHoverEnd,
  }
}

function filterNonSelectableElements(
  nonSelectablePaths: Array<ElementPath>,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  return paths.filter((path) =>
    nonSelectablePaths.every((nonSelectablePath) => !EP.pathsEqual(path, nonSelectablePath)),
  )
}

function isExcludedGroupAncestor(
  selectablePath: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
): boolean {
  return (
    treatElementAsGroupLike(componentMetadata, selectablePath) &&
    selectedViews.some((selectedView) => EP.isDescendantOf(selectedView, selectablePath))
  )
}

function replaceNonSelectablePaths(
  selectablePaths: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  lockedElements: Array<ElementPath>,
): Array<ElementPath> {
  let updatedSelectablePaths: Array<ElementPath> = []
  Utils.fastForEach(selectablePaths, (selectablePath) => {
    if (isExcludedGroupAncestor(selectablePath, componentMetadata, selectedViews)) {
      // exclude all group-like elements which are ancestors to the currently selected element
      return
    }
    if (selectedViews.some((selectedView) => EP.pathsEqual(selectablePath, selectedView))) {
      updatedSelectablePaths.push(selectablePath)
    } else {
      const isLocked = lockedElements.some((lockedPath) =>
        EP.pathsEqual(lockedPath, selectablePath),
      )

      const mustReplaceWithChildren = isLocked

      // If this element is locked we want to recurse the children
      if (mustReplaceWithChildren) {
        const childrenPaths = MetadataUtils.getImmediateChildrenPathsOrdered(
          componentMetadata,
          pathTrees,
          selectablePath,
        )
        const childrenPathsWithLockedPathsReplaced = replaceNonSelectablePaths(
          childrenPaths,
          componentMetadata,
          pathTrees,
          selectedViews,
          lockedElements,
        )

        updatedSelectablePaths.push(...childrenPathsWithLockedPathsReplaced)
      } else {
        updatedSelectablePaths.push(selectablePath)
      }
    }
  })

  return updatedSelectablePaths
}

export function getSelectableViews(
  componentMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  allElementsDirectlySelectable: boolean,
  childrenSelectable: boolean,
  lockedElements: LockedElements,
): ElementPath[] {
  const allLockedElementPaths = getAllLockedElementPaths(
    componentMetadata,
    elementPathTree,
    lockedElements,
  )
  const candidateSelectableViews = getCandidateSelectableViews(
    componentMetadata,
    elementPathTree,
    selectedViews,
    allElementsDirectlySelectable,
    childrenSelectable,
    allLockedElementPaths,
  )

  const selectableElements = filterNonSelectableElements(hiddenInstances, candidateSelectableViews)

  return selectableElements
}

function getCandidateSelectableViews(
  componentMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  allElementsDirectlySelectable: boolean,
  childrenSelectable: boolean,
  lockedElements: Array<ElementPath>,
): ElementPath[] {
  let unfilteredPaths: Array<ElementPath>
  if (allElementsDirectlySelectable) {
    unfilteredPaths = MetadataUtils.getAllPathsIncludingUnfurledFocusedComponents(
      componentMetadata,
      elementPathTree,
    )
  } else {
    const allRoots = MetadataUtils.getAllCanvasSelectablePathsOrdered(
      componentMetadata,
      elementPathTree,
    )
    const allAncestors = selectedViews.flatMap((path) =>
      EP.allPathsForLastPart(EP.parentPath(path)),
    )
    const allAncestorsWithAllSiblings = allAncestors.flatMap((path) =>
      MetadataUtils.getImmediateChildrenPathsOrdered(componentMetadata, elementPathTree, path),
    )
    const children = childrenSelectable
      ? selectedViews.flatMap((path) =>
          MetadataUtils.getImmediateChildrenPathsOrdered(componentMetadata, elementPathTree, path),
        )
      : []

    unfilteredPaths = [...allRoots, ...allAncestorsWithAllSiblings, ...children]
  }

  const selectableViews = replaceNonSelectablePaths(
    unfilteredPaths,
    componentMetadata,
    elementPathTree,
    selectedViews,
    lockedElements,
  )
  const uniqueSelectableViews = uniqBy<ElementPath>(selectableViews, EP.pathsEqual)

  return uniqueSelectableViews
}

export function useFindValidTarget(): (
  selectableViews: Array<ElementPath>,
  mousePoint: WindowPoint | null,
  preferAlreadySelected: 'prefer-selected' | 'dont-prefer-selected',
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
      elementPathTree: store.editor.elementPathTree,
      allElementProps: store.editor.allElementProps,
    }
  })

  return React.useCallback(
    (
      selectableViews: Array<ElementPath>,
      mousePoint: WindowPoint | null,
      preferAlreadySelected: 'prefer-selected' | 'dont-prefer-selected',
    ) => {
      const {
        selectedViews,
        componentMetadata,
        hiddenInstances,
        canvasScale,
        canvasOffset,
        elementPathTree,
        allElementProps,
      } = storeRef.current
      const validElementMouseOver: ElementPath | null =
        preferAlreadySelected === 'prefer-selected'
          ? getSelectionOrValidTargetAtPoint(
              componentMetadata,
              selectedViews,
              hiddenInstances,
              selectableViews,
              mousePoint,
              canvasScale,
              canvasOffset,
              elementPathTree,
              allElementProps,
            )
          : getValidTargetAtPoint(
              selectableViews,
              mousePoint,
              canvasScale,
              canvasOffset,
              componentMetadata,
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

export function useGetSelectableViewsForSelectMode() {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadata,
      elementPathTree: store.editor.elementPathTree,
      selectedViews: store.editor.selectedViews,
      hiddenInstances: store.editor.hiddenInstances,
      lockedElements: store.editor.lockedElements,
    }
  })

  return React.useCallback(
    (allElementsDirectlySelectable: boolean, childrenSelectable: boolean) => {
      const { componentMetadata, elementPathTree, selectedViews, hiddenInstances, lockedElements } =
        storeRef.current
      const selectableViews = getSelectableViews(
        componentMetadata,
        elementPathTree,
        selectedViews,
        hiddenInstances,
        allElementsDirectlySelectable,
        childrenSelectable,
        lockedElements,
      )
      return selectableViews
    },
    [storeRef],
  )
}

export function useGetSelectableViewsForSelectModeFromSelectedViews(
  selectedViews: Array<ElementPath>,
): (allElementsDirectlySelectable: boolean, childrenSelectable: boolean) => Array<ElementPath> {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadata,
      elementPathTree: store.editor.elementPathTree,
      hiddenInstances: store.editor.hiddenInstances,
      lockedElements: store.editor.lockedElements,
    }
  })

  return React.useCallback(
    (allElementsDirectlySelectable: boolean, childrenSelectable: boolean) => {
      const { componentMetadata, elementPathTree, hiddenInstances, lockedElements } =
        storeRef.current
      const selectableViews = getSelectableViews(
        componentMetadata,
        elementPathTree,
        selectedViews,
        hiddenInstances,
        allElementsDirectlySelectable,
        childrenSelectable,
        lockedElements,
      )
      return selectableViews
    },
    [selectedViews, storeRef],
  )
}

export function useCalculateHighlightedViews(
  allowHoverOnSelectedView: boolean,
  getHighlightableViews: (
    allElementsDirectlySelectable: boolean,
    childrenSelectable: boolean,
  ) => ElementPath[],
): (targetPoint: WindowPoint, eventCmdPressed: boolean) => void {
  const {
    maybeHighlightOnHover,
    maybeClearHighlightsOnHoverEnd,
    maybeHoverOnHover,
    maybeClearHoveredViewsOnHoverEnd,
  } = useMaybeHighlightElement()
  const findValidTarget = useFindValidTarget()
  return React.useCallback(
    (targetPoint: WindowPoint, eventCmdPressed: boolean) => {
      const selectableViews: Array<ElementPath> = getHighlightableViews(eventCmdPressed, false)
      const validElementPath = findValidTarget(selectableViews, targetPoint, 'dont-prefer-selected')
      const validElementPathForHover = findValidTarget(
        selectableViews,
        targetPoint,
        'prefer-selected',
      )

      if (validElementPathForHover == null) {
        maybeClearHoveredViewsOnHoverEnd()
      } else {
        maybeHoverOnHover(validElementPathForHover.elementPath)
      }

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
      getHighlightableViews,
      findValidTarget,
      allowHoverOnSelectedView,
      maybeClearHighlightsOnHoverEnd,
      maybeClearHoveredViewsOnHoverEnd,
      maybeHighlightOnHover,
      maybeHoverOnHover,
    ],
  )
}

export function useHighlightCallbacks(
  active: boolean,
  cmdPressed: boolean,
  getHighlightableViews: (
    allElementsDirectlySelectable: boolean,
    childrenSelectable: boolean,
  ) => ElementPath[],
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const calculateHighlightedViews = useCalculateHighlightedViews(true, getHighlightableViews)

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
    if (
      active &&
      WindowMousePositionRaw != null &&
      document.elementFromPoint != null // this is here so in case the editor is rendered in Jest we don't bother with figuring out highlighted views
    ) {
      // this useEffect will re-calculate (and update) the highlighted views if the user presses or releases 'cmd' without moving the mouse,
      // or if the user enters a new mode (the `active` flag will change for the modes), this is important when entering insert mode
      setTimeout(() => {
        calculateHighlightedViews(WindowMousePositionRaw!, cmdPressed)
      }, 0)
    }
  }, [calculateHighlightedViews, active, cmdPressed])

  return { onMouseMove }
}

function getPreferredSelectionForEvent(
  eventType: 'mousedown' | 'mouseup' | string,
  isDoubleClick: boolean,
): 'prefer-selected' | 'dont-prefer-selected' {
  // mousedown keeps selection on a single click to allow dragging overlapping elements and selection happens on mouseup
  // with continuous clicking mousedown should select
  switch (eventType) {
    case 'mousedown':
      return isDoubleClick ? 'dont-prefer-selected' : 'prefer-selected'
    case 'mouseup':
      return isDoubleClick ? 'prefer-selected' : 'dont-prefer-selected'
    default:
      return 'prefer-selected'
  }
}

export interface MouseCallbacks {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseUp: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
}

function useSelectOrLiveModeSelectAndHover(
  active: boolean,
  draggingAllowed: boolean,
  cmdPressed: boolean,
  setSelectedViewsForCanvasControlsOnly: (newSelectedViews: ElementPath[]) => void,
): MouseCallbacks {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const findValidTarget = useFindValidTarget()
  const getSelectableViewsForSelectMode = useGetSelectableViewsForSelectModeFromSelectedViews(
    selectedViewsRef.current,
  )
  const windowToCanvasCoordinates = useWindowToCanvasCoordinates()
  const interactionSessionHappened = React.useRef(false)
  const didWeHandleMouseDown = React.useRef(false) //  this is here to avoid selecting when closing text editing

  const { onMouseMove: innerOnMouseMove } = useHighlightCallbacks(
    active,
    cmdPressed,
    getSelectableViewsForSelectMode,
  )

  const editorStoreRef = useRefEditorState((store) => ({
    editor: store.editor,
    derived: store.derived,
  }))

  const onMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      // Do not handle the mouse move in the regular style if 'space' is pressed.
      const isDragIntention =
        isDragToPan(
          editorStoreRef.current.editor.canvas.interactionSession,
          editorStoreRef.current.editor.keysPressed['space'],
        ) || event.buttons === 4
      if (isDragIntention) {
        return
      }
      if (editorStoreRef.current.editor.canvas.interactionSession == null) {
        innerOnMouseMove(event)
      } else {
        if (!isMouseInteractionSession(editorStoreRef.current.editor.canvas.interactionSession)) {
          innerOnMouseMove(event)
        }
        // An interaction session has happened, which is important to know on mouseup
        interactionSessionHappened.current = true
      }
    },
    [innerOnMouseMove, editorStoreRef],
  )
  const mouseHandler = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const isLeftClick = event.button === 0
      const isRightClick = event.type === 'contextmenu' && event.detail === 0
      const isDragIntention =
        editorStoreRef.current.editor.keysPressed['space'] || event.button === 1
      const hasInteractionSessionWithMouseMoved =
        editorStoreRef.current.editor.canvas.interactionSession?.interactionData?.type === 'DRAG'
          ? editorStoreRef.current.editor.canvas.interactionSession?.interactionData?.drag != null
          : false
      const hasInteractionSession = editorStoreRef.current.editor.canvas.interactionSession != null
      const hadInteractionSessionThatWasCancelled =
        interactionSessionHappened.current && !hasInteractionSession

      const activeControl = editorStoreRef.current.editor.canvas.interactionSession?.activeControl
      const mouseUpSelectionAllowed =
        didWeHandleMouseDown.current &&
        !hadInteractionSessionThatWasCancelled &&
        (activeControl == null || activeControl.type === 'BOUNDING_AREA')

      if (event.type === 'mousedown') {
        didWeHandleMouseDown.current = true
      }
      if (event.type === 'mouseup') {
        // Clear the interaction session tracking flag
        interactionSessionHappened.current = false
        // didWeHandleMouseDown is used to avoid selecting when closing text editing
        didWeHandleMouseDown.current = false

        if (!mouseUpSelectionAllowed) {
          // We should skip this mouseup
          return
        }
      }

      if (
        isDragIntention ||
        hasInteractionSessionWithMouseMoved ||
        !active ||
        !(isLeftClick || isRightClick)
      ) {
        // Skip all of this handling if 'space' is pressed or a mousemove happened in an interaction, or the hook is not active
        return
      }

      const doubleClick = event.type === 'mousedown' && event.detail > 0 && event.detail % 2 === 0
      const selectableViews = getSelectableViewsForSelectMode(event.metaKey, doubleClick)
      const preferAlreadySelected = getPreferredSelectionForEvent(event.type, doubleClick)
      const foundTarget = findValidTarget(
        selectableViews,
        windowPoint(point(event.clientX, event.clientY)),
        preferAlreadySelected,
      )

      const isMultiselect = event.shiftKey
      const isDeselect = foundTarget == null && !isMultiselect
      let editorActions: Array<EditorAction> = []

      if (foundTarget != null || isDeselect) {
        if (foundTarget != null && draggingAllowed) {
          const start = windowToCanvasCoordinates(
            windowPoint(point(event.clientX, event.clientY)),
          ).canvasPositionRounded
          if (event.button !== 2 && event.type !== 'mouseup') {
            editorActions.push(
              CanvasActions.createInteractionSession(
                createInteractionViaMouse(
                  start,
                  Modifier.modifiersForEvent(event),
                  boundingArea(),
                  'zero-drag-not-permitted',
                ),
              ),
            )
          }
        }

        let updatedSelection: Array<ElementPath>
        if (isMultiselect) {
          updatedSelection = EP.addPathIfMissing(foundTarget!.elementPath, selectedViewsRef.current)
        } else {
          updatedSelection = foundTarget != null ? [foundTarget.elementPath] : []
        }

        const foundTargetIsSelected = foundTarget?.isSelected ?? false

        if (foundTarget != null && foundTargetIsSelected && doubleClick) {
          // for components without passed children doubleclicking enters focus mode
          const isFocusableLeaf = MetadataUtils.isManuallyFocusableLeafComponent(
            foundTarget.elementPath,
            editorStoreRef.current.editor.elementPathTree,
            editorStoreRef.current.editor.jsxMetadata,
            editorStoreRef.current.derived.autoFocusedPaths,
            editorStoreRef.current.derived.filePathMappings,
          )
          if (isFocusableLeaf) {
            editorActions.push(CanvasActions.clearInteractionSession(false))
            editorActions.push(setFocusedElement(foundTarget.elementPath))
          }

          const isEditableText = MetadataUtils.targetTextEditableAndHasText(
            editorStoreRef.current.editor.jsxMetadata,
            editorStoreRef.current.editor.elementPathTree,
            foundTarget.elementPath,
          )
          if (isEditableText) {
            editorActions.push(CanvasActions.clearInteractionSession(false))
            // We need to dispatch switching to text edit mode in the next frame, otherwise the mouse up blurs the text editor immediately
            scheduleTextEditForNextFrame(
              foundTarget.elementPath,
              { x: event.clientX, y: event.clientY },
              dispatch,
            )
          }
        }

        if (!foundTargetIsSelected) {
          // first we only set the selected views for the canvas controls
          setSelectedViewsForCanvasControlsOnly(updatedSelection)

          // In either case cancel insert mode.
          if (isInsertMode(editorStoreRef.current.editor.mode)) {
            editorActions.push(...cancelInsertModeActions('apply-changes'))
          }

          if (updatedSelection.length === 0) {
            // then we set the selected views for the editor state, 1 frame later
            editorActions.push(clearSelection(), setFocusedElement(null))
          } else {
            editorActions.push(selectComponents(updatedSelection, event.shiftKey))
          }
        }
      }
      dispatch(editorActions)
    },
    [
      dispatch,
      selectedViewsRef,
      findValidTarget,
      setSelectedViewsForCanvasControlsOnly,
      getSelectableViewsForSelectMode,
      editorStoreRef,
      draggingAllowed,
      windowToCanvasCoordinates,
      active,
    ],
  )

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => mouseHandler(event),
    [mouseHandler],
  )

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => mouseHandler(event),
    [mouseHandler],
  )

  return { onMouseMove, onMouseDown, onMouseUp }
}

export function useSelectAndHover(
  cmdPressed: boolean,
  setSelectedViewsForCanvasControlsOnly: (newSelectedViews: ElementPath[]) => void,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseUp: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'useSelectAndHover mode',
  )

  const modeType = mode.type
  const isZoomMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.keysPressed['z'] ?? false,
    'useSelectAndHover isZoomMode',
  )
  const hasInteractionSession = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession != null,
    'useSelectAndHover hasInteractionSession',
  )
  const selectModeCallbacks = useSelectOrLiveModeSelectAndHover(
    (modeType === 'select' || modeType === 'live') && !isZoomMode,
    (modeType === 'select' || modeType === 'live') && !isZoomMode,
    cmdPressed,
    setSelectedViewsForCanvasControlsOnly,
  )
  const insertModeCallbacks = useInsertModeSelectAndHover(modeType === 'insert', cmdPressed)
  const textEditModeCallbacks = useTextEditModeSelectAndHover(modeType === 'textEdit')
  const commentModeCallbacks = useCommentModeSelectAndHover(
    mode?.type === 'comment' ? mode.location : null,
  )

  if (hasInteractionSession) {
    return {
      onMouseMove: selectModeCallbacks.onMouseMove,
      onMouseDown: Utils.NO_OP,
      onMouseUp: selectModeCallbacks.onMouseUp,
    }
  } else {
    switch (modeType) {
      case 'select':
        return selectModeCallbacks
      case 'insert':
        return insertModeCallbacks
      case 'live':
        return selectModeCallbacks
      case 'textEdit':
        return textEditModeCallbacks
      case 'comment':
        return commentModeCallbacks
      default:
        const _exhaustiveCheck: never = modeType
        throw new Error(`Unhandled editor mode ${JSON.stringify(modeType)}`)
    }
  }
}

export function useClearKeyboardInteraction(editorStoreRef: {
  readonly current: EditorStorePatched
}): () => void {
  const dispatch = useDispatch()
  const keyboardTimeoutHandler = React.useRef<NodeJS.Timeout | null>(null)
  return React.useCallback(() => {
    if (keyboardTimeoutHandler.current != null) {
      clearTimeout(keyboardTimeoutHandler.current)
      keyboardTimeoutHandler.current = null
    }

    const clearKeyboardInteraction = () => {
      window.removeEventListener('mousedown', clearKeyboardInteraction, { capture: true })
      if (keyboardTimeoutHandler.current != null) {
        clearTimeout(keyboardTimeoutHandler.current)
        keyboardTimeoutHandler.current = null
      }
      if (
        editorStoreRef.current.editor.canvas.interactionSession?.interactionData.type === 'KEYBOARD'
      ) {
        dispatch([CanvasActions.clearInteractionSession(true)], 'everyone')
      }
    }

    keyboardTimeoutHandler.current = setTimeout(
      clearKeyboardInteraction,
      KeyboardInteractionTimeout,
    )

    window.addEventListener('mousedown', clearKeyboardInteraction, { once: true, capture: true })
  }, [dispatch, editorStoreRef])
}

export function useSetHoveredControlsHandlers<T>(): {
  onMouseEnter: (controls: Array<CanvasControlWithProps<T>>) => void
  onMouseLeave: () => void
} {
  const setHoveredCanvasControls = useSetAtom(InspectorHoveredCanvasControls)

  const onMouseEnter = React.useCallback(
    (controls: Array<CanvasControlWithProps<T>>) => {
      setHoveredCanvasControls(controls)
    },
    [setHoveredCanvasControls],
  )

  const onMouseLeave = React.useCallback(
    () => setHoveredCanvasControls([]),
    [setHoveredCanvasControls],
  )

  return { onMouseEnter, onMouseLeave }
}

function isMouseInteractionSession(interactionSession: InteractionSession): boolean {
  switch (interactionSession.interactionData.type) {
    case 'DRAG':
    case 'HOVER':
      return true
    case 'KEYBOARD':
      return false
    default:
      assertNever(interactionSession.interactionData)
  }
}
