import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { uniqBy } from '../../../../core/shared/array-utils'
import { JSXMetadata } from '../../../../core/shared/element-template'
import { ScenePath, TemplatePath } from '../../../../core/shared/project-file-types'
import * as TP from '../../../../core/shared/template-path'
import { KeysPressed } from '../../../../utils/keyboard'
import utils from '../../../../utils/utils'
import {
  clearHighlightedViews,
  clearSelection,
  selectComponents,
  setHighlightedView,
} from '../../../editor/actions/action-creators'
import { EditorState } from '../../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { DragState } from '../../canvas-types'
import { findFirstParentWithValidUID } from '../../dom-lookup'

export function pickIsResizing(dragState: DragState | null): boolean {
  return dragState != null && dragState.type === 'RESIZE_DRAG_STATE' && dragState.drag != null
}

export function pickIsDragging(dragState: DragState | null): boolean {
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
      isResizing: pickIsResizing(store.editor.canvas.dragState),
      isDragging: pickIsDragging(store.editor.canvas.dragState),
      selectionEnabled: pickSelectionEnabled(store.editor.canvas, store.editor.keysPressed),
    }
  })

  const maybeHighlightOnHover = React.useCallback(
    (target: TemplatePath): void => {
      const { dispatch, isDragging, isResizing, selectionEnabled } = stateRef.current
      if (selectionEnabled && !isDragging && !isResizing) {
        dispatch([setHighlightedView(target)], 'canvas')
      }
    },
    [stateRef],
  )

  const maybeClearHighlightsOnHoverEnd = React.useCallback((): void => {
    const { dispatch, isDragging, isResizing, selectionEnabled } = stateRef.current
    if (selectionEnabled && !isDragging && !isResizing) {
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
    utils.fastForEach(scenes, (path) => {
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
    utils.fastForEach(selectedViews, (view) => {
      utils.fastForEach(TP.allPaths(view), (ancestor) => {
        const ancestorChildren = MetadataUtils.getImmediateChildren(componentMetadata, ancestor)

        siblings.push(...ancestorChildren.map((child) => child.templatePath))
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
      allElementsDirectlySelectable: store.editor.keysPressed.cmd ?? false,
    }
  })

  return React.useCallback(
    (targetHtmlElement: HTMLElement) => {
      const {
        componentMetadata,
        selectedViews,
        hiddenInstances,
        allElementsDirectlySelectable,
      } = storeRef.current
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

export function useSelectAndHover(): {
  onMouseOver: (event: React.MouseEvent<HTMLDivElement>) => void
  onMouseOut: () => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement>) => void
} {
  const dispatch = useEditorState((store) => store.dispatch, 'useSelectAndHover dispatch')
  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
  const findValidTarget = useFindValidTarget()

  const onMouseOver = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const validTemplatePath = findValidTarget(event.target as HTMLDivElement)
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
      const foundTarget = findValidTarget(event.target as HTMLDivElement)
      if (foundTarget != null) {
        if (!foundTarget.isSelected) {
          const doubleClick = event.detail > 1 // we interpret a triple click as two double clicks, a quadruple click as three double clicks, etc  // TODO TEST ME
          if (foundTarget.selectionMode === 'singleclick' || doubleClick) {
            dispatch([selectComponents([foundTarget.templatePath], false)])
          }
        }
      }
    },
    [dispatch, findValidTarget],
  )

  // TODO if mouse moves, enter drag mode

  return { onMouseOver, onMouseOut, onMouseDown }
}
