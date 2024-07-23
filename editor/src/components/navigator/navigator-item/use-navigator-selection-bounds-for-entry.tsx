import React from 'react'
import * as EP from '../../../core/shared/element-path'
import type { NavigatorEntry } from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { isRegulaNavigatorRow } from '../navigator-row'
import { createSelector } from 'reselect'
import { navigatorTargetsSelector } from '../navigator-utils'

const navigatorRowsPathsSelector = createSelector(navigatorTargetsSelector, (targets) => {
  return targets.navigatorRows.map((row) =>
    isRegulaNavigatorRow(row) ? row.entry.elementPath : row.entries[0].elementPath,
  )
})

export function useNavigatorSelectionBoundsForEntry(
  navigatorEntry: NavigatorEntry,
  selected: boolean,
  childComponentCount: number,
): {
  isTopOfSelection: boolean
  isBottomOfSelection: boolean
} {
  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'useNavigatorSelectionBoundsCheck selectedViews',
  )

  const navigatorRowsPaths = useEditorState(
    Substores.navigatorTargetsSubstate,
    navigatorRowsPathsSelector,
    'useNavigatorSelectionBoundsCheck navigatorRowsPaths',
  )

  const collapsedViews = useEditorState(
    Substores.navigator,
    (store) => {
      return store.editor.navigator.collapsedViews
    },
    'useNavigatorSelectionBoundsCheck collapsedViews',
  )

  return React.useMemo(() => {
    const index = navigatorRowsPaths.findIndex((view) =>
      EP.pathsEqual(view, navigatorEntry.elementPath),
    )

    const previous = index > 0 ? navigatorRowsPaths.at(index - 1) : null
    const next = index < navigatorRowsPaths.length - 1 ? navigatorRowsPaths.at(index + 1) : null

    const isDangling =
      childComponentCount === 0 || collapsedViews.includes(navigatorEntry.elementPath)

    const isTopOfSelection =
      previous == null ||
      (!selectedViews.some(
        (view) =>
          EP.pathsEqual(view, previous) ||
          EP.isDescendantOf(EP.parentPath(navigatorEntry.elementPath), view) ||
          EP.isDescendantOf(previous, view),
      ) &&
        selected)

    const isBottomOfSelection =
      next == null ||
      ((!selected || isDangling) &&
        !selectedViews.some((view) => EP.pathsEqual(view, next) || EP.isDescendantOf(next, view)))

    return {
      isTopOfSelection: isTopOfSelection,
      isBottomOfSelection: isBottomOfSelection,
    }
  }, [
    navigatorRowsPaths,
    navigatorEntry,
    selectedViews,
    selected,
    childComponentCount,
    collapsedViews,
  ])
}
