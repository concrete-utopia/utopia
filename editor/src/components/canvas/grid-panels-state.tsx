import immutableUpdate from 'immutability-helper'
import { atom, useAtom, useSetAtom } from 'jotai'
import findLastIndex from 'lodash.findlastindex'
import React from 'react'
import { accumulate, insert, removeAll, removeIndexFromArray } from '../../core/shared/array-utils'
import { clamp, mod } from '../../core/shared/math-utils'
import { assertNever } from '../../core/shared/utils'
import { deepFreeze } from '../../utils/deep-freeze'
import { Substores, useEditorState } from '../editor/store/store-hook'
import type {
  GridPanelData,
  LayoutUpdate,
  PanelName,
  StoredColumn,
  StoredLayout,
  StoredPanel,
} from './stored-layout'
import {
  GridMenuMaxWidth,
  GridMenuMinWidth,
  GridPaneMinWidth,
  GridPanelsNumberOfRows,
  IndexOfCanvas,
  NumberOfColumns,
  gridMenuDefaultPanels,
  storedColumn,
  storedPanel,
} from './stored-layout'
import {
  loadUserPreferences,
  getProjectStoredLayoutOrDefault,
  saveUserPreferencesProjectLayout,
} from '../common/user-preferences'

export const GridPanelsStateAtom = atom(gridMenuDefaultPanels())

export function useGridPanelState() {
  const [loaded, setLoaded] = React.useState(false)
  const stateAtom = useAtom(GridPanelsStateAtom)
  const [state, setState] = stateAtom

  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'GridPanelsContainer projectId',
  )

  React.useEffect(() => {
    if (projectId == null || loaded) {
      return
    }
    setLoaded(true)
    async function loadPrefs(id: string) {
      const prefs = await loadUserPreferences()
      setState(getProjectStoredLayoutOrDefault(prefs.panelsLayout, id))
    }
    void loadPrefs(projectId)
  }, [loaded, setState, projectId])

  React.useEffect(() => {
    if (projectId == null || !loaded) {
      return
    }
    void saveUserPreferencesProjectLayout(projectId, state)
  }, [loaded, state, projectId])

  return stateAtom
}

function useVisibleGridPanels() {
  const [panelState] = useGridPanelState()

  const { codeEditorVisible, navigatorVisible, inspectorVisible } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        codeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
        navigatorVisible: store.editor.leftMenu.visible,
        inspectorVisible: store.editor.rightMenu.visible,
      }
    },
    'storedLayoutToResolvedPanels panel visibility',
  )

  const visiblePanels: StoredLayout = panelState.map((column) => ({
    ...column,
    panels: column.panels.filter((panel) => {
      switch (panel.name) {
        case 'code-editor':
          return codeEditorVisible
        case 'navigator':
          return navigatorVisible
        case 'inspector':
          return inspectorVisible
        default:
          assertNever(panel.name)
      }
      throw new Error('never should run')
    }),
  }))

  return visiblePanels
}

export function useResolvedGridPanels() {
  const [panelState] = useGridPanelState()
  const visiblePanels = useVisibleGridPanels()

  return React.useMemo(() => {
    const panels = accumulate<{ [index in PanelName]: GridPanelData }>({} as any, (acc) => {
      panelState.forEach((column, colIndex) => {
        const visiblePanelsForColumn = visiblePanels[colIndex].panels
        column.panels.forEach((panel, panelIndex) => {
          acc[panel.name] = {
            panel: panel,
            span: GridPanelsNumberOfRows / visiblePanelsForColumn.length, // TODO introduce resize function
            index: colIndex,
            order: panelIndex,
            visible: visiblePanelsForColumn.findIndex((p) => p.name === panel.name) > -1,
          }
        })
      })
    })

    return panels
  }, [panelState, visiblePanels])
}

/**
 * Returns the index in the wraparound annotation, currently the values are -2, -1, 0, 1
 */
export function wrapAroundColIndex(index: number): number {
  const normalized = normalizeColIndex(index)
  if (normalized >= IndexOfCanvas) {
    return normalized - NumberOfColumns
  } else {
    return normalized
  }
}

/**
 * Normalizes the index to 0,1,2,3
 */
export function normalizeColIndex(index: number): number {
  return mod(index, NumberOfColumns)
}

export function updateLayout(
  stored: StoredLayout,
  paneToMove: StoredPanel,
  update: LayoutUpdate,
): StoredLayout {
  const panelToInsert = storedPanel(paneToMove)

  function insertPanel(layout: StoredLayout) {
    deepFreeze(layout)

    if (update.type === 'before-column' || update.type === 'after-column') {
      const atLeastOneEmptyColumn = layout.some((col) => col.panels.length === 0)
      if (!atLeastOneEmptyColumn) {
        // the user wants to create a new column and fill it with the moved Panel.
        // if there's zero empty columns, it means we cannot create a new column, so we must bail out

        return layout // BAIL OUT! TODO we should show a Toast
      }
      const newColumn: StoredColumn = storedColumn([panelToInsert])

      const normalizedIndex = normalizeColIndex(update.columnIndex)

      const indexInArray = update.type === 'before-column' ? normalizedIndex : normalizedIndex + 1

      const rightHandSide = normalizedIndex >= IndexOfCanvas

      const withElementInserted = insert(indexInArray, newColumn, layout)
      const withOldPanelRemoved = removeOldPanel(withElementInserted)

      const indexOfFirstEmptyColumn = rightHandSide
        ? findLastIndex(withOldPanelRemoved, (col) => col.panels.length === 0)
        : withOldPanelRemoved.findIndex((col) => col.panels.length === 0)

      return removeIndexFromArray(indexOfFirstEmptyColumn, withOldPanelRemoved)
    }
    if (update.type === 'before-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = {
        ...working[update.columnIndex],
        panels: insert(update.indexInColumn, panelToInsert, working[update.columnIndex].panels),
      }

      return removeOldPanel(working)
    }
    if (update.type === 'after-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = {
        ...working[update.columnIndex],
        panels: insert(update.indexInColumn + 1, panelToInsert, working[update.columnIndex].panels),
      }

      return removeOldPanel(working)
    }

    assertNever(update)
  }

  function removeOldPanel(layout: StoredLayout): StoredLayout {
    return layout.map((column) => {
      return {
        ...column,
        panels: removeAll(column.panels, [paneToMove], (l, r) => l.uid === r.uid),
      }
    })
  }

  function floatColumnsTowardsEdges(layout: StoredLayout) {
    const leftSide = layout.slice(0, IndexOfCanvas)
    const rightSideReversed = layout.slice(IndexOfCanvas).reverse()
    const leftSideFixed = accumulate(
      new Array<StoredColumn>(leftSide.length).fill(storedColumn([])),
      (acc) => {
        let indexInAccumulator = 0
        leftSide.forEach((column) => {
          if (column.panels.length > 0) {
            acc[indexInAccumulator] = column
            indexInAccumulator++
          }
        })
      },
    )
    const rightSideFixed = accumulate(
      new Array<StoredColumn>(rightSideReversed.length).fill(storedColumn([])),
      (acc) => {
        let indexInAccumulator = rightSideReversed.length - 1
        rightSideReversed.forEach((column) => {
          if (column.panels.length > 0) {
            acc[indexInAccumulator] = column
            indexInAccumulator--
          }
        })
      },
    )
    return [...leftSideFixed, ...rightSideFixed]
  }

  const withPanelInserted = insertPanel(stored)
  const withEmptyColumnsInMiddle = floatColumnsTowardsEdges(withPanelInserted)

  // TODO we need to fix the sizes too!
  return withEmptyColumnsInMiddle
}

export function useUpdateGridPanelLayout(): (panelName: PanelName, update: LayoutUpdate) => void {
  const setStoredState = useSetAtom(GridPanelsStateAtom)

  return React.useCallback(
    (panelName: PanelName, update: LayoutUpdate) => {
      setStoredState((stored) => {
        const paneToMove: StoredPanel = (() => {
          for (const column of stored) {
            for (const panel of column.panels) {
              if (panel.name === panelName) {
                return panel
              }
            }
          }
          throw new Error(`Invariant error: we should have found a panel by now: '${panelName}'`)
        })()
        return updateLayout(stored, paneToMove, update)
      })
    },
    [setStoredState],
  )
}

export function useUpdateGridPanelLayoutPutCodeEditorBelowNavigator(): () => void {
  const setStoredState = useSetAtom(GridPanelsStateAtom)

  return React.useCallback(() => {
    setStoredState((stored) => {
      const codeEditorPane: StoredPanel = (() => {
        for (const column of stored) {
          for (const panel of column.panels) {
            if (panel.name === 'code-editor') {
              return panel
            }
          }
        }
        throw new Error('Invariant error: we should have found a code-editor panel by now')
      })()
      const update: LayoutUpdate = (() => {
        for (let columnIndex = 0; columnIndex < stored.length; columnIndex++) {
          const column = stored[columnIndex]
          for (let indexInColumn = 0; indexInColumn < column.panels.length; indexInColumn++) {
            const panel = column.panels[indexInColumn]
            if (panel.name === 'navigator') {
              return {
                type: 'after-index',
                columnIndex: columnIndex,
                indexInColumn: indexInColumn,
              }
            }
          }
        }
        throw new Error('Invariant error: we should have found a navigator panel by now')
      })()
      return updateLayout(stored, codeEditorPane, update)
    })
  }, [setStoredState])
}

export function useColumnWidths(): [
  Array<number>,
  (columnIndex: number, newWidth: number) => void,
] {
  const [panelState, setPanelState] = useAtom(GridPanelsStateAtom)
  const visiblePanels = useVisibleGridPanels()

  // start with the default value
  const columnWidths: Array<number> = React.useMemo(
    () =>
      visiblePanels.map((_, index) => {
        return getColumnWidth(visiblePanels, index)
      }),
    [visiblePanels],
  )

  const setColumnWidths = React.useCallback(
    (columnIndexIncoming: number, newWidth: number) => {
      setPanelState((current) => {
        const columnIndex = normalizeColIndex(columnIndexIncoming)
        const columnContainsMenu = current[columnIndex].panels.some((p) => p.type === 'menu')
        if (columnContainsMenu) {
          // menu resize – clamped!
          return immutableUpdate(current, {
            [columnIndex]: {
              menuWidth: { $set: clamp(GridMenuMinWidth, GridMenuMaxWidth, newWidth) },
            },
          })
        } else {
          // pane resize!
          return immutableUpdate(current, {
            [columnIndex]: { paneWidth: { $set: Math.max(newWidth, GridPaneMinWidth) } },
          })
        }
      })
    },
    [setPanelState],
  )

  return [columnWidths, setColumnWidths]
}

export function getColumnWidth(panelState: StoredLayout, columnIndex: number): number {
  const column = panelState[normalizeColIndex(columnIndex)]
  if (column.panels.length === 0) {
    // empty columns are zero width
    return 0
  }
  const width = column.panels.some((p) => p.type === 'menu') ? column.menuWidth : column.paneWidth
  return width
}
