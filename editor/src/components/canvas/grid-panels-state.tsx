import findLastIndex from 'lodash.findlastindex'
import { v4 as UUID } from 'uuid'
import { accumulate, insert, removeAll, removeIndexFromArray } from '../../core/shared/array-utils'
import { mod } from '../../core/shared/math-utils'
import { assertNever } from '../../core/shared/utils'

export const GridMenuWidth = 260
export const GridPaneWidth = 500

export const NumberOfColumns = 4
export const IndexOfCanvas = 2

export const GridPanelVerticalGapHalf = 6
export const GridPanelHorizontalGapHalf = 6

export const ExtraHorizontalDropTargetPadding = 15

export const GridPanelsNumberOfRows = 12

export type Menu = 'inspector' | 'navigator'
export type Pane = 'code-editor'

export const allMenusAndPanels: Array<Menu | Pane> = [
  'navigator',
  'code-editor',
  'inspector',
  // 'preview', // Does this exist?
]

export interface GridPanelData {
  panel: StoredPanel
  span: number
  index: number
  order: number
}

export type PanelName = Menu | Pane

export interface StoredPanel {
  name: PanelName
  type: 'menu' | 'pane'
  uid: string
}

function storedPanel({ name, type }: { name: PanelName; type: 'menu' | 'pane' }): StoredPanel {
  return {
    name: name,
    type: type,
    uid: UUID(),
  }
}

type StoredLayout = Array<Array<StoredPanel>>

type BeforeColumn = {
  type: 'before-column'
  columnIndex: number
}
type AfterColumn = {
  type: 'after-column'
  columnIndex: number
}
type ColumnUpdate = BeforeColumn | AfterColumn

type BeforeIndex = {
  type: 'before-index'
  columnIndex: number
  indexInColumn: number
}
type AfterIndex = {
  type: 'after-index'
  columnIndex: number
  indexInColumn: number
}
type RowUpdate = BeforeIndex | AfterIndex
export type LayoutUpdate = ColumnUpdate | RowUpdate

export const GridMenuDefaultPanels: StoredLayout = [
  [storedPanel({ name: 'code-editor', type: 'pane' })],
  [storedPanel({ name: 'navigator', type: 'menu' })],
  [],
  [storedPanel({ name: 'inspector', type: 'menu' })],
]

export function storedLayoutToResolvedPanels(stored: StoredLayout): {
  [index in PanelName]: GridPanelData
} {
  const panels = accumulate<{ [index in PanelName]: GridPanelData }>({} as any, (acc) => {
    stored.forEach((column, colIndex) => {
      const panelsForColumn = column.length
      column.forEach((panel, panelIndex) => {
        acc[panel.name] = {
          panel: panel,
          span: GridPanelsNumberOfRows / panelsForColumn, // TODO introduce resize function
          index: colIndex,
          order: panelIndex,
        }
      })
    })
  })

  return panels
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
function normalizeColIndex(index: number): number {
  return mod(index, NumberOfColumns)
}

export function updateLayout(
  stored: StoredLayout,
  paneToMove: StoredPanel, // must be referentially equal to the stored panel!
  update: LayoutUpdate,
): StoredLayout {
  const panelToInsert = storedPanel(paneToMove)

  function insertPanel(layout: StoredLayout) {
    if (update.type === 'before-column' || update.type === 'after-column') {
      const atLeastOneEmptyColumn = layout.some((col) => col.length === 0)
      if (!atLeastOneEmptyColumn) {
        // the user wants to create a new column and fill it with the moved Panel.
        // if there's zero empty columns, it means we cannot create a new column, so we must bail out

        return layout // BAIL OUT! TODO we should show a Toast
      }
      const newColumn: Array<StoredPanel> = [panelToInsert]

      const normalizedIndex = normalizeColIndex(update.columnIndex)

      const indexInArray = update.type === 'before-column' ? normalizedIndex : normalizedIndex + 1

      const rightHandSide = normalizedIndex >= IndexOfCanvas

      const withElementInserted = insert(indexInArray, newColumn, layout)
      const withOldPanelRemoved = removeOldPanel(withElementInserted)

      const indexOfFirstEmptyColumn = rightHandSide
        ? findLastIndex(withOldPanelRemoved, (col) => col.length === 0)
        : withOldPanelRemoved.findIndex((col) => col.length === 0)

      return removeIndexFromArray(indexOfFirstEmptyColumn, withOldPanelRemoved)
    }
    if (update.type === 'before-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = insert(
        update.indexInColumn,
        panelToInsert,
        working[update.columnIndex],
      )

      return removeOldPanel(working)
    }
    if (update.type === 'after-index') {
      const working = [...layout]

      // insert
      working[update.columnIndex] = insert(
        update.indexInColumn + 1,
        panelToInsert,
        working[update.columnIndex],
      )

      return removeOldPanel(working)
    }

    assertNever(update)
  }

  function removeOldPanel(layout: StoredLayout) {
    return layout.map((column) => {
      return removeAll(column, [paneToMove], (l, r) => l.uid === r.uid)
    })
  }

  function floatColumnsTowardsEdges(layout: StoredLayout) {
    const leftSide = layout.slice(0, IndexOfCanvas)
    const rightSideReversed = layout.slice(IndexOfCanvas).reverse()
    const leftSideFixed = accumulate(new Array(leftSide.length).fill([]), (acc) => {
      let iterator = 0
      leftSide.forEach((column) => {
        if (column.length > 0) {
          acc[iterator] = column
          iterator++
        }
      })
    })
    const rightSideFixed = accumulate(new Array(rightSideReversed.length).fill([]), (acc) => {
      let iterator = rightSideReversed.length - 1
      rightSideReversed.forEach((column) => {
        if (column.length > 0) {
          acc[iterator] = column
          iterator--
        }
      })
    })
    return [...leftSideFixed, ...rightSideFixed]
  }

  const withPanelInserted = insertPanel(stored)
  const withEmptyColumnsInMiddle = floatColumnsTowardsEdges(withPanelInserted)

  // TODO we need to fix the sizes too!
  return withEmptyColumnsInMiddle
}
