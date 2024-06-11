import { v4 as UUID } from 'uuid'

export const GridMenuWidth = 268
export const GridMenuMinWidth = 200
export const GridMenuMaxWidth = 500

export const GridPaneWidth = 500
export const GridPaneMinWidth = 100

export const NumberOfColumns = 4
export const IndexOfCanvas = 2

export const GridPanelVerticalGapHalf = 6
export const GridVerticalExtraPadding = -4
export const GridPanelHorizontalGapHalf = 6
export const GridHorizontalExtraPadding = -4

export const ExtraHorizontalDropTargetPadding = 45

export const ResizeColumnWidth = 10

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
  visible: boolean
}

export type PanelName = Menu | Pane

export interface StoredPanel {
  name: PanelName
  type: 'menu' | 'pane'
  uid: string
}

export function storedPanel({
  name,
  type,
}: {
  name: PanelName
  type: 'menu' | 'pane'
}): StoredPanel {
  return {
    name: name,
    type: type,
    uid: UUID(),
  }
}

export interface StoredColumn {
  panels: Array<StoredPanel>
  paneWidth: number // the width to use if they only contain Panes
  menuWidth: number // the width to use if they contain at least one Menu
}

export function storedColumn(panels: Array<StoredPanel>): StoredColumn {
  return { panels: panels, paneWidth: GridPaneWidth, menuWidth: GridMenuWidth }
}

export type StoredLayout = Array<StoredColumn>

export type BeforeColumn = {
  type: 'before-column'
  columnIndex: number
}
export type AfterColumn = {
  type: 'after-column'
  columnIndex: number
}
export type ColumnUpdate = BeforeColumn | AfterColumn

export type BeforeIndex = {
  type: 'before-index'
  columnIndex: number
  indexInColumn: number
}
export type AfterIndex = {
  type: 'after-index'
  columnIndex: number
  indexInColumn: number
}
export type RowUpdate = BeforeIndex | AfterIndex
export type LayoutUpdate = ColumnUpdate | RowUpdate

export function gridMenuDefaultPanels(): StoredLayout {
  return [
    storedColumn([
      storedPanel({ name: 'navigator', type: 'menu' }),
      storedPanel({ name: 'code-editor', type: 'pane' }),
    ]),
    storedColumn([]),
    storedColumn([]),
    storedColumn([storedPanel({ name: 'inspector', type: 'menu' })]),
  ]
}
