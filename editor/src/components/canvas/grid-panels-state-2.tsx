import type { StoredPanel } from './grid-panels'

export type Menu = 'inspector' | 'navigator'
export type Pane = 'code-editor'

export const allMenusAndPanels: Array<Menu | Pane> = [
  'navigator',
  'code-editor',
  'inspector',
  // 'preview', // Does this exist?
]

export interface PanelData {
  panel: StoredPanel
  span: number
  index: number
  order: number
}

type ColumnData = Array<PanelData>

interface FloatingPanelData {}
