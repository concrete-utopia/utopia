import type { StoredPanel } from './floating-panels'

export type Menu = 'inspector' | 'navigator'
export type Pane = 'code-editor' | 'preview'

export const allMenusAndPanels: Array<Menu | Pane> = [
  'navigator',
  'code-editor',
  'inspector',
  'preview',
]

export interface PanelData {
  panel: StoredPanel
  span: number
  index: number
  order: number
}

type ColumnData = Array<PanelData>

interface FloatingPanelData {}
