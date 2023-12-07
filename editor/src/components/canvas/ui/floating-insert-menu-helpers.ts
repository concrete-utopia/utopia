import type { FloatingInsertMenuState } from '../../editor/store/editor-state'

export type InsertMenuMode = FloatingInsertMenuState['insertMenuMode']

export const insertMenuModes: {
  all: InsertMenuMode[]
  onlyClosed: InsertMenuMode[]
  onlyInsert: InsertMenuMode[]
  onlyConvert: InsertMenuMode[]
  onlyWrap: InsertMenuMode[]
} = {
  all: ['closed', 'insert', 'swap', 'wrap'],
  onlyClosed: ['closed'],
  onlyInsert: ['insert'],
  onlyConvert: ['swap'],
  onlyWrap: ['wrap'],
}
