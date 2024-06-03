export type InsertMenuMode = 'insert' | 'swap' | 'wrap'

export const insertMenuModes: {
  all: InsertMenuMode[]
  onlyInsert: InsertMenuMode[]
  onlyConvert: InsertMenuMode[]
  onlyWrap: InsertMenuMode[]
} = {
  all: ['insert', 'swap', 'wrap'],
  onlyInsert: ['insert'],
  onlyConvert: ['swap'],
  onlyWrap: ['wrap'],
}
