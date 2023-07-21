export type InsertMenuMode = 'closed' | 'insert' | 'convert' | 'wrap'

export const insertMenuModes: {
  all: InsertMenuMode[]
  onlyClosed: InsertMenuMode[]
  onlyInsert: InsertMenuMode[]
  onlyConvert: InsertMenuMode[]
  onlyWrap: InsertMenuMode[]
} = {
  all: ['closed', 'insert', 'convert', 'wrap'],
  onlyClosed: ['closed'],
  onlyInsert: ['insert'],
  onlyConvert: ['convert'],
  onlyWrap: ['wrap'],
}
