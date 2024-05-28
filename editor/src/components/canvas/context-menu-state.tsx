import React from 'react'
import { atom, useAtom } from 'jotai'

export const ContextMenuStateAtom = atom<Array<string>>([])
export const ContextMenuOpen = atom((get) => {
  const contextMenusOpen = get(ContextMenuStateAtom)
  return contextMenusOpen.length > 0
})

export const useContextMenuState = () => {
  const [_values, setValues] = useAtom(ContextMenuStateAtom)

  const add = (id: string) => {
    setValues((prev) => {
      if (prev.includes(id)) {
        return prev
      }
      return [...prev, id]
    })
  }

  const remove = (id: string) => {
    setValues((prev) => {
      if (!prev.includes(id)) {
        return prev
      }

      const index = prev.findIndex((curId) => curId === id)
      return prev.slice(index, index)
    })
  }

  return {
    add,
    remove,
  }
}
