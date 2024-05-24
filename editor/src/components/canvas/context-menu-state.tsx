import { atom, useAtom } from 'jotai'

import { BodyMenuOpenClass } from '../../core/shared/utils'
import React from 'react'

export const ContextMenuStateAtom = atom([] as string[])
export const ContextMenuOpen = atom((get) => {
  const contextMenusOpen = get(ContextMenuStateAtom)
  return contextMenusOpen.length > 0
})

export const useContextMenuState = () => {
  const [values, setValues] = useAtom(ContextMenuStateAtom)

  React.useEffect(() => {
    if (values.length > 0) {
      document.body.classList.add(BodyMenuOpenClass)
    } else {
      document.body.classList.remove(BodyMenuOpenClass)
    }
  }, [values])

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
