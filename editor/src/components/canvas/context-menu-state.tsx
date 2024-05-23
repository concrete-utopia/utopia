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
  const [isContextMenuOpen] = useAtom(ContextMenuOpen)

  React.useEffect(() => {
    if (isContextMenuOpen) {
      document.body.classList.add(BodyMenuOpenClass)
    } else {
      document.body.classList.remove(BodyMenuOpenClass)
    }
  }, [isContextMenuOpen])

  const add = (id: string) => {
    if (values.includes(id)) return
    setValues((prev) => [...prev, id])
  }

  const remove = (id: string) => {
    if (!values.includes(id)) return
    setValues((prev) => {
      const index = prev.findIndex((curId) => curId === id)
      return prev.slice(index, index)
    })
  }

  return {
    add,
    remove,
  }
}
