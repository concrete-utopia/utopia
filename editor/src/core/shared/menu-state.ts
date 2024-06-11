const openMenuIds = new Set<string>()

export const addOpenMenuId = (id: string) => {
  openMenuIds.add(id)
}

export const removeOpenMenuId = (id: string) => {
  openMenuIds.delete(id)
}

export const isMenuOpen = (id: string) => {
  return openMenuIds.has(id)
}

export const isSomeMenuOpen = () => {
  return openMenuIds.size > 0
}
