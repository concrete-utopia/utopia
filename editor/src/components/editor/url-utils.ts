export function getParamFromUrl(key: string, remove?: boolean): string | null {
  const urlParams = new URLSearchParams(window.location.search)
  const value = urlParams.get(key)
  if (remove) {
    urlParams.delete(key)
    const searchQuery = urlParams.toString() == '' ? '' : `?${urlParams.toString()}`
    window.history.replaceState({}, '', `${window.location.pathname}${searchQuery}`)
  }
  return value
}
