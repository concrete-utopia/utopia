export function readParamFromUrl(param: string, removeFromUrl: boolean = false): string | null {
  const url = new URL(window.location.href)
  const result = url.searchParams.get(param)
  if (removeFromUrl) {
    url.searchParams.delete(param)
    window.history.replaceState({}, '', url.toString())
  }
  return result
}
