export function readParamFromUrl(param: string): string | null {
  const url = new URL(window.location.href)
  const result = url.searchParams.get(param)
  url.searchParams.delete(param)
  window.history.replaceState({}, '', url.toString())
  return result
}
