export function readParamFromUrl(
  param: string,
  removeFromUrl: 'remove-from-url' | 'keep-url-intact' = 'keep-url-intact',
): string | null {
  const url = new URL(window.location.href)
  const result = url.searchParams.get(param)
  if (removeFromUrl === 'remove-from-url') {
    url.searchParams.delete(param)
    window.history.replaceState({}, '', url.toString())
  }
  return result
}
