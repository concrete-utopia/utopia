export const Scheme = 'utopia'
export const RootDir = `/${Scheme}`
export const URIPrefix = `${Scheme}:/`

export function appendToPath(path: string, elem: string): string {
  const left = path.endsWith('/') ? path.slice(0, -1) : path
  const right = elem.startsWith('/') ? elem.slice(1) : elem
  return `${left}/${right}`
}

export function stripRootPrefix(path: string): string {
  return path.startsWith(RootDir) ? path.slice(RootDir.length + 1) : path
}

export function toUtopiaPath(path: string): string {
  const result = appendToPath(URIPrefix, stripRootPrefix(path))
  return result
}

export function dirname(path: string): string {
  path = rtrim(path, '/')
  if (!path) {
    return '/'
  }

  return path.substr(0, path.lastIndexOf('/'))
}

export function rtrim(haystack: string, needle: string): string {
  if (!haystack || !needle) {
    return haystack
  }

  const needleLen = needle.length,
    haystackLen = haystack.length

  if (needleLen === 0 || haystackLen === 0) {
    return haystack
  }

  let offset = haystackLen,
    idx = -1

  while (true) {
    idx = haystack.lastIndexOf(needle, offset - 1)
    if (idx === -1 || idx + needleLen !== offset) {
      break
    }
    if (idx === 0) {
      return ''
    }
    offset = idx
  }

  return haystack.substring(0, offset)
}
