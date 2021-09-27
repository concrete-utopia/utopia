export const RootDir = `/utopia`

export function stripTrailingSlash(path: string): string {
  return path.endsWith('/') ? path.slice(0, -1) : path
}

export function stripLeadingSlash(path: string): string {
  return path.startsWith('/') ? path.slice(1) : path
}

export function appendToPath(path: string, elem: string): string {
  const left = stripTrailingSlash(path)
  const right = stripLeadingSlash(elem)
  return `${left}/${right}`
}

export function stripRootPrefix(path: string): string {
  return path.startsWith(RootDir) ? path.slice(RootDir.length + 1) : path
}

export function toUtopiaPath(projectID: string, path: string): string {
  const result = appendToPath(`${projectID}:/`, stripRootPrefix(path))
  return result
}

export function dirname(rawPath: string): string {
  const path = rtrim(rawPath, '/')
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

  /* eslint-disable-next-line no-constant-condition */
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
