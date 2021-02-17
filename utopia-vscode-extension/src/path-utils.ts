import { Uri } from 'vscode'

export const Scheme = 'utopia'
export const RootDir = `/${Scheme}`
export const URIPrefix = `${Scheme}:/`

export function appendToPath(path: string, elem: string): string {
  const joinChar = path.endsWith('/') ? '' : '/'
  return `${path}${joinChar}${elem}`
}

export function stripRootPrefix(path: string): string {
  return path == null ? '' : path.slice(RootDir.length + 1)
}

export function toUtopiaPath(path: string): string {
  const result = `${URIPrefix}${stripRootPrefix(path)}`
  return result
}

export function toUtopiaURI(path: string): Uri {
  return Uri.parse(toUtopiaPath(path))
}

export function fromUtopiaURI(uri: Uri): string {
  return `${RootDir}/${uri.path}`
}

// Copied from VS Code's source

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
