export function normalizePath(path: Array<string>): Array<string> {
  return path.reduce((pathSoFar: Array<string>, pathElem: string, index: number) => {
    if (pathElem === '' && index !== 0) {
      return pathSoFar
    }
    if (pathElem === '.') {
      return pathSoFar
    }
    if (pathElem === '..') {
      return pathSoFar.slice(0, -1)
    }
    return [...pathSoFar, pathElem]
  }, [])
}

export function stripTrailingSlash(path: string): string {
  return path.endsWith('/') ? path.slice(0, -1) : path
}

export function stripLeadingSlash(path: string): string {
  return path.startsWith('/') ? path.slice(1) : path
}

export function appendToPath(firstPart: string, secondPart: string): string {
  const left = stripTrailingSlash(firstPart)
  const right = stripLeadingSlash(secondPart)
  return `${left}/${right}`
}

export function absolutePathFromRelativePath(origin: string, relativePath: string): string {
  if (relativePath.startsWith('/')) {
    // Not actually relative in this case.
    return relativePath
  } else {
    const joinedPath = appendToPath(origin, relativePath)
    const normalized = joinedPath.split('/')
    return `/${normalized.join('/')}`
  }
}
