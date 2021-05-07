export function normalizePath(path: Array<string>): Array<string> {
  return path.reduce((pathSoFar: Array<string>, pathElem: string, index: number) => {
    if (pathElem === '') {
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

export function getNearestAncestorDirectory(filepath: string, isDirectory: boolean): string {
  if (isDirectory) {
    return filepath
  } else {
    return makePathFromParts(getPartsFromPath(filepath).slice(0, -1))
  }
}

export function makePathFromParts(parts: Array<string>): string {
  return `/${parts.join('/')}`
}

export function getPartsFromPath(path: string): Array<string> {
  return stripLeadingSlash(path).split('/')
}

export function absolutePathFromRelativePath(
  origin: string,
  originIsDir: boolean,
  relativePath: string,
): string {
  if (relativePath.startsWith('/')) {
    // Not actually relative in this case.
    return relativePath
  } else {
    let originDir: string
    if (originIsDir) {
      originDir = origin
    } else {
      originDir = makePathFromParts(getPartsFromPath(origin).slice(0, -1))
    }
    const joinedPath = appendToPath(originDir, relativePath)
    const normalized = normalizePath(joinedPath.split('/'))
    return makePathFromParts(normalized)
  }
}
