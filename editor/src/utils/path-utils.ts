export function normalizePath(path: Array<string>): Array<string> {
  let result: Array<string> = []
  for (const elem of path) {
    switch (elem) {
      case '':
      case '.':
        break
      case '..':
        result.pop()
        break
      default:
        result.push(elem)
    }
  }
  return result
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

export function getParentDirectory(filepath: string): string {
  return makePathFromParts(getPartsFromPath(filepath).slice(0, -1))
}

type SubPathCache = { [key: string]: PathCache }

interface PathCache {
  cachedString: string
  subPathCache: SubPathCache
}

function pathCache(cachedString: string, subPathCache: SubPathCache): PathCache {
  return {
    cachedString: cachedString,
    subPathCache: subPathCache,
  }
}

let rootPathCache: SubPathCache = {}

function getPathFromCache(parts: Array<string>): string {
  let workingSubCache: SubPathCache = rootPathCache
  let workingPathCache: PathCache | null = null
  let partsSoFar: Array<string> = []
  for (const part of parts) {
    partsSoFar.push(part)
    if (part in workingSubCache) {
      // The `in` check above proves this does not return `undefined`.
      workingPathCache = workingSubCache[part]!
      workingSubCache = workingPathCache.subPathCache
    } else {
      const cachedString = `/${partsSoFar.join('/')}`
      const newPathCache = pathCache(cachedString, {})
      workingPathCache = newPathCache
      workingSubCache[part] = newPathCache
      workingSubCache = newPathCache.subPathCache
    }
  }
  return workingPathCache == null ? '/' : workingPathCache.cachedString
}

export function makePathFromParts(parts: Array<string>): string {
  return getPathFromCache(parts)
}

export function getPartsFromPath(path: string): Array<string> {
  return stripLeadingSlash(path).split('/')
}

export function absolutePathFromRelativePath(
  origin: string,
  originIsDir: boolean,
  relativePath: string,
): string {
  if (!relativePath.startsWith('.')) {
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
