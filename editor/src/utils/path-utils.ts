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

// TODO!!! this should be replaced with a more comprehensive module resolution, a generic version of `resolveModule`.
// The reason is that a user might import something like `src/folder/index.js` as `import { ComponentFromIndex } from "./folder"` and rely on JS module resolution to find index.js for them

export function absolutePathFromRelativePath(origin: string, relativePath: string): string {
  if (relativePath.startsWith('/')) {
    // Not actually relative in this case.
    return relativePath
  } else {
    const joinedPath = appendToPath(origin, relativePath)
    // TODO this is not calling normalizePath, but it seems like it should
    const normalized = joinedPath.split('/')
    return `/${normalized.join('/')}`
  }
}
