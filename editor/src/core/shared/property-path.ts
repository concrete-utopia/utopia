import { PRODUCTION_ENV } from '../../common/env-vars'
import type { PropertyPath, PropertyPathPart } from './project-file-types'
import { arrayEqualsByReference, fastForEach, longestCommonArray } from './utils'

export function fromString(value: string): PropertyPath {
  let fromPathStringCache: PropertyPath | null = globalPathStringToPathCache[value]
  if (fromPathStringCache == null) {
    const result = createFromArray(value.split('.'))
    globalPathStringToPathCache[value] = result
    return result
  } else {
    return fromPathStringCache
  }
}

interface PropertyPathCache {
  cached: PropertyPath | null
  cachedToString: string | null
  stringChildCaches: { [key: string]: PropertyPathCache }
  numberChildCaches: { [key: number]: PropertyPathCache }
}

function emptyPathCache(): PropertyPathCache {
  return {
    cached: null,
    cachedToString: null,
    stringChildCaches: {},
    numberChildCaches: {},
  }
}

let globalPathStringToPathCache: { [key: string]: PropertyPath } = {}
let globalPathCache: PropertyPathCache = emptyPathCache()

export function clearPropertyPathCache() {
  globalPathStringToPathCache = {}
  globalPathCache = emptyPathCache()
}

function getPathCache(elements: Array<PropertyPathPart>): PropertyPathCache {
  let workingPathCache: PropertyPathCache = globalPathCache
  for (const pathPart of elements) {
    // Create a distinction between keys of `0` and `'0'`,
    // as indexing into an object coerces the key to a string.
    if (typeof pathPart === 'number') {
      if (pathPart in workingPathCache.numberChildCaches) {
        workingPathCache = workingPathCache.numberChildCaches[pathPart]
      } else {
        const newCache = emptyPathCache()
        workingPathCache.numberChildCaches[pathPart] = newCache
        workingPathCache = newCache
      }
    } else {
      if (pathPart in workingPathCache.stringChildCaches) {
        workingPathCache = workingPathCache.stringChildCaches[pathPart]
      } else {
        const newCache = emptyPathCache()
        workingPathCache.stringChildCaches[pathPart] = newCache
        workingPathCache = newCache
      }
    }
  }
  return workingPathCache
}

export function create<T1 extends PropertyPathPart>(element1: T1): PropertyPath<[T1]>
export function create<T1 extends PropertyPathPart, T2 extends PropertyPathPart>(
  element1: T1,
  element2: T2,
): PropertyPath<[T1, T2]>
export function create<
  T1 extends PropertyPathPart,
  T2 extends PropertyPathPart,
  T3 extends PropertyPathPart,
>(element1: T1, element2: T2, element3: T3): PropertyPath<[T1, T2, T3]>
export function create(...elements: Array<PropertyPathPart>): PropertyPath<Array<PropertyPathPart>>
export function create(
  ...elements: Array<PropertyPathPart>
): PropertyPath<Array<PropertyPathPart>> {
  const pathCache = getPathCache(elements)
  if (pathCache.cached == null) {
    const newPath = { propertyElements: elements }
    pathCache.cached = newPath
    return newPath
  } else {
    return pathCache.cached
  }
}

export function createFromArray<T extends Array<PropertyPathPart>>(elements: T): PropertyPath<T> {
  const pathCache = getPathCache(elements)
  if (pathCache.cached == null) {
    const newPath = { propertyElements: elements }
    pathCache.cached = newPath
    return newPath
  } else {
    return pathCache.cached as PropertyPath<T>
  }
}

export function toString(propertyPath: PropertyPath): string {
  const joinWith = '.'
  const pathCache = getPathCache(propertyPath.propertyElements)
  if (pathCache.cachedToString == null) {
    let result: string = ''
    const elementsLength = propertyPath.propertyElements.length
    fastForEach(propertyPath.propertyElements, (elem, index) => {
      result += elem
      if (index < elementsLength - 1) {
        result += joinWith
      }
    })
    pathCache.cachedToString = result
    return result
  } else {
    return pathCache.cachedToString
  }
}

export function lastPart(propertyPath: PropertyPath): PropertyPathPart {
  return propertyPath.propertyElements[propertyPath.propertyElements.length - 1]
}

export function lastPartToString(propertyPath: PropertyPath): string {
  return `${lastPart(propertyPath)}`
}

export function firstPart(propertyPath: PropertyPath): PropertyPathPart {
  return propertyPath.propertyElements[0]
}

export function firstPartToString(propertyPath: PropertyPath): string {
  return `${firstPart(propertyPath)}`
}

export function tail(propertyPath: PropertyPath): PropertyPath {
  const newElements =
    propertyPath.propertyElements.length > 0 ? propertyPath.propertyElements.slice(1) : []
  return createFromArray(newElements)
}

export function getElements(propertyPath: PropertyPath): Array<PropertyPathPart> {
  return propertyPath.propertyElements
}

export function appendPropertyPathElems(
  path: PropertyPath,
  elems: Array<PropertyPathPart>,
): PropertyPath {
  return createFromArray(path.propertyElements.concat(elems))
}

export function prependPropertyPathElems(
  elems: Array<PropertyPathPart>,
  path: PropertyPath,
): PropertyPath {
  return createFromArray(elems.concat(path.propertyElements))
}

export function append(first: PropertyPath, second: PropertyPath): PropertyPath {
  return appendPropertyPathElems(first, second.propertyElements)
}

export function sameOrSubPath(compareTo: PropertyPath, possibleSubPath: PropertyPath): boolean {
  for (let i = 0; i < compareTo.propertyElements.length; i++) {
    if (compareTo.propertyElements[i] !== possibleSubPath.propertyElements[i]) {
      return false
    }
  }
  return true
}

export function pathsEqual(l: PropertyPath, r: PropertyPath): boolean {
  return arrayEqualsByReference(l.propertyElements, r.propertyElements)
}

export function contains(paths: Array<PropertyPath>, path: PropertyPath): boolean {
  return paths.some((p) => pathsEqual(p, path))
}

export function containsAll(l: PropertyPath, elements: Array<PropertyPathPart>): boolean {
  const propertyElements = getElements(l)
  return elements.every(
    (element: PropertyPathPart): boolean => propertyElements.indexOf(element) > -1,
  )
}

export const EventHandlersPathElement = 'eventHandlers'

export function getEventHandlerName(path: PropertyPath): string | null {
  if (path.propertyElements.length === 2) {
    if (path.propertyElements[0] === EventHandlersPathElement) {
      return path.propertyElements[1].toString()
    }
  }
  return null
}

export function isSameProperty(path: PropertyPath, stringPath: string): boolean {
  return toString(path) === stringPath
}

export function rootPath(path: PropertyPath): PropertyPath {
  return create(getElements(path)[0])
}

export function stepDownPath(
  pathToStep: PropertyPath | null,
  pathToFollow: PropertyPath,
): PropertyPath {
  if (!PRODUCTION_ENV) {
    if (pathToStep != null) {
      if (!sameOrSubPath(pathToStep, pathToFollow)) {
        throw new Error(`${toString(pathToFollow)} isn't a sub path of ${toString(pathToStep)}.`)
      }
    }
  }
  if (pathToStep == null) {
    return rootPath(pathToFollow)
  } else {
    return createFromArray(
      pathToFollow.propertyElements.slice(0, pathToStep.propertyElements.length + 1),
    )
  }
}

export function depth(path: PropertyPath): number {
  return path.propertyElements.length
}

function findLongestMatchingSubPath(
  pathArray: Array<PropertyPathPart>,
  from: Array<PropertyPathPart>,
): Array<PropertyPathPart> {
  return longestCommonArray(
    pathArray,
    from,
    (first: PropertyPathPart, second: PropertyPathPart) => first === second,
  )
}

export function findLongestMatchingPropertyPath(
  path: PropertyPath,
  from: Array<PropertyPathPart>,
): PropertyPath {
  return createFromArray(findLongestMatchingSubPath(path.propertyElements, from))
}
