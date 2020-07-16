import { PropertyPath, PropertyPathPart } from './project-file-types'
import { arrayEquals, fastForEach, longestCommonArray } from './utils'
import { PRODUCTION_ENV } from './detect-env'

export function fromString(value: string): PropertyPath {
  let fromPathStringCache: PropertyPath | null = globalPathStringToPathCache[value]
  if (fromPathStringCache == null) {
    const result = create(value.split('.'))
    globalPathStringToPathCache[value] = result
    return result
  } else {
    return fromPathStringCache
  }
}

interface PropertyPathCache {
  cached: PropertyPath | null
  cachedToString: string | null
  childCaches: { [key: string]: PropertyPathCache }
}

function emptyPathCache(): PropertyPathCache {
  return {
    cached: null,
    cachedToString: null,
    childCaches: {},
  }
}

const globalPathStringToPathCache: { [key: string]: PropertyPath } = {}
const globalPathCache: PropertyPathCache = emptyPathCache()

function getPathCache(elements: Array<PropertyPathPart>): PropertyPathCache {
  let workingPathCache: PropertyPathCache = globalPathCache
  fastForEach(elements, (pathPart) => {
    if (workingPathCache.childCaches[pathPart] == null) {
      const newCache = emptyPathCache()
      workingPathCache.childCaches[pathPart] = newCache
      workingPathCache = newCache
    } else {
      workingPathCache = workingPathCache.childCaches[pathPart]
    }
  })
  return workingPathCache
}

export function create(elements: Array<PropertyPathPart>): PropertyPath {
  const pathCache = getPathCache(elements)
  if (pathCache.cached == null) {
    const newPath = { propertyElements: elements }
    pathCache.cached = newPath
    return newPath
  } else {
    return pathCache.cached
  }
}

export function toString(propertyPath: PropertyPath, joinWith: string = '.'): string {
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

export function toVarSafeString(propertyPath: PropertyPath): string {
  return toString(propertyPath, '_')
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

export function tail(propertyPath: PropertyPath): PropertyPath {
  const newElements =
    propertyPath.propertyElements.length > 0 ? propertyPath.propertyElements.slice(1) : []
  return create(newElements)
}

export function getElements(propertyPath: PropertyPath): Array<PropertyPathPart> {
  return propertyPath.propertyElements
}

export function appendPropertyPathElems(
  path: PropertyPath,
  elems: Array<PropertyPathPart>,
): PropertyPath {
  return create(path.propertyElements.concat(elems))
}

export function prependPropertyPathElems(
  elems: Array<PropertyPathPart>,
  path: PropertyPath,
): PropertyPath {
  return create(elems.concat(path.propertyElements))
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

export function pathsEqual(l: PropertyPath | null, r: PropertyPath | null): boolean {
  if (l === null && r === null) {
    return true
  } else if (l !== null && r !== null) {
    return arrayEquals(l.propertyElements, r.propertyElements)
  } else {
    return false
  }
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
  return create([getElements(path)[0]])
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
    return create(pathToFollow.propertyElements.slice(0, pathToStep.propertyElements.length + 1))
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
  return create(findLongestMatchingSubPath(path.propertyElements, from))
}
