import {
  id,
  ElementPath,
  ElementPathPart,
  StaticElementPathPart,
  StaticElementPath,
} from './project-file-types'
import { arrayEquals, longestCommonArray, identity, fastForEach } from './utils'
import { replaceAll } from './string-utils'
import { last, dropLastN, drop, splitAt, flattenArray, dropLast } from './array-utils'
import { extractOriginalUidFromIndexedUid } from './uid-utils'
import { forceNotNull } from './optional-utils'

// KILLME, except in 28 places
export const toComponentId = toString

// Probably KILLME too
export function toVarSafeComponentId(path: ElementPath): string {
  const asStr = toString(path)
  return replaceAll(asStr, '-', '_')
}

interface ElementPathCache {
  cached: ElementPath | null
  cachedToString: string | null
  childCaches: { [key: string]: ElementPathCache }
  rootElementCaches: { [key: string]: ElementPathCache }
}

function emptyElementPathCache(): ElementPathCache {
  return {
    cached: null,
    cachedToString: null,
    childCaches: {},
    rootElementCaches: {},
  }
}

let dynamicToStaticPathCache: Map<ElementPath, StaticElementPath> = new Map()
let dynamicElementPathToStaticElementPathCache: Map<
  ElementPathPart,
  StaticElementPathPart
> = new Map()

let globalPathStringToPathCache: { [key: string]: ElementPath } = {}
let globalElementPathCache: ElementPathCache = emptyElementPathCache()

export function clearElementPathCache() {
  globalPathStringToPathCache = {}
  globalElementPathCache = emptyElementPathCache()
  dynamicToStaticPathCache = new Map()
  dynamicElementPathToStaticElementPathCache = new Map()
}

function getElementPathCache(fullElementPath: ElementPathPart[]): ElementPathCache {
  let workingPathCache: ElementPathCache = globalElementPathCache

  fastForEach(fullElementPath, (elementPathPart) => {
    fastForEach(elementPathPart, (pathPart, index) => {
      const cacheToUse = index === 0 ? 'rootElementCaches' : 'childCaches'
      if (workingPathCache[cacheToUse][pathPart] == null) {
        workingPathCache[cacheToUse][pathPart] = emptyElementPathCache()
      }

      workingPathCache = workingPathCache[cacheToUse][pathPart]
    })
  })

  return workingPathCache
}

const SceneSeparator = ':'
const ElementSeparator = '/'

export function elementPathPartToString(path: ElementPathPart): string {
  let result: string = ''
  const elementsLength = path.length
  fastForEach(path, (elem, index) => {
    result += elem
    if (index < elementsLength - 1) {
      result += ElementSeparator
    }
  })
  return result
}

export function toString(target: ElementPath): string {
  const pathCache = getElementPathCache(target.parts)
  if (pathCache.cachedToString == null) {
    pathCache.cachedToString = target.parts.map(elementPathPartToString).join(SceneSeparator)
  }

  return pathCache.cachedToString
}

export const emptyElementPathPart: StaticElementPathPart = staticElementPath([])
export const emptyElementPath: StaticElementPath = newElementPath([])

export function staticElementPath(elements: string[]): StaticElementPathPart {
  return elements as StaticElementPathPart
}

function isEmptyElementPathsArray(elementPathParts: ElementPathPart[]): boolean {
  return (
    elementPathParts.length === 0 ||
    (elementPathParts.length === 1 && elementPathParts[0].length === 0)
  )
}

export function isEmptyPath(path: ElementPath): boolean {
  return isEmptyElementPathsArray(path.parts)
}

function newElementPath(fullElementPath: StaticElementPathPart[]): StaticElementPath
function newElementPath(fullElementPath: ElementPathPart[]): ElementPath
function newElementPath(fullElementPath: ElementPathPart[]): ElementPath {
  return {
    type: 'elementpath',
    parts: fullElementPath,
  }
}

export function elementPath(fullElementPath: StaticElementPathPart[]): StaticElementPath
export function elementPath(fullElementPath: ElementPathPart[]): ElementPath
export function elementPath(fullElementPath: ElementPathPart[]): ElementPath {
  if (isEmptyElementPathsArray(fullElementPath)) {
    return emptyElementPath
  }

  const pathCache = getElementPathCache(fullElementPath)
  if (pathCache.cached == null) {
    pathCache.cached = newElementPath(fullElementPath)
  }

  return pathCache.cached
}

export function asStatic(path: ElementPath): StaticElementPath {
  return path as StaticElementPath
}

export function isElementPath(path: unknown): path is ElementPath {
  return (path as any)?.type === 'elementpath'
}

export function isRootElementOfInstance(path: ElementPath): boolean {
  return path.parts.length > 1 && last(path.parts)!.length === 1
}

export function isStoryboardPath(path: ElementPath): boolean {
  return path.parts.length === 1 && path.parts[0].length === 1
}

export function isStoryboardDescendant(path: ElementPath): boolean {
  return path.parts.length === 1 && path.parts[0].length > 1
}

export function isStoryboardChild(path: ElementPath): boolean {
  return path.parts.length === 1 && path.parts[0].length === 2
}

export function lastElementPathForPath(path: StaticElementPath): StaticElementPathPart | null
export function lastElementPathForPath(path: ElementPath): ElementPathPart | null
export function lastElementPathForPath(path: ElementPath): ElementPathPart | null {
  return last(path.parts) ?? null
}

function fromStringUncached(path: string): ElementPath {
  const elementPathPartStrings = path.split(SceneSeparator)
  const fullElementPath = elementPathPartStrings.map((pathString) =>
    pathString.split(ElementSeparator).filter((str) => str.trim().length > 0),
  )
  return elementPath(fullElementPath)
}

export function fromString(path: string): ElementPath {
  let fromPathStringCache = globalPathStringToPathCache[path]
  if (fromPathStringCache == null) {
    const result = fromStringUncached(path)
    globalPathStringToPathCache[path] = result
    return result
  } else {
    return fromPathStringCache
  }
}

function allElementPaths(fullPath: ElementPathPart[]): Array<ElementPathPart[]> {
  let paths: Array<ElementPathPart[]> = []
  for (var index = 1; index < fullPath.length; index++) {
    const prefix: ElementPathPart[] = fullPath.slice(0, index)
    const suffixes = allElementPathsForPart(fullPath[index])
    fastForEach(suffixes, (suffix) => paths.push(prefix.concat(suffix)))
  }

  return paths
}

function allElementPathsForPart(path: ElementPathPart): Array<ElementPathPart> {
  let paths: Array<ElementPathPart> = []
  for (var size = 1; size <= path.length; size++) {
    paths.push(path.slice(0, size))
  }
  return paths
}

export function allPathsForLastPart(path: ElementPath | null): Array<ElementPath> {
  if (path == null || isEmptyPath(path)) {
    return []
  } else {
    const prefix = dropLast(path.parts)
    const lastPart = last(path.parts)!
    const toElementPath = (elementPathPart: ElementPathPart) =>
      elementPath([...prefix, elementPathPart])
    return [elementPath(prefix), ...allElementPathsForPart(lastPart).map(toElementPath)]
  }
}

export function depth(path: ElementPath): number {
  return 1 + path.parts.length
}

export function navigatorDepth(path: ElementPath): number {
  return path.parts.reduce((working, next) => working + next.length, -2)
}

export function isInsideFocusedComponent(path: ElementPath): boolean {
  return path.parts.length > 2
}

function fullElementPathParent(path: StaticElementPathPart[]): StaticElementPathPart[]
function fullElementPathParent(path: ElementPathPart[]): ElementPathPart[]
function fullElementPathParent(path: ElementPathPart[]): ElementPathPart[] {
  const prefix = dropLast(path)
  const lastPart = last(path)
  if (lastPart != null && lastPart.length > 1) {
    return [...prefix, elementPathPartParent(lastPart)]
  } else {
    return prefix
  }
}

export function elementPathPartParent(path: StaticElementPathPart): StaticElementPathPart
export function elementPathPartParent(path: ElementPathPart): ElementPathPart
export function elementPathPartParent(path: ElementPathPart): ElementPathPart {
  return path.slice(0, path.length - 1)
}

export function parentPath(path: StaticElementPath): StaticElementPath
export function parentPath(path: ElementPath): ElementPath
export function parentPath(path: ElementPath): ElementPath {
  const parentFullElementPath = fullElementPathParent(path.parts)
  return elementPath(parentFullElementPath)
}

export function isParentOf(maybeParent: ElementPath, maybeChild: ElementPath): boolean {
  return pathsEqual(parentPath(maybeChild), maybeParent)
}

export function elementPathPartToUID(path: ElementPathPart): id {
  return forceNotNull('Attempting to get the UID of an empty ElementPath', last(path))
}

function lastElementPathPart(path: ElementPath): ElementPathPart {
  return last(path.parts) ?? emptyElementPathPart
}

export function toUid(path: ElementPath): id {
  const elementPathPartToUse = lastElementPathPart(path)
  return elementPathPartToUID(elementPathPartToUse)
}

export function toStaticUid(path: ElementPath): id {
  return extractOriginalUidFromIndexedUid(toUid(path))
}

export function appendToElementPath(
  path: StaticElementPathPart,
  next: id | Array<id>,
): StaticElementPathPart
export function appendToElementPath(path: ElementPathPart, next: id | Array<id>): ElementPathPart
export function appendToElementPath(path: ElementPathPart, next: id | Array<id>): ElementPathPart {
  return path.concat(next)
}

function appendToElementPathArray(
  pathArray: ElementPathPart[],
  next: id | ElementPathPart,
): ElementPathPart[] {
  if (isEmptyElementPathsArray(pathArray)) {
    return [Array.isArray(next) ? next : [next]]
  } else {
    const prefix = dropLast(pathArray)
    const lastPart = last(pathArray)!
    const updatedLastPart = appendToElementPath(lastPart, next)
    return [...prefix, updatedLastPart]
  }
}

export function appendNewElementPath(
  path: ElementPath,
  next: StaticElementPathPart,
): StaticElementPath
export function appendNewElementPath(path: ElementPath, next: id | ElementPathPart): ElementPath
export function appendNewElementPath(path: ElementPath, next: id | ElementPathPart): ElementPath {
  const toAppend = Array.isArray(next) ? next : [next]
  return elementPath([...path.parts, toAppend])
}

export function appendToPath(
  path: StaticElementPath,
  next: id | StaticElementPathPart,
): StaticElementPath
export function appendToPath(path: ElementPath, next: id | ElementPathPart): ElementPath
export function appendToPath(path: ElementPath, next: id | ElementPathPart): ElementPath {
  const updatedPathParts = appendToElementPathArray(path.parts, next)
  return elementPath(updatedPathParts)
}

export function notNullPathsEqual(l: ElementPath, r: ElementPath): boolean {
  return pathsEqual(l, r)
}

function elementPathPartsEqual(l: ElementPathPart, r: ElementPathPart): boolean {
  if (l === r) {
    return true
  } else {
    return arrayEquals(l, r)
  }
}

function fullElementPathsEqual(l: ElementPathPart[], r: ElementPathPart[]): boolean {
  return l === r || arrayEquals(l, r, elementPathPartsEqual)
}

export function pathsEqual(l: ElementPath | null, r: ElementPath | null): boolean {
  if (l == null) {
    return r == null
  } else if (r == null) {
    return false
  } else if (l === r) {
    return true
  } else {
    return fullElementPathsEqual(l.parts, r.parts)
  }
}

export function containsPath(path: ElementPath | null, paths: Array<ElementPath>): boolean {
  const matchesPath = (p: ElementPath) => pathsEqual(path, p)
  return paths.some(matchesPath)
}

export function filterPaths(paths: ElementPath[], pathsToFilter: ElementPath[]): ElementPath[] {
  return paths.filter((path) => !containsPath(path, pathsToFilter))
}

export function addPathIfMissing(path: ElementPath, paths: Array<ElementPath>): Array<ElementPath> {
  if (containsPath(path, paths)) {
    return paths
  } else {
    return paths.concat(path)
  }
}

export function addPathsIfMissing(
  existingPaths: Array<ElementPath>,
  pathsToAdd: Array<ElementPath>,
): Array<ElementPath> {
  if (pathsToAdd.length === 0) {
    return existingPaths
  } else if (existingPaths.length === 0) {
    return pathsToAdd
  } else {
    return existingPaths.concat(filterPaths(pathsToAdd, existingPaths))
  }
}

export function isChildOf(path: ElementPath | null, parent: ElementPath | null): boolean {
  if (path == null || parent == null) {
    return false
  } else {
    return isParentOf(parent, path)
  }
}

export function isSiblingOf(l: ElementPath | null, r: ElementPath | null): boolean {
  return l != null && r != null && pathsEqual(parentPath(l), parentPath(r))
}

function slicedPathsEqual(l: ElementPathPart, r: ElementPathPart): boolean {
  const slicedL = l.slice(0, r.length)
  return elementPathPartsEqual(slicedL, r)
}

function elementIsDescendant(l: ElementPathPart, r: ElementPathPart): boolean {
  return l.length > r.length && slicedPathsEqual(l, r)
}

function elementIsDescendantOrEqualTo(l: ElementPathPart, r: ElementPathPart): boolean {
  return l.length >= r.length && slicedPathsEqual(l, r)
}

export function isDescendantOfOrEqualTo(
  target: ElementPath,
  maybeAncestorOrEqual: ElementPath,
): boolean {
  return pathsEqual(target, maybeAncestorOrEqual) || isDescendantOf(target, maybeAncestorOrEqual)
}

export function isDescendantOf(target: ElementPath, maybeAncestor: ElementPath): boolean {
  const targetElementPath = target.parts
  const maybeAncestorElementPath = maybeAncestor.parts
  if (targetElementPath.length >= maybeAncestorElementPath.length) {
    const partsToCheck = targetElementPath.slice(0, maybeAncestorElementPath.length)
    return partsToCheck.every((elementPathPart, i) => {
      // all parts up to the last must match, and the last must be a descendant
      if (i < maybeAncestorElementPath.length - 1) {
        return elementPathPartsEqual(elementPathPart, maybeAncestorElementPath[i])
      } else {
        const finalPartComparison =
          targetElementPath.length === maybeAncestorElementPath.length
            ? elementIsDescendant
            : elementIsDescendantOrEqualTo

        return finalPartComparison(elementPathPart, maybeAncestorElementPath[i])
      }
    })
  } else {
    return false
  }
}

export function getAncestorsForLastPart(path: ElementPath): ElementPath[] {
  return allPathsForLastPart(path).slice(0, -1)
}

function dropFromElementPaths(elementPathParts: ElementPathPart[], n: number): ElementPathPart[] {
  const prefix = dropLast(elementPathParts)
  const lastPart = last(elementPathParts)
  if (lastPart == null) {
    return []
  } else {
    const remaining = lastPart.length - n
    if (remaining > 0) {
      return dropFromElementPaths(prefix, n)
    } else {
      return [...prefix, dropLastN(n, lastPart)]
    }
  }
}

function dropFromPath(path: ElementPath, n: number): ElementPath {
  const updatedPathParts = dropFromElementPaths(path.parts, n)
  return updatedPathParts.length > 0 ? elementPath(updatedPathParts) : emptyElementPath
}

export const getNthParent = dropFromPath

export function replaceIfAncestor(
  path: ElementPath,
  replaceSearch: ElementPath,
  replaceWith: ElementPath | null,
): ElementPath | null {
  // A/B/C -> A/B -> G/H -> G/H/C
  if (isDescendantOf(path, replaceSearch) || pathsEqual(path, replaceSearch)) {
    const oldParts = path.parts
    const suffix = drop(replaceSearch.parts.length, oldParts)
    const lastReplaceSearchPartLength = last(replaceSearch.parts)?.length ?? 0
    const overlappingPart = oldParts[replaceSearch.parts.length - 1]
    const trimmedOverlappingPart =
      overlappingPart == null ? null : drop(lastReplaceSearchPartLength, overlappingPart)

    let prefix: ElementPathPart[]
    if (trimmedOverlappingPart == null) {
      prefix = replaceWith == null ? [] : replaceWith.parts
    } else if (replaceWith == null) {
      prefix = [trimmedOverlappingPart]
    } else {
      prefix = appendToElementPathArray(replaceWith.parts, trimmedOverlappingPart)
    }

    const updatedPathParts = [...prefix, ...suffix]
    return elementPath(updatedPathParts)
  } else {
    return null
  }
}

export function replaceOrDefault(
  path: ElementPath,
  replaceSearch: ElementPath,
  replaceWith: ElementPath | null,
): ElementPath {
  return replaceIfAncestor(path, replaceSearch, replaceWith) ?? path
}

export function closestSharedAncestor(
  l: ElementPath | null,
  r: ElementPath | null,
  includePathsEqual: boolean,
): ElementPath | null {
  const toTargetPath: (p: ElementPath) => ElementPath | null = includePathsEqual
    ? identity
    : parentPath

  const lTarget = l == null ? null : toTargetPath(l)
  const rTarget = r == null ? null : toTargetPath(r)

  if (l === null || r === null || lTarget == null || rTarget == null) {
    return null
  } else if (l === r) {
    return toTargetPath(l)
  } else {
    const fullyMatchedElementPathParts = longestCommonArray(
      lTarget.parts,
      rTarget.parts,
      elementPathPartsEqual,
    )
    const nextLPart = lTarget.parts[fullyMatchedElementPathParts.length]
    const nextRPart = rTarget.parts[fullyMatchedElementPathParts.length]

    const nextMatchedElementPath = longestCommonArray(nextLPart ?? [], nextRPart ?? [])
    const totalMatchedParts =
      nextMatchedElementPath.length > 0
        ? [...fullyMatchedElementPathParts, nextMatchedElementPath]
        : fullyMatchedElementPathParts

    return totalMatchedParts.length > 0 ? elementPath(totalMatchedParts) : null
  }
}

export function getCommonParent(
  paths: Array<ElementPath>,
  includeSelf: boolean = false,
): ElementPath | null {
  if (paths.length === 0) {
    return null
  } else {
    const parents = includeSelf ? paths : paths.map(parentPath)
    return parents.reduce<ElementPath | null>(
      (l, r) => closestSharedAncestor(l, r, true),
      parents[0],
    )
  }
}

export interface ElementsTransformResult<T> {
  elements: Array<T>
  transformedElement: T | null
}

export function findAndTransformAtPath<T>(
  elements: Array<T>,
  path: ElementPathPart,
  getChildren: (t: T) => Array<T> | null,
  getElementID: (element: T) => string,
  transform: (t: T) => T,
): ElementsTransformResult<T> {
  let transformedElement: T | null = null

  function findAndTransformAtPathInner(element: T, workingPath: string[]): T {
    if (transformedElement != null) {
      return element
    }

    const [firstUIDOrIndex, ...tailPath] = workingPath
    if (getElementID(element) === firstUIDOrIndex) {
      // transform
      if (tailPath.length === 0) {
        // this is the element we want to transform
        transformedElement = transform(element)
        return transformedElement
      } else {
        // we will want to transform one of our children
        let childrenChanged: boolean = false
        let transformedChildren: Array<T> = []
        const children = getChildren(element)
        if (children != null) {
          fastForEach(children, (child) => {
            const transformedChild = findAndTransformAtPathInner(child, tailPath)
            if (transformedChild == child) {
              transformedChildren.push(child)
            } else {
              childrenChanged = true
              transformedChildren.push(transformedChild)
            }
          })
        }
        if (childrenChanged && children != null) {
          return {
            ...element,
            children: transformedChildren,
          }
        } else {
          // we had a NO_OP transform result, let's keep reference equality
          return element
        }
      }
    } else {
      // this is not the branch we are looking for
      return element
    }
  }

  const transformedElements = elements.map((element) => {
    if (transformedElement == null) {
      return findAndTransformAtPathInner(element, path)
    } else {
      return element
    }
  })

  return {
    elements: transformedElements,
    transformedElement: transformedElement,
  }
}

export function transformAtPath<T>(
  elements: Array<T>,
  path: ElementPathPart,
  getChildren: (t: T) => Array<T> | null,
  getElementID: (element: T) => string,
  transform: (t: T) => T,
): Array<T> {
  const transformResult = findAndTransformAtPath(
    elements,
    path,
    getChildren,
    getElementID,
    transform,
  )
  if (transformResult.transformedElement == null) {
    throw new Error(`Did not find element to transform ${elementPathPartToString(path)}`)
  } else {
    return transformResult.elements
  }
}

export function findAtElementPath<T>(
  elements: Array<T>,
  path: ElementPathPart,
  getChildren: (t: T) => Array<T> | null,
  getElementID: (element: T) => string,
): T | null {
  let searchFailed: boolean = false
  let foundElement: T | null = null
  const shouldStop = () => searchFailed || foundElement != null

  function findAtPathInner(element: T, workingPath: string[]) {
    if (shouldStop()) {
      return
    }

    const firstUIDOrIndex = workingPath[0]
    if (getElementID(element) === firstUIDOrIndex) {
      // we've found the right path
      if (workingPath.length === 1) {
        foundElement = element
        return
      } else {
        const children = getChildren(element)
        if (children != null) {
          const tailPath = workingPath.slice(1)
          fastForEach(children, (child) => {
            if (shouldStop()) {
              return
            }

            findAtPathInner(child, tailPath)
          })
        }

        if (foundElement == null) {
          searchFailed = true
        }
      }
    } else {
      // this is not the branch we are looking for
      return
    }
  }

  fastForEach(elements, (element) => findAtPathInner(element, path))

  return foundElement
}

function dropLastPathPart(path: ElementPath): ElementPath {
  return elementPath(dropLast(path.parts))
}

export function areAllElementsInSameInstance(paths: ElementPath[]): boolean {
  if (paths.length === 0) {
    return true
  } else {
    const firstPathWithoutLastPart = dropLastPathPart(paths[0])
    return paths.every((p) => pathsEqual(firstPathWithoutLastPart, dropLastPathPart(p)))
  }
}

export function isFromSameInstanceAs(a: ElementPath, b: ElementPath): boolean {
  return pathsEqual(dropLastPathPart(a), dropLastPathPart(b))
}

function dynamicElementPathToStaticElementPath(element: ElementPathPart): StaticElementPathPart {
  const existing = dynamicElementPathToStaticElementPathCache.get(element)
  if (existing == null) {
    const result = element.map(extractOriginalUidFromIndexedUid) as StaticElementPathPart
    dynamicElementPathToStaticElementPathCache.set(element, result)
    return result
  } else {
    return existing
  }
}

export function dynamicPathToStaticPath(path: ElementPath): StaticElementPath {
  const existing = dynamicToStaticPathCache.get(path)
  if (existing == null) {
    const result = elementPath(path.parts.map(dynamicElementPathToStaticElementPath))
    dynamicToStaticPathCache.set(path, result)
    return result
  } else {
    return existing
  }
}

export function makeLastPartOfPathStatic(path: ElementPath): ElementPath {
  const dynamicLastPart = last(path.parts)
  if (dynamicLastPart == null) {
    return path
  } else {
    const staticLastPart = dynamicElementPathToStaticElementPath(dynamicLastPart)
    return elementPath([...dropLast(path.parts), staticLastPart])
  }
}

export function pathUpToElementPath(
  fullPath: ElementPath,
  elementPathPart: ElementPathPart,
  convertToStatic: 'dynamic-path' | 'static-path',
): ElementPath | null {
  const fullElementPath = fullPath.parts
  const pathToUse =
    convertToStatic === 'static-path'
      ? fullElementPath.map(dynamicElementPathToStaticElementPath)
      : fullElementPath
  const foundIndex = pathToUse.findIndex((pathPart) => {
    return elementPathPartsEqual(pathPart, elementPathPart)
  })
  return foundIndex === -1 ? null : elementPath(fullElementPath.slice(0, foundIndex + 1))
}

export interface DropFirstPathElementResultType {
  newPath: StaticElementPath | null
  droppedPathElements: StaticElementPathPart | null
}

export function dropFirstPathElement(
  path: StaticElementPath | null,
): DropFirstPathElementResultType {
  if (path == null) {
    return {
      newPath: null,
      droppedPathElements: null,
    }
  } else {
    const fullElementPath = path.parts
    if (fullElementPath.length > 1) {
      return {
        newPath: elementPath(drop(1, fullElementPath)),
        droppedPathElements: fullElementPath[0],
      }
    } else {
      return {
        newPath: path,
        droppedPathElements: null,
      }
    }
  }
}

export function createBackwardsCompatibleScenePath(path: ElementPath): ElementPath {
  const firstPart = path.parts[0] ?? emptyElementPathPart
  return elementPath([firstPart.slice(0, 2)]) // we only return the FIRST TWO elements (storyboard, scene), this is a horrible, horrible hack
}

export function isFocused(focusedElementPath: ElementPath | null, path: ElementPath): boolean {
  const lastPart = lastElementPathForPath(path)
  if (focusedElementPath == null || lastPart == null || isStoryboardDescendant(path)) {
    return false
  } else {
    return pathUpToElementPath(focusedElementPath, lastPart, 'dynamic-path') != null
  }
}
