import {
  id,
  TemplatePath,
  ElementPath,
  StaticElementPath,
  StaticTemplatePath,
} from './project-file-types'
import { arrayEquals, longestCommonArray, identity, fastForEach } from './utils'
import { replaceAll } from './string-utils'
import { last, dropLastN, drop, splitAt, flattenArray, dropLast } from './array-utils'
import { extractOriginalUidFromIndexedUid } from './uid-utils'
import { forceNotNull } from './optional-utils'

// KILLME, except in 28 places
export const toComponentId = toString

// Probably KILLME too
export function toVarSafeComponentId(path: TemplatePath): string {
  const asStr = toString(path)
  return replaceAll(asStr, '-', '_')
}

interface TemplatePathCache {
  cached: TemplatePath | null
  cachedToString: string | null
  childCaches: { [key: string]: TemplatePathCache }
  rootElementCaches: { [key: string]: TemplatePathCache }
}

function emptyTemplatePathCache(): TemplatePathCache {
  return {
    cached: null,
    cachedToString: null,
    childCaches: {},
    rootElementCaches: {},
  }
}

let globalPathStringToPathCache: { [key: string]: TemplatePath } = {}
let globalTemplatePathCache: TemplatePathCache = emptyTemplatePathCache()

export function clearTemplatePathCache() {
  globalPathStringToPathCache = {}
  globalTemplatePathCache = emptyTemplatePathCache()
}

function getTemplatePathCache(fullElementPath: ElementPath[]): TemplatePathCache {
  let workingPathCache: TemplatePathCache = globalTemplatePathCache

  fastForEach(fullElementPath, (elementPath) => {
    fastForEach(elementPath, (pathPart, index) => {
      const cacheToUse = index === 0 ? 'rootElementCaches' : 'childCaches'
      if (workingPathCache[cacheToUse][pathPart] == null) {
        workingPathCache[cacheToUse][pathPart] = emptyTemplatePathCache()
      }

      workingPathCache = workingPathCache[cacheToUse][pathPart]
    })
  })

  return workingPathCache
}

const SceneSeparator = ':'
const ElementSeparator = '/'

export function elementPathToString(path: ElementPath): string {
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

export function toString(target: TemplatePath): string {
  const pathCache = getTemplatePathCache(target.parts)
  if (pathCache.cachedToString == null) {
    pathCache.cachedToString = target.parts.map(elementPathToString).join(SceneSeparator)
  }

  return pathCache.cachedToString
}

export const emptyElementPath: StaticElementPath = staticElementPath([])
export const emptyTemplatePath: StaticTemplatePath = newTemplatePath([])

export function staticElementPath(elements: string[]): StaticElementPath {
  return elements as StaticElementPath
}

function isEmptyElementPathsArray(elementPaths: ElementPath[]): boolean {
  return elementPaths.length === 0 || (elementPaths.length === 1 && elementPaths[0].length === 0)
}

export function isEmptyPath(path: TemplatePath): boolean {
  return isEmptyElementPathsArray(path.parts)
}

function newTemplatePath(fullElementPath: StaticElementPath[]): StaticTemplatePath
function newTemplatePath(fullElementPath: ElementPath[]): TemplatePath
function newTemplatePath(fullElementPath: ElementPath[]): TemplatePath {
  return {
    type: 'templatepath',
    parts: fullElementPath,
  }
}

export function templatePath(fullElementPath: StaticElementPath[]): StaticTemplatePath
export function templatePath(fullElementPath: ElementPath[]): TemplatePath
export function templatePath(fullElementPath: ElementPath[]): TemplatePath {
  if (isEmptyElementPathsArray(fullElementPath)) {
    return emptyTemplatePath
  }

  const pathCache = getTemplatePathCache(fullElementPath)
  if (pathCache.cached == null) {
    pathCache.cached = newTemplatePath(fullElementPath)
  }

  return pathCache.cached
}

export function asStatic(path: TemplatePath): StaticTemplatePath {
  return path as StaticTemplatePath
}

export function isTemplatePath(path: unknown): path is TemplatePath {
  return (path as any)?.type === 'templatepath'
}

export function isRootElementPath(path: TemplatePath): boolean {
  return path.parts.length === 1
}

export function isStoryboardPath(path: TemplatePath): boolean {
  return path.parts.length === 1 && path.parts[0].length === 1
}

export function isStoryboardDescendant(path: TemplatePath): boolean {
  return path.parts.length === 1 && path.parts[0].length > 1
}

export function isStoryboardChild(path: TemplatePath): boolean {
  return path.parts.length === 1 && path.parts[0].length === 1
}

export function lastElementPathForPath(path: StaticTemplatePath): StaticElementPath | null
export function lastElementPathForPath(path: TemplatePath): ElementPath | null
export function lastElementPathForPath(path: TemplatePath): ElementPath | null {
  return last(path.parts) ?? null
}

function fromStringUncached(path: string): TemplatePath {
  const elementPathStrings = path.split(SceneSeparator)
  const fullElementPath = elementPathStrings.map((pathString) =>
    pathString.split(ElementSeparator).filter((str) => str.trim().length > 0),
  )
  return templatePath(fullElementPath)
}

export function fromString(path: string): TemplatePath {
  let fromPathStringCache = globalPathStringToPathCache[path]
  if (fromPathStringCache == null) {
    const result = fromStringUncached(path)
    globalPathStringToPathCache[path] = result
    return result
  } else {
    return fromPathStringCache
  }
}

function allElementPaths(fullPath: ElementPath[]): Array<ElementPath[]> {
  let paths: Array<ElementPath[]> = []
  for (var index = 1; index < fullPath.length; index++) {
    const prefix: ElementPath[] = fullPath.slice(0, index)
    const suffixes = allElementPathsForPart(fullPath[index])
    fastForEach(suffixes, (suffix) => paths.push(prefix.concat(suffix)))
  }

  return paths
}

function allElementPathsForPart(path: ElementPath): Array<ElementPath> {
  let paths: Array<ElementPath> = []
  for (var size = 1; size <= path.length; size++) {
    paths.push(path.slice(0, size))
  }
  return paths
}

export function allPathsForLastPart(path: TemplatePath | null): Array<TemplatePath> {
  if (path == null || isEmptyPath(path)) {
    return []
  } else {
    const prefix = dropLast(path.parts)
    const lastPart = last(path.parts)!
    const toTemplatePath = (elementPath: ElementPath) => templatePath([...prefix, elementPath])
    return [templatePath(prefix), ...allElementPathsForPart(lastPart).map(toTemplatePath)]
  }
}

export function depth(path: TemplatePath): number {
  return 1 + path.parts.length
}

export function navigatorDepth(path: TemplatePath): number {
  return path.parts.reduce((working, next) => working + next.length, -2)
}

export function isInsideFocusedComponent(path: TemplatePath): boolean {
  return path.parts.length > 2
}

function fullElementPathParent(path: StaticElementPath[]): StaticElementPath[]
function fullElementPathParent(path: ElementPath[]): ElementPath[]
function fullElementPathParent(path: ElementPath[]): ElementPath[] {
  const prefix = dropLast(path)
  const lastPart = last(path)
  if (lastPart != null && lastPart.length > 1) {
    return [...prefix, elementPathParent(lastPart)]
  } else {
    return prefix
  }
}

export function elementPathParent(path: StaticElementPath): StaticElementPath
export function elementPathParent(path: ElementPath): ElementPath
export function elementPathParent(path: ElementPath): ElementPath {
  return path.slice(0, path.length - 1)
}

export function parentPath(path: StaticTemplatePath): StaticTemplatePath
export function parentPath(path: TemplatePath): TemplatePath
export function parentPath(path: TemplatePath): TemplatePath {
  const parentFullElementPath = fullElementPathParent(path.parts)
  return templatePath(parentFullElementPath)
}

export function isParentOf(maybeParent: TemplatePath, maybeChild: TemplatePath): boolean {
  return pathsEqual(parentPath(maybeChild), maybeParent)
}

export function elementPathToUID(path: ElementPath): id {
  return forceNotNull('Attempting to get the UID of an empty ElementPath', last(path))
}

function lastElementPathPart(path: TemplatePath): ElementPath {
  return last(path.parts) ?? emptyElementPath
}

export function toUid(path: TemplatePath): id {
  const elementPathToUse = lastElementPathPart(path)
  return elementPathToUID(elementPathToUse)
}

export function toStaticUid(path: TemplatePath): id {
  return extractOriginalUidFromIndexedUid(toUid(path))
}

export function appendToElementPath(
  path: StaticElementPath,
  next: id | Array<id>,
): StaticElementPath
export function appendToElementPath(path: ElementPath, next: id | Array<id>): ElementPath
export function appendToElementPath(path: ElementPath, next: id | Array<id>): ElementPath {
  return path.concat(next)
}

function appendToElementPathArray(pathArray: ElementPath[], next: id | ElementPath): ElementPath[] {
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
  path: TemplatePath,
  next: StaticElementPath,
): StaticTemplatePath
export function appendNewElementPath(path: TemplatePath, next: id | ElementPath): TemplatePath
export function appendNewElementPath(path: TemplatePath, next: id | ElementPath): TemplatePath {
  const toAppend = Array.isArray(next) ? next : [next]
  return templatePath([...path.parts, toAppend])
}

export function appendToPath(
  path: StaticTemplatePath,
  next: id | StaticElementPath,
): StaticTemplatePath
export function appendToPath(path: TemplatePath, next: id | ElementPath): TemplatePath
export function appendToPath(path: TemplatePath, next: id | ElementPath): TemplatePath {
  const updatedPathParts = appendToElementPathArray(path.parts, next)
  return templatePath(updatedPathParts)
}

export function notNullPathsEqual(l: TemplatePath, r: TemplatePath): boolean {
  return pathsEqual(l, r)
}

function elementPathsEqual(l: ElementPath, r: ElementPath): boolean {
  if (l === r) {
    return true
  } else {
    return arrayEquals(l, r)
  }
}

function fullElementPathsEqual(l: ElementPath[], r: ElementPath[]): boolean {
  return l === r || arrayEquals(l, r, elementPathsEqual)
}

export function pathsEqual(l: TemplatePath | null, r: TemplatePath | null): boolean {
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

export function containsPath(path: TemplatePath | null, paths: Array<TemplatePath>): boolean {
  const matchesPath = (p: TemplatePath) => pathsEqual(path, p)
  return paths.some(matchesPath)
}

export function filterPaths(paths: TemplatePath[], pathsToFilter: TemplatePath[]): TemplatePath[] {
  return paths.filter((path) => !containsPath(path, pathsToFilter))
}

export function addPathIfMissing(
  path: TemplatePath,
  paths: Array<TemplatePath>,
): Array<TemplatePath> {
  if (containsPath(path, paths)) {
    return paths
  } else {
    return paths.concat(path)
  }
}

export function addPathsIfMissing(
  existingPaths: Array<TemplatePath>,
  pathsToAdd: Array<TemplatePath>,
): Array<TemplatePath> {
  if (pathsToAdd.length === 0) {
    return existingPaths
  } else if (existingPaths.length === 0) {
    return pathsToAdd
  } else {
    return existingPaths.concat(filterPaths(pathsToAdd, existingPaths))
  }
}

export function isChildOf(path: TemplatePath | null, parent: TemplatePath | null): boolean {
  if (path == null || parent == null) {
    return false
  } else {
    return isParentOf(parent, path)
  }
}

export function isSiblingOf(l: TemplatePath | null, r: TemplatePath | null): boolean {
  return l != null && r != null && pathsEqual(parentPath(l), parentPath(r))
}

function slicedPathsEqual(l: ElementPath, r: ElementPath): boolean {
  const slicedL = l.slice(0, r.length)
  return elementPathsEqual(slicedL, r)
}

function elementIsDescendant(l: ElementPath, r: ElementPath): boolean {
  return l.length > r.length && slicedPathsEqual(l, r)
}

function elementIsDescendantOrEqualTo(l: ElementPath, r: ElementPath): boolean {
  return l.length >= r.length && slicedPathsEqual(l, r)
}

export function isDescendantOfOrEqualTo(
  target: TemplatePath,
  maybeAncestorOrEqual: TemplatePath,
): boolean {
  return pathsEqual(target, maybeAncestorOrEqual) || isDescendantOf(target, maybeAncestorOrEqual)
}

export function isDescendantOf(target: TemplatePath, maybeAncestor: TemplatePath): boolean {
  const targetElementPath = target.parts
  const maybeAncestorElementPath = maybeAncestor.parts
  if (targetElementPath.length >= maybeAncestorElementPath.length) {
    const partsToCheck = targetElementPath.slice(0, maybeAncestorElementPath.length)
    return partsToCheck.every((elementPath, i) => {
      // all parts up to the last must match, and the last must be a descendant
      if (i < maybeAncestorElementPath.length - 1) {
        return elementPathsEqual(elementPath, maybeAncestorElementPath[i])
      } else {
        const finalPartComparison =
          targetElementPath.length === maybeAncestorElementPath.length
            ? elementIsDescendant
            : elementIsDescendantOrEqualTo

        return finalPartComparison(elementPath, maybeAncestorElementPath[i])
      }
    })
  } else {
    return false
  }
}

export function getAncestorsForLastPart(path: TemplatePath): TemplatePath[] {
  return allPathsForLastPart(path).slice(0, -1)
}

function dropFromElementPaths(elementPaths: ElementPath[], n: number): ElementPath[] {
  const prefix = dropLast(elementPaths)
  const lastPart = last(elementPaths)
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

function dropFromPath(path: TemplatePath, n: number): TemplatePath {
  const updatedPathParts = dropFromElementPaths(path.parts, n)
  return updatedPathParts.length > 0 ? templatePath(updatedPathParts) : emptyTemplatePath
}

export const getNthParent = dropFromPath

export function replaceIfAncestor(
  path: TemplatePath,
  replaceSearch: TemplatePath,
  replaceWith: TemplatePath | null,
): TemplatePath | null {
  // A/B/C -> A/B -> G/H -> G/H/C
  if (isDescendantOf(path, replaceSearch) || pathsEqual(path, replaceSearch)) {
    const oldParts = path.parts
    const suffix = drop(replaceSearch.parts.length, oldParts)
    const lastReplaceSearchPartLength = last(replaceSearch.parts)?.length ?? 0
    const overlappingPart = oldParts[replaceSearch.parts.length - 1]
    const trimmedOverlappingPart =
      overlappingPart == null ? null : drop(lastReplaceSearchPartLength, overlappingPart)

    let prefix: ElementPath[]
    if (trimmedOverlappingPart == null) {
      prefix = replaceWith == null ? [] : replaceWith.parts
    } else if (replaceWith == null) {
      prefix = [trimmedOverlappingPart]
    } else {
      prefix = appendToElementPathArray(replaceWith.parts, trimmedOverlappingPart)
    }

    const updatedPathParts = [...prefix, ...suffix]
    return templatePath(updatedPathParts)
  } else {
    return null
  }
}

export function replaceOrDefault(
  path: TemplatePath,
  replaceSearch: TemplatePath,
  replaceWith: TemplatePath | null,
): TemplatePath {
  return replaceIfAncestor(path, replaceSearch, replaceWith) ?? path
}

export function closestSharedAncestor(
  l: TemplatePath | null,
  r: TemplatePath | null,
  includePathsEqual: boolean,
): TemplatePath | null {
  const toTargetPath: (p: TemplatePath) => TemplatePath | null = includePathsEqual
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
      elementPathsEqual,
    )
    const nextLPart = lTarget.parts[fullyMatchedElementPathParts.length]
    const nextRPart = rTarget.parts[fullyMatchedElementPathParts.length]

    const nextMatchedElementPath = longestCommonArray(nextLPart ?? [], nextRPart ?? [])
    const totalMatchedParts =
      nextMatchedElementPath.length > 0
        ? [...fullyMatchedElementPathParts, nextMatchedElementPath]
        : fullyMatchedElementPathParts

    return totalMatchedParts.length > 0 ? templatePath(totalMatchedParts) : null
  }
}

export function getCommonParent(
  paths: Array<TemplatePath>,
  includeSelf: boolean = false,
): TemplatePath | null {
  if (paths.length === 0) {
    return null
  } else {
    const parents = includeSelf ? paths : paths.map(parentPath)
    return parents.reduce<TemplatePath | null>(
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
  path: ElementPath,
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
  path: ElementPath,
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
    throw new Error(`Did not find element to transform ${elementPathToString(path)}`)
  } else {
    return transformResult.elements
  }
}

export function findAtElementPath<T>(
  elements: Array<T>,
  path: ElementPath,
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

function dropLastPathPart(path: TemplatePath): TemplatePath {
  return templatePath(dropLast(path.parts))
}

export function areAllElementsInSameInstance(paths: TemplatePath[]): boolean {
  if (paths.length === 0) {
    return true
  } else {
    const firstPathWithoutLastPart = dropLastPathPart(paths[0])
    return paths.every((p) => pathsEqual(firstPathWithoutLastPart, dropLastPathPart(p)))
  }
}

export function isFromSameInstanceAs(a: TemplatePath, b: TemplatePath): boolean {
  return pathsEqual(dropLastPathPart(a), dropLastPathPart(b))
}

function dynamicElementPathToStaticElementPath(element: ElementPath): StaticElementPath {
  return element.map(extractOriginalUidFromIndexedUid) as StaticElementPath
}

export function dynamicPathToStaticPath(path: TemplatePath): StaticTemplatePath {
  return templatePath(path.parts.map(dynamicElementPathToStaticElementPath))
}

export function makeLastPartOfPathStatic(path: TemplatePath): TemplatePath {
  const dynamicLastPart = last(path.parts)
  if (dynamicLastPart == null) {
    return path
  } else {
    const staticLastPart = dynamicElementPathToStaticElementPath(dynamicLastPart)
    return templatePath([...dropLast(path.parts), staticLastPart])
  }
}

export function pathUpToElementPath(
  fullPath: TemplatePath,
  elementPath: ElementPath,
  convertToStatic: 'dynamic-path' | 'static-path',
): TemplatePath | null {
  const fullElementPath = fullPath.parts
  const pathToUse =
    convertToStatic === 'static-path'
      ? fullElementPath.map(dynamicElementPathToStaticElementPath)
      : fullElementPath
  const foundIndex = pathToUse.findIndex((pathPart) => {
    return elementPathsEqual(pathPart, elementPath)
  })
  return foundIndex === -1 ? null : templatePath(fullElementPath.slice(0, foundIndex + 1))
}

interface DropFirstPathElementResultType {
  newPath: StaticTemplatePath | null
  droppedPathElements: StaticElementPath | null
}

export function dropFirstPathElement(
  path: StaticTemplatePath | null,
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
        newPath: templatePath(drop(1, fullElementPath)),
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

export function createBackwardsCompatibleScenePath(path: TemplatePath): TemplatePath {
  return templatePath(path.parts.slice(0, 2)) // we only return the FIRST TWO elements (storyboard, scene), this is a horrible, horrible hack
}

export function isFocused(focusedElementPath: TemplatePath | null, path: TemplatePath): boolean {
  const lastPart = lastElementPathForPath(path)
  if (focusedElementPath == null || lastPart == null || isStoryboardDescendant(path)) {
    return false
  } else {
    return pathUpToElementPath(focusedElementPath, lastPart, 'dynamic-path') != null
  }
}
