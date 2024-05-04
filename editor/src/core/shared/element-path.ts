import type {
  id,
  ElementPath,
  ElementPathPart,
  StaticElementPathPart,
  StaticElementPath,
} from './project-file-types'
import {
  arrayEqualsByReference,
  longestCommonArray,
  identity,
  fastForEach,
  arrayEqualsByValue,
} from './utils'
import { replaceAll } from './string-utils'
import type { NonEmptyArray } from './array-utils'
import { last, dropLastN, drop, dropLast } from './array-utils'
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
  childCaches: { [key: string]: ElementPathCache }
  rootElementCaches: { [key: string]: ElementPathCache }
}

function emptyElementPathCache(): ElementPathCache {
  return {
    cached: null,
    childCaches: {},
    rootElementCaches: {},
  }
}

let pathToStringCache: WeakMap<ElementPath, string> = new WeakMap()
let dynamicToStaticPathCache: Map<ElementPath, StaticElementPath> = new Map()
let dynamicToStaticLastElementPathPartCache: Map<ElementPath, ElementPath> = new Map()
let dynamicElementPathToStaticElementPathCache: Map<ElementPathPart, StaticElementPathPart> =
  new Map()

let globalPathStringToPathCache: { [key: string]: ElementPath } = {}
let globalElementPathCache: ElementPathCache = emptyElementPathCache()

type RemovedPaths = Set<ElementPath>

function removePathsFromElementPathCacheWithDeadUIDs(existingUIDs: Set<string>): RemovedPaths {
  let removedPaths: Set<ElementPath> = new Set()

  function pushRemovedValues(cacheToRemove: ElementPathCache) {
    if (cacheToRemove.cached != null) {
      removedPaths.add(cacheToRemove.cached)
    }
  }

  function traverseAndCull(
    currentCache: ElementPathCache,
    testBeforeCull: 'test-before-cull' | 'cull-all',
  ) {
    fastForEach(Object.entries(currentCache.childCaches), ([key, value]) => {
      if (testBeforeCull === 'test-before-cull' && existingUIDs.has(key)) {
        traverseAndCull(value, 'test-before-cull')
      } else {
        pushRemovedValues(value)
        traverseAndCull(value, 'cull-all')
        delete currentCache.childCaches[key]
      }
    })

    fastForEach(Object.entries(currentCache.rootElementCaches), ([key, value]) => {
      if (testBeforeCull === 'test-before-cull' && existingUIDs.has(key)) {
        traverseAndCull(value, 'test-before-cull')
      } else {
        pushRemovedValues(value)
        traverseAndCull(value, 'cull-all')
        delete currentCache.rootElementCaches[key]
      }
    })
  }

  traverseAndCull(globalElementPathCache, 'test-before-cull')

  return removedPaths
}

export function removePathsWithDeadUIDs(existingUIDs: Set<string>): void {
  const removedPaths = removePathsFromElementPathCacheWithDeadUIDs(existingUIDs)

  fastForEach(Object.keys(globalPathStringToPathCache), (stringifiedPath) => {
    const pathUIDs = stringifiedPath.split(/[:/]/)
    if (pathUIDs.some((uid) => !existingUIDs.has(uid))) {
      delete globalPathStringToPathCache[stringifiedPath]
    }
  })

  dynamicToStaticPathCache.forEach((value, key) => {
    if (removedPaths.has(value)) {
      dynamicToStaticPathCache.delete(key)
    }
  })

  dynamicElementPathToStaticElementPathCache.forEach((value, key) => {
    if (value.some((uid) => !existingUIDs.has(uid))) {
      dynamicElementPathToStaticElementPathCache.delete(key)
    }
  })
}

function getElementPathCache(fullElementPath: ElementPathPart[]): ElementPathCache {
  let workingPathCache: ElementPathCache = globalElementPathCache

  function shiftWorkingCacheRoot(pathPart: string) {
    const innerCache = workingPathCache.rootElementCaches
    const pathPartCache = innerCache[pathPart]
    if (pathPartCache == null) {
      const newCache = emptyElementPathCache()
      innerCache[pathPart] = newCache
      workingPathCache = newCache
    } else {
      workingPathCache = pathPartCache
    }
  }

  function shiftWorkingCacheChild(pathPart: string) {
    const innerCache = workingPathCache.childCaches
    const pathPartCache = innerCache[pathPart]
    if (pathPartCache == null) {
      const newCache = emptyElementPathCache()
      innerCache[pathPart] = newCache
      workingPathCache = newCache
    } else {
      workingPathCache = pathPartCache
    }
  }

  for (const elementPathPart of fullElementPath) {
    if (elementPathPart.length === 0) {
      // Special cased handling for when the path part is empty
      shiftWorkingCacheRoot('empty-path')
    } else {
      let first: boolean = true
      for (const pathPart of elementPathPart) {
        if (first) {
          shiftWorkingCacheRoot(pathPart)
        } else {
          shiftWorkingCacheChild(pathPart)
        }
        first = false
      }
    }
  }

  return workingPathCache
}

export const SceneSeparator = ':'
export const ElementSeparator = '/'

function getComponentPathStringForPathString(path: string): string | null {
  const indexOfLastSceneSeparator = path.lastIndexOf(SceneSeparator)
  const indexOfLastElementSeparator = path.lastIndexOf(ElementSeparator)
  if (indexOfLastSceneSeparator > indexOfLastElementSeparator) {
    return path.slice(0, indexOfLastSceneSeparator)
  } else {
    return null
  }
}

export function getAllElementPathStringsForPathString(path: string): Array<string> {
  let allElementPathStrings: Array<string> = []
  let workingPath: string | null = path

  while (workingPath != null) {
    allElementPathStrings.push(workingPath)
    workingPath = getComponentPathStringForPathString(workingPath)
  }

  return allElementPathStrings
}

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
  const cachedToString = pathToStringCache.get(target)
  if (cachedToString == null) {
    const stringifiedPath = target.parts.map(elementPathPartToString).join(SceneSeparator)
    pathToStringCache.set(target, stringifiedPath)
    return stringifiedPath
  } else {
    return cachedToString
  }
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
    // This makes `newElementPath` defensive against potentially mutated arrays
    // being passed in, for this small cost that should only be incurred once for
    // each path.
    parts: [...fullElementPath.map((pathPart) => [...pathPart])],
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

export function fromStringStatic(pathString: string): StaticElementPath {
  const path = fromString(pathString)
  return dynamicPathToStaticPath(path)
}

function allElementPathsForPart(path: ElementPathPart): Array<ElementPathPart> {
  let paths: Array<ElementPathPart> = []
  for (var size = 1; size <= path.length; size++) {
    paths.push(path.slice(0, size))
  }
  return paths
}

/**
 * @deprecated use EP.allPathsInsideComponent instead! Reason: this includes the ElementPath of the containing components instance, which is probably not what you want
 */
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

export function allPathsInsideComponent(path: ElementPath | null): Array<ElementPath> {
  if (path == null || isEmptyPath(path)) {
    return []
  } else {
    const prefix = dropLast(path.parts)
    const lastPart = last(path.parts)!
    const toElementPath = (elementPathPart: ElementPathPart) =>
      elementPath([...prefix, elementPathPart])
    return allElementPathsForPart(lastPart).map(toElementPath).reverse()
  }
}

export function depth(path: ElementPath): number {
  return 1 + path.parts.length
}

export function fullDepth(path: ElementPath): number {
  return path.parts.reduce((working, part) => working + part.length, 0)
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

let parentPathCache: Map<ElementPath, ElementPath> = new Map()

export function parentPath(path: StaticElementPath): StaticElementPath
export function parentPath(path: ElementPath): ElementPath
export function parentPath(path: ElementPath): ElementPath {
  const existing = parentPathCache.get(path)
  if (existing == null) {
    const parentFullElementPath = fullElementPathParent(path.parts)
    const result = elementPath(parentFullElementPath)
    parentPathCache.set(path, result)
    return result
  } else {
    return existing
  }
}

export function nthParentPath(path: StaticElementPath, n: number): StaticElementPath
export function nthParentPath(path: ElementPath, n: number): ElementPath
export function nthParentPath(path: ElementPath, n: number): ElementPath {
  let working = path
  for (let i = 0; i < n; i++) {
    working = parentPath(working)
  }
  return working
}

export function isParentComponentOf(maybeParent: ElementPath, maybeChild: ElementPath): boolean {
  return pathsEqual(maybeParent, dropLastPathPart(maybeChild))
}

export function isParentOf(maybeParent: ElementPath, maybeChild: ElementPath): boolean {
  const childLength = maybeChild.parts.length
  const parentLength = maybeParent.parts.length
  if (childLength === parentLength + 1) {
    const lastChildPart = last(maybeChild.parts)
    if (lastChildPart != null && lastChildPart.length === 1) {
      for (let index = 0; index < parentLength; index++) {
        const childParts = maybeChild.parts[index]
        const parentParts = maybeParent.parts[index]
        if (!elementPathPartsEqual(childParts, parentParts)) {
          return false
        }
      }

      // Otherwise this is a parent.
      return true
    } else {
      return false
    }
  } else if (childLength === parentLength) {
    const lastChildPart = last(maybeChild.parts)
    const lastParentPart = last(maybeParent.parts)
    if (
      lastChildPart != null &&
      lastParentPart != null &&
      lastChildPart.length === lastParentPart.length + 1
    ) {
      // Check the main bulk of the parts to ensure those are the same.
      for (let index = 0; index < parentLength - 1; index++) {
        const childParts = maybeChild.parts[index]
        const parentParts = maybeParent.parts[index]
        if (!elementPathPartsEqual(childParts, parentParts)) {
          return false
        }
      }

      // Check the last array part.
      for (let index = 0; index < lastChildPart.length - 1; index++) {
        if (lastChildPart[index] !== lastParentPart[index]) {
          return false
        }
      }

      // Otherwise this is a parent.
      return true
    } else {
      return false
    }
  } else {
    return false
  }
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

export function appendArrayToElementPath(
  path: StaticElementPathPart,
  next: Array<id>,
): StaticElementPathPart
export function appendArrayToElementPath(path: ElementPathPart, next: Array<id>): ElementPathPart
export function appendArrayToElementPath(path: ElementPathPart, next: Array<id>): ElementPathPart {
  return [...path, ...next]
}

export function appendToElementPath(path: StaticElementPathPart, next: id): StaticElementPathPart
export function appendToElementPath(path: ElementPathPart, next: id): ElementPathPart
export function appendToElementPath(path: ElementPathPart, next: id): ElementPathPart {
  return [...path, next]
}

function appendToElementPathArray(pathArray: ElementPathPart[], next: id): ElementPathPart[] {
  if (isEmptyElementPathsArray(pathArray)) {
    return [[next]]
  } else {
    let result: Array<ElementPathPart> = []
    const lengthMinusOne = pathArray.length - 1
    for (let index = 0; index < lengthMinusOne; index++) {
      result.push(pathArray[index])
    }
    result.push(appendToElementPath(pathArray[lengthMinusOne], next))
    return result
  }
}

function appendPartToElementPathArray(
  pathArray: ElementPathPart[],
  next: ElementPathPart,
): ElementPathPart[] {
  if (isEmptyElementPathsArray(pathArray)) {
    return [next]
  } else {
    let result: Array<ElementPathPart> = []
    const lengthMinusOne = pathArray.length - 1
    for (let index = 0; index < lengthMinusOne; index++) {
      result.push(pathArray[index])
    }
    result.push(appendArrayToElementPath(pathArray[lengthMinusOne], next))
    return result
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

export function appendToPath(path: StaticElementPath, next: id): StaticElementPath
export function appendToPath(path: ElementPath, next: id): ElementPath
export function appendToPath(path: ElementPath, next: id): ElementPath {
  const updatedPathParts = appendToElementPathArray(path.parts, next)
  return elementPath(updatedPathParts)
}

export function appendPartToPath(
  path: StaticElementPath,
  next: StaticElementPathPart,
): StaticElementPath
export function appendPartToPath(path: ElementPath, next: ElementPathPart): ElementPath
export function appendPartToPath(path: ElementPath, next: ElementPathPart): ElementPath {
  const updatedPathParts = appendPartToElementPathArray(path.parts, next)
  return elementPath(updatedPathParts)
}

function elementPathPartsEqual(l: ElementPathPart, r: ElementPathPart): boolean {
  return arrayEqualsByReference(l, r)
}

function stringifiedPathsEqual(l: ElementPath, r: ElementPath): boolean {
  return toString(l) === toString(r)
}

export function pathsEqual(l: ElementPath | null, r: ElementPath | null): boolean {
  if (l == null) {
    return r == null
  } else if (r == null) {
    return false
  } else if (l === r) {
    return true
  } else {
    return stringifiedPathsEqual(l, r)
  }
}

export function arrayOfPathsEqual(l: Array<ElementPath>, r: Array<ElementPath>): boolean {
  return arrayEqualsByValue(l, r, pathsEqual)
}

export function containsPath(path: ElementPath, paths: Array<ElementPath>): boolean {
  for (const toCheck of paths) {
    if (pathsEqual(toCheck, path)) {
      return true
    }
  }
  return false
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

export function isRootElementOf(path: ElementPath | null, parent: ElementPath | null): boolean {
  return path != null && isChildOf(path, parent) && isRootElementOfInstance(path)
}

export function isSiblingOf(l: ElementPath | null, r: ElementPath | null): boolean {
  return l != null && r != null && pathsEqual(parentPath(l), parentPath(r))
}

export function areSiblings(paths: Array<ElementPath>): boolean {
  return paths.every((p) => isSiblingOf(paths[0], p))
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

export function findAmongAncestorsOfPath<T>(
  path: ElementPath,
  fn: (ancestor: ElementPath) => T | null,
): T | null {
  const ancestors = getAncestors(path)
  for (const ancestor of ancestors) {
    const maybeFound = fn(ancestor)
    if (maybeFound != null) {
      return maybeFound
    }
  }
  return null
}

export function isDescendantOf(target: ElementPath, maybeAncestor: ElementPath): boolean {
  const targetElementPath = target.parts
  const maybeAncestorElementPath = maybeAncestor.parts
  if (targetElementPath.length >= maybeAncestorElementPath.length) {
    for (let pathPartIndex = 0; pathPartIndex < maybeAncestorElementPath.length; pathPartIndex++) {
      const elementPathPart = targetElementPath[pathPartIndex]
      // all parts up to the last must match, and the last must be a descendant
      const maybeAncestorElementPathPart = maybeAncestorElementPath[pathPartIndex]
      if (pathPartIndex < maybeAncestorElementPath.length - 1) {
        if (!elementPathPartsEqual(elementPathPart, maybeAncestorElementPathPart)) {
          return false
        }
      } else {
        if (targetElementPath.length === maybeAncestorElementPath.length) {
          if (!elementIsDescendant(elementPathPart, maybeAncestorElementPathPart)) {
            return false
          }
        } else {
          if (!elementIsDescendantOrEqualTo(elementPathPart, maybeAncestorElementPathPart)) {
            return false
          }
        }
      }
    }
    return true
  } else {
    return false
  }
}

export function getAncestorsForLastPart(path: ElementPath): ElementPath[] {
  return allPathsForLastPart(path).slice(0, -1)
}

export function getAncestors(path: ElementPath): ElementPath[] {
  let workingPath = parentPath(path)
  let ancestors = [workingPath]

  while (!isEmptyPath(workingPath)) {
    workingPath = parentPath(workingPath)
    ancestors.push(workingPath)
  }

  return ancestors
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

export function dropNPathParts(path: ElementPath, n: number): ElementPath {
  if (n <= 0) {
    return path
  }
  return dropNPathParts(parentPath(path), n - 1)
}

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
    const dropEntireLastPart = overlappingPart.length === lastReplaceSearchPartLength
    const trimmedOverlappingPart =
      overlappingPart == null || dropEntireLastPart
        ? null
        : drop(lastReplaceSearchPartLength, overlappingPart)

    let updatedPathParts: ElementPathPart[] = []
    if (trimmedOverlappingPart == null) {
      if (replaceWith != null) {
        updatedPathParts.push(...replaceWith.parts)
      }
    } else if (replaceWith == null) {
      updatedPathParts.push(trimmedOverlappingPart)
    } else {
      updatedPathParts.push(
        ...appendPartToElementPathArray(replaceWith.parts, trimmedOverlappingPart),
      )
    }

    updatedPathParts.push(...suffix)
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

// TODO: remove null
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

export function getCommonParentOfNonemptyPathArray(
  paths: NonEmptyArray<ElementPath>,
  includeSelf: boolean = false,
): ElementPath {
  const parents = includeSelf ? paths : paths.map(parentPath)
  return parents.reduce<ElementPath>(
    (l, r) => closestSharedAncestor(l, r, true) ?? emptyElementPath,
    parents[0],
  )
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

export function isDynamicPath(path: ElementPath): boolean {
  return !pathsEqual(path, dynamicPathToStaticPath(path))
}

export function makeLastPartOfPathStatic(path: ElementPath): ElementPath {
  const existing = dynamicToStaticLastElementPathPartCache.get(path)
  if (existing == null) {
    const dynamicLastPart = last(path.parts)
    let result: ElementPath
    if (dynamicLastPart == null) {
      result = path
    } else {
      const staticLastPart = dynamicElementPathToStaticElementPath(dynamicLastPart)
      result = elementPath([...dropLast(path.parts), staticLastPart])
    }
    dynamicToStaticLastElementPathPartCache.set(path, result)
    return result
  } else {
    return existing
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
  newPath: ElementPath | null
  droppedPathElements: ElementPathPart | null
}

export function dropFirstPathElement(path: ElementPath | null): DropFirstPathElementResultType {
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

export function takeLastPartOfPath(path: ElementPath): ElementPath {
  if (path?.parts.length > 1) {
    const lastPart = last(path.parts)!
    return elementPath([lastPart])
  }

  return path
}

export function createBackwardsCompatibleScenePath(path: ElementPath): ElementPath {
  const firstPart = path.parts[0] ?? emptyElementPathPart
  return elementPath([firstPart.slice(0, 2)]) // we only return the FIRST TWO elements (storyboard, scene), this is a horrible, horrible hack
}

export function isFocused(focusedElementPath: ElementPath | null, path: ElementPath): boolean {
  const lastPart = lastElementPathForPath(path)
  if (focusedElementPath == null || lastPart == null) {
    return false
  } else {
    // FIXME this is incorrect, as it will be marking other instances as focused
    return pathUpToElementPath(focusedElementPath, lastPart, 'dynamic-path') != null
  }
}

export function isExplicitlyFocused(
  focusedElementPath: ElementPath | null,
  autoFocusedPaths: Array<ElementPath>,
  path: ElementPath,
): boolean {
  if (pathsEqual(focusedElementPath, path)) {
    return true
  }

  const isNotAutoFocused =
    path.parts.length > 1 ||
    !autoFocusedPaths.some((autoFocusedPath) => isDescendantOfOrEqualTo(path, autoFocusedPath))
  return isNotAutoFocused && isFocused(focusedElementPath, path)
}

export function isInExplicitlyFocusedSubtree(
  focusedElementPath: ElementPath | null,
  autoFocusedPaths: Array<ElementPath>,
  path: ElementPath,
): boolean {
  // we exclude children of a potentially focused component as children are not part of the focus system
  const componentToCheck = dropLastPathPart(path)
  // the focusedElementPath can contain multiple focused components along its path parts
  // if the path to check is a descendant of any of the focused components, it is considered focused
  let focusedAncestor = focusedElementPath
  while (
    focusedAncestor != null &&
    !isEmptyPath(focusedAncestor) &&
    !isStoryboardDescendant(focusedAncestor) &&
    // since there is a single focusedElementPath by default we considered all ancestors as focused
    // but if an ancestor is also autofocused, we should not consider it as explicitly focused
    !autoFocusedPaths.some((autoFocusedPath) => pathsEqual(autoFocusedPath, focusedAncestor))
  ) {
    if (isDescendantOfOrEqualTo(componentToCheck, focusedAncestor)) {
      return true
    }
    focusedAncestor = getContainingComponent(focusedAncestor)
  }
  return false
}

export function getOrderedPathsByDepth(elementPaths: Array<ElementPath>): Array<ElementPath> {
  return elementPaths.slice().sort((a, b) => {
    if (depth(b) === depth(a)) {
      const aInnerDepth = last(a.parts)?.length ?? 0
      const bInnerDepth = last(b.parts)?.length ?? 0
      return bInnerDepth - aInnerDepth
    }

    return depth(b) - depth(a)
  })
}

export function emptyStaticElementPathPart(): StaticElementPathPart {
  return [] as unknown as StaticElementPathPart
}

export function getContainingComponent(path: ElementPath): ElementPath {
  return dropLastPathPart(path)
}

export function uniqueElementPaths(paths: Array<ElementPath>): Array<ElementPath> {
  const pathSet = new Set(paths.map(toString))
  return Array.from(pathSet).map(fromString)
}

export const GeneratedUIDSeparator = `~~~`
export function createIndexedUid(originalUid: string, index: string | number): string {
  return `${originalUid}${GeneratedUIDSeparator}${index}`
}

export function extractOriginalUidFromIndexedUid(uid: string): string {
  const separatorIndex = uid.indexOf(GeneratedUIDSeparator)
  if (separatorIndex >= 0) {
    return uid.substr(0, separatorIndex)
  } else {
    return uid
  }
}

export function getStoryboardPathFromPath(path: ElementPath): ElementPath | null {
  if (isEmptyPath(path)) {
    return null
  }
  return fromString(path.parts[0][0])
}

export function appendTwoPaths(base: ElementPath, other: ElementPath): ElementPath {
  return elementPath([...base.parts, ...other.parts])
}

export function multiplePathsAllWithTheSameUID(paths: Array<ElementPath>): boolean {
  if (paths.length <= 1) {
    return false
  }

  const firstUid = toUid(paths[0])
  if (paths.every((v) => toUid(v) === firstUid)) {
    return true
  }
  return false
}
