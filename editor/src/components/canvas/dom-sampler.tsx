import type {
  ComputedStyle,
  DetectedLayoutSystem,
  ElementPath,
  StyleAttributeMetadata,
} from 'utopia-shared/src/types'
import {
  fillMissingDataFromAncestors,
  MetadataUtils,
} from '../../core/model/element-metadata-utils'
import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import { allElemsEqual, mapDropNulls, pluck } from '../../core/shared/array-utils'
import { getCanvasRectangleFromElement } from '../../core/shared/dom-utils'
import { alternativeEither, left } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import {
  domElementMetadata,
  emptySpecialSizeMeasurements,
  type ComputedStyleMetadata,
  type DomElementMetadata,
  type ElementInstanceMetadata,
  type ElementInstanceMetadataMap,
} from '../../core/shared/element-template'
import type { CanvasRectangle } from '../../core/shared/math-utils'
import {
  boundingRectangleArray,
  infinityCanvasRectangle,
  infinityLocalRectangle,
  nullIfInfinity,
  type CanvasPoint,
} from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { camelCaseToDashed } from '../../core/shared/string-utils'
import { getPathWithStringsOnDomElement } from '../../core/shared/uid-utils'
import type { ElementsToRerender } from '../editor/store/editor-state'
import type { CSSPosition, FlexDirection } from '../inspector/common/css-utils'
import { computedStyleKeys } from '../inspector/common/css-utils'
import { CanvasContainerID } from './canvas-types'
import {
  collectDomElementMetadataForElement,
  getAttributesComingFromStyleSheets,
} from './dom-walker'
import type { UiJsxCanvasContextData } from './ui-jsx-canvas'

function collectMetadataForElement(
  foundElement: HTMLElement,
  path: ElementPath,
  validPaths: Array<ElementPath>,
  selectedViews: Array<ElementPath>,
  scale: number,
  containerRect: CanvasPoint,
  spyCollector: UiJsxCanvasContextData,
): DomElementMetadata {
  const pathsWithStrings = getPathWithStringsOnDomElement(foundElement)
  if (pathsWithStrings.length == 0) {
    throw new Error('No path found on element')
  }
  const foundValidPaths = pathsWithStrings.filter((pathWithString) => {
    const staticPath = EP.makeLastPartOfPathStatic(pathWithString.path)
    return validPaths.some((vp) => EP.pathsEqual(vp, staticPath)) // this is from the old implementation, no descendants are included
  })

  const foundValidPathsMatchOriginalPath = foundValidPaths.some((vp) =>
    EP.pathsEqual(EP.makeLastPartOfPathStatic(vp.path), EP.makeLastPartOfPathStatic(path)),
  )

  const foundElementIsNotRealDomElement = !foundValidPathsMatchOriginalPath

  const invalidatedPathForceRecalculation =
    spyCollector.current.spyValues.invalidatedElementsInFrame.some((i) =>
      EP.isDescendantOfOrEqualTo(path, i),
    )

  const metadataResult = createElementInstanceMetadataForElementCached.get(
    invalidatedPathForceRecalculation,
    foundElement,
    scale,
    containerRect.x, // passing this as two values so it can be used as cache key
    containerRect.y,
  )

  const metadata: DomElementMetadata = {
    // TODO instead of shallow cloning the metadata and then modifying it, we should pass in to this function the fact that we are collecting for a non-dom element
    ...metadataResult,
    specialSizeMeasurements: {
      ...metadataResult.specialSizeMeasurements,
    },
  }
  const computedStyle = getComputedStyleOptionallyForElement(
    invalidatedPathForceRecalculation,
    foundElement,
    pluck(foundValidPaths, 'path'),
    selectedViews,
  )

  if (foundElementIsNotRealDomElement) {
    // TODO this should not be in the metadata to begin with
    // TODO express with types refactoring that this only applies to non-dom elements
    // if the element is not a real dom element, we need to clear out the layout system for children
    metadata.specialSizeMeasurements.layoutSystemForChildren = null
    metadata.specialSizeMeasurements.globalContentBoxForChildren = null
  }

  if (computedStyle != null) {
    metadata.computedStyle = computedStyle.computedStyle
    metadata.attributeMetadatada = computedStyle.attributeMetadatada
  }

  return metadata
}

function createSyntheticDomElementMetadataForMultipleClosestMatches(
  closestMatches: Array<HTMLElement>,
  path: ElementPath,
  scale: number,
  containerRect: CanvasPoint,
  spyCollector: UiJsxCanvasContextData,
): DomElementMetadata {
  const metadatas: Array<DomElementMetadata> = mapDropNulls((el) => {
    if (!(el instanceof HTMLElement)) {
      return null
    }
    const invalidatedPathForceCacheReset =
      spyCollector.current.spyValues.invalidatedElementsInFrame.some((i) => EP.pathsEqual(i, path))

    return createElementInstanceMetadataForElementCached.get(
      invalidatedPathForceCacheReset,
      el,
      scale,
      containerRect.x,
      containerRect.y,
    )
  }, closestMatches)

  const mergedGlobalFrame = boundingRectangleArray(
    mapDropNulls((m) => nullIfInfinity(m.globalFrame), metadatas),
  )

  // TODO instead of these expensive little plucks, we should create the values in a single loop
  const parentLayoutSystemFromChildren: Array<DetectedLayoutSystem> = metadatas.map(
    (c) => c.specialSizeMeasurements.parentLayoutSystem,
  )
  const parentFlexDirectionFromChildren: Array<FlexDirection | null> = metadatas.map(
    (c) => c.specialSizeMeasurements.parentFlexDirection,
  )
  const immediateParentBoundsFromChildren: Array<CanvasRectangle | null> = metadatas.map(
    (c) => c.specialSizeMeasurements.immediateParentBounds,
  )
  const positionForChildren: Array<CSSPosition | null> = metadatas.map(
    (c) => c.specialSizeMeasurements.position,
  )

  return domElementMetadata(
    left('unknown'),
    mergedGlobalFrame,
    mergedGlobalFrame,
    {
      ...emptySpecialSizeMeasurements,
      parentLayoutSystem: allElemsEqual(parentLayoutSystemFromChildren)
        ? parentLayoutSystemFromChildren[0]
        : 'none',
      parentFlexDirection: allElemsEqual(parentFlexDirectionFromChildren)
        ? parentFlexDirectionFromChildren[0]
        : null,
      immediateParentBounds: allElemsEqual(immediateParentBoundsFromChildren)
        ? immediateParentBoundsFromChildren[0]
        : null,
      position: allElemsEqual(positionForChildren) ? positionForChildren[0] : null,
    },
    null,
  )
}

function createFakeMetadataForCanvasRoot(canvasRootPath: ElementPath): DomElementMetadata {
  return domElementMetadata(
    left('Storyboard'),
    infinityCanvasRectangle, // TODO the canvas is not actually sized infinity, in the future we should remove the InfinityRectangle and use the actual size
    infinityCanvasRectangle,
    emptySpecialSizeMeasurements,
    null,
  )
}

function collectMetadataForElementPath(
  path: ElementPath,
  validPaths: Array<ElementPath>,
  selectedViews: Array<ElementPath>,
  scale: number,
  containerRect: CanvasPoint,
  spyCollector: UiJsxCanvasContextData,
): DomElementMetadata | null {
  if (EP.isStoryboardPath(path)) {
    return createFakeMetadataForCanvasRoot(path)
  }

  const foundElements = document.querySelectorAll(`[${UTOPIA_PATH_KEY}^="${EP.toString(path)}"]`)

  let closestMatches: Array<HTMLElement> = []
  let foundElementDepth: number = Infinity

  for (let index = 0; index < foundElements.length; index++) {
    const el = foundElements[index]
    if (!(el instanceof HTMLElement)) {
      continue
    }

    if (closestMatches.length === 0 || getDomElementDepth(el) < foundElementDepth) {
      closestMatches = [el]
      foundElementDepth = getDomElementDepth(el)
    } else if (getDomElementDepth(el) === foundElementDepth) {
      closestMatches.push(el)
    }
  }

  if (closestMatches.length == 0) {
    return null
  } else if (closestMatches.length == 1) {
    return collectMetadataForElement(
      closestMatches[0],
      path,
      validPaths,
      selectedViews,
      scale,
      containerRect,
      spyCollector,
    )
  } else {
    // if there are multiple closestMatches that are the same depth, we want to return a synthetic metadata with a globalFrame that is the union of all the closestMatches
    return createSyntheticDomElementMetadataForMultipleClosestMatches(
      closestMatches,
      path,
      scale,
      containerRect,
      spyCollector,
    )
  }
}

function getValidPathsFromCanvasContainer(canvasRootContainer: HTMLElement): Array<ElementPath> {
  const validPaths: Array<ElementPath> | null = optionalMap(
    (paths) => paths.split(' ').map(EP.fromString),
    canvasRootContainer.getAttribute('data-utopia-valid-paths'),
  )

  if (validPaths == null) {
    throw new Error(
      'Utopia Internal Error: Running DOM-walker without canvasRootPath or validRootPaths',
    )
  }

  return validPaths
}

function collectMetadataForPaths(
  canvasRootContainer: HTMLElement,
  pathsToCollect: Array<ElementPath>,
  validPaths: Array<ElementPath>,
  metadataToUpdate_MUTATE: ElementInstanceMetadataMap,
  options: {
    scale: number
    selectedViews: Array<ElementPath>
    spyCollector: UiJsxCanvasContextData
    spyPaths: Array<string>
  },
): {
  metadata: ElementInstanceMetadataMap
  tree: ElementPathTrees
} {
  const containerRect = getCanvasRectangleFromElement(
    canvasRootContainer,
    options.scale,
    'without-text-content',
    'nearest-half',
  )

  const dynamicPathsToCollect: Array<ElementPath> = pathsToCollect
    .flatMap((staticPath) => {
      return options.spyPaths.filter((spyPath) =>
        EP.pathsEqual(EP.makeLastPartOfPathStatic(EP.fromString(spyPath)), staticPath),
      )
    })
    .map(EP.fromString)

  dynamicPathsToCollect.forEach((path) => {
    const domMetadata = collectMetadataForElementPath(
      path,
      validPaths,
      options.selectedViews,
      options.scale,
      containerRect,
      options.spyCollector,
    )

    if (domMetadata == null) {
      // if we couldn't find any dom elements for the path, we must scan through all the spy elements to find a fallback with a potentially dynamic path
      const spyElem = options.spyCollector.current.spyValues.metadata[EP.toString(path)]
      if (spyElem != null) {
        metadataToUpdate_MUTATE[EP.toString(path)] = {
          ...spyElem,
        }
      }
      return // we couldn't find a fallback spy element, so we bail out
    }

    const validDynamicPath = path
    const spyMetadata =
      options.spyCollector.current.spyValues.metadata[EP.toString(validDynamicPath)]
    if (spyMetadata == null) {
      // if the element is missing from the spyMetadata, we bail out. this is the same behavior as the old reconstructJSXMetadata implementation
      return
    }

    let jsxElement = alternativeEither(spyMetadata.element, domMetadata.element)

    // TODO avoid temporary object creation
    const elementInstanceMetadata: ElementInstanceMetadata = {
      ...domMetadata,
      element: jsxElement,
      elementPath: spyMetadata.elementPath,
      componentInstance: spyMetadata.componentInstance,
      isEmotionOrStyledComponent: spyMetadata.isEmotionOrStyledComponent,
      label: spyMetadata.label,
      importInfo: spyMetadata.importInfo,
      assignedToProp: spyMetadata.assignedToProp,
      conditionValue: spyMetadata.conditionValue,
      earlyReturn: spyMetadata.earlyReturn,
    }
    metadataToUpdate_MUTATE[EP.toString(spyMetadata.elementPath)] = elementInstanceMetadata
  })

  const finalMetadata = [
    pruneInvalidPathsFromMetadata_MUTATE(validPaths),
    fillMissingDataFromAncestors,
  ].reduce((metadata, fix) => fix(metadata), metadataToUpdate_MUTATE)

  return {
    metadata: finalMetadata,
    tree: MetadataUtils.createElementPathTreeFromMetadata(finalMetadata),
  }
}

export function collectMetadata(
  elementsToFocusOn: ElementsToRerender,
  options: {
    scale: number
    selectedViews: Array<ElementPath>
    metadataToUpdate: ElementInstanceMetadataMap
    spyCollector: UiJsxCanvasContextData
  },
): { metadata: ElementInstanceMetadataMap; tree: ElementPathTrees } {
  getComputedStylesCache.updateObservers()
  createElementInstanceMetadataForElementCached.updateObservers()

  const canvasRootContainer = document.getElementById(CanvasContainerID)
  if (canvasRootContainer == null) {
    return {
      metadata: options.metadataToUpdate,
      tree: MetadataUtils.createElementPathTreeFromMetadata(options.metadataToUpdate), // TODO this should return a cached tree
    }
  }

  const validPaths = getValidPathsFromCanvasContainer(canvasRootContainer)

  const spyPaths = Object.keys(options.spyCollector.current.spyValues.metadata)
  if (spyPaths.length === 0 && validPaths.length > 0) {
    // if there are no spy elements, but we have valid paths, we probably encountered a transient case during a canvas runtime error
    // the intended behavior of the editor during a runtime error is to preserve the existing metadata to avoid flickering
    // so we return the existing metadata in that case
    return {
      metadata: options.metadataToUpdate,
      tree: MetadataUtils.createElementPathTreeFromMetadata(options.metadataToUpdate), // TODO this should return a cached tree
    }
  }

  let result
  if (elementsToFocusOn == 'rerender-all-elements') {
    result = collectMetadataForPaths(
      canvasRootContainer,
      validPaths,
      validPaths,
      {},
      { ...options, spyPaths: spyPaths },
    )
  } else {
    result = collectMetadataForPaths(
      canvasRootContainer,
      validPaths.filter((vp) =>
        elementsToFocusOn.some((e) => {
          const staticElement = EP.makeLastPartOfPathStatic(e)
          return EP.pathsEqual(staticElement, vp) || EP.isParentOf(staticElement, vp)
        }),
      ),
      validPaths,
      { ...options.metadataToUpdate },
      { ...options, spyPaths: spyPaths },
    )
  }

  return result
}

const ObserversAvailable = window.MutationObserver != null && window.ResizeObserver != null

const MutationObserverConfig = { attributes: true, childList: true, subtree: true }

export class ObserverCache<T, N extends Element = Element, A extends Array<any> = Array<any>> {
  private cache = new WeakMap<N, { value: T; params: A }>()

  private getter: (node: N, ...args: A) => T

  private handleMutation = (mutations: Array<MutationRecord>) => {
    // delete the metadata for the element that has been mutated
    for (const mutation of mutations) {
      // TODO this may be a little excessive, possibly tone it down
      this.cache.delete(mutation.target as N)
      mutation.addedNodes.forEach((n) => this.cache.delete(n as N))
      mutation.removedNodes.forEach((n) => this.cache.delete(n as N))
      // remove all children of the mutation target from the cache
      mutation.target.childNodes.forEach((n) => this.cache.delete(n as N))
    }
  }
  private handleResize = (entries: Array<ResizeObserverEntry>) => {
    // delete the metadata for the element that has been resized
    for (const entry of entries) {
      const target = entry.target
      this.cache.delete(target as N)
      // invalidate all siblings as a resize of one element can affect the layout of its siblings
      entry.target.parentElement?.childNodes.forEach((n) => this.cache.delete(n as N))
    }
  }

  private mutationObserver: MutationObserver | null = null
  private resizeObserver: ResizeObserver | null = null

  constructor(getter: (node: N, ...args: A) => T) {
    this.getter = getter
    if (ObserversAvailable) {
      this.mutationObserver = new MutationObserver(this.handleMutation)
      this.resizeObserver = new ResizeObserver(this.handleResize)
    }
  }

  public updateObservers() {
    const canvasRootContainer = document.getElementById(CanvasContainerID)
    if (
      canvasRootContainer != null &&
      this.resizeObserver != null &&
      this.mutationObserver != null
    ) {
      document.querySelectorAll(`#${CanvasContainerID} *`).forEach((elem) => {
        this.resizeObserver!.observe(elem)
      })
      this.mutationObserver.observe(canvasRootContainer, MutationObserverConfig)
    }
  }

  // first parameter is the Node we are weakMap caching on
  public get(force: boolean, node: N, ...args: A): T {
    const cacheResult = this.cache.get(node)

    // if the cache is empty, or the parameters have changed, recompute the value
    if (
      force ||
      !ObserversAvailable ||
      cacheResult == null ||
      cacheResult.params.some((param, index) => param !== args[index])
    ) {
      const value = this.getter(node, ...args)
      this.cache.set(node, { value: value, params: args })
      return value
    }
    return cacheResult.value
  }
}

const getComputedStylesCache = new ObserverCache(getComputedStyleForElement)

const createElementInstanceMetadataForElementCached = new ObserverCache(
  collectDomElementMetadataForElement,
)

function getComputedStyleOptionallyForElement(
  force: boolean,
  element: HTMLElement,
  paths: Array<ElementPath>,
  selectedViews: Array<ElementPath>,
): ComputedStyleMetadata | null {
  const isSelectedOnAnyPaths = selectedViews.some((sv) =>
    paths.some((path) => EP.pathsEqual(sv, path)),
  )
  if (!isSelectedOnAnyPaths) {
    // the element is not among the selected views, skip computing the style
    return null
  }

  return getComputedStylesCache.get(force, element)
}

function getComputedStyleForElement(element: HTMLElement): ComputedStyleMetadata {
  const elementStyle = window.getComputedStyle(element)
  const attributesSetByStylesheet = getAttributesComingFromStyleSheets(element)
  let computedStyle: ComputedStyle = {}
  let attributeMetadata: StyleAttributeMetadata = {}
  if (elementStyle != null) {
    computedStyleKeys.forEach((key) => {
      // Accessing the value directly often doesn't work, and using `getPropertyValue` requires
      // using dashed case rather than camel case
      const caseCorrectedKey = camelCaseToDashed(key)
      const propertyValue = elementStyle.getPropertyValue(caseCorrectedKey)
      if (propertyValue != '') {
        computedStyle[key] = propertyValue
        const isSetFromStyleSheet = attributesSetByStylesheet.has(key)
        if (isSetFromStyleSheet) {
          attributeMetadata[key] = { fromStyleSheet: true }
        }
      }
    })
  }

  return {
    computedStyle: computedStyle,
    attributeMetadatada: attributeMetadata,
  }
}

const pruneInvalidPathsFromMetadata_MUTATE =
  (validPaths: Array<ElementPath>) =>
  (metadata_MUTATE: ElementInstanceMetadataMap): ElementInstanceMetadataMap => {
    const validPathsSet = new Set(validPaths.map(EP.toString))
    const keys = Object.keys(metadata_MUTATE)
    for (const key of keys) {
      const staticPath = EP.toString(EP.makeLastPartOfPathStatic(EP.fromString(key)))
      if (!validPathsSet.has(staticPath)) {
        delete metadata_MUTATE[key]
      }
    }
    return metadata_MUTATE
  }

function getDomElementDepth(element: HTMLElement): number {
  let depth = 0
  let currentElement = element
  while (currentElement.parentElement != null) {
    depth += 1
    currentElement = currentElement.parentElement
  }
  return depth
}
