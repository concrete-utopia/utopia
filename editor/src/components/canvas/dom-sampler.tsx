import type { ComputedStyle, ElementPath, StyleAttributeMetadata } from 'utopia-shared/src/types'
import {
  fillMissingDataFromAncestors,
  MetadataUtils,
} from '../../core/model/element-metadata-utils'
import { UTOPIA_PATH_KEY, UTOPIA_STATIC_PATH_KEY } from '../../core/model/utopia-constants'
import { mapDropNulls, pluck } from '../../core/shared/array-utils'
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
import { computedStyleKeys } from '../inspector/common/css-utils'
import { CanvasContainerID } from './canvas-types'
import {
  createElementInstanceMetadataForElement,
  getAttributesComingFromStyleSheets,
} from './dom-walker'
import type { UiJsxCanvasContextData } from './ui-jsx-canvas'
import { DefaultMap } from 'mnemonist'

function collectMetadataForElementPath(
  path: ElementPath,
  validPaths: Array<ElementPath>,
  selectedViews: Array<ElementPath>,
  scale: number,
  containerRect: CanvasPoint,
): Array<{ metadata: DomElementMetadata; foundValidDynamicPaths: Array<ElementPath> }> {
  if (EP.isStoryboardPath(path)) {
    return [{ metadata: createFakeMetadataForCanvasRoot(path), foundValidDynamicPaths: [path] }]
  }

  const foundElements = document.querySelectorAll(
    `[${UTOPIA_STATIC_PATH_KEY}^="${EP.toString(path)}"]`,
  )

  const elementsToCollectFor = new DefaultMap<
    string,
    {
      closestMatches: Array<HTMLElement>
      foundElementPathDepth: number
      foundElementPathFullDepth: number
    }
  >(() => ({ closestMatches: [], foundElementPathDepth: 0, foundElementPathFullDepth: 0 }))

  for (let index = 0; index < foundElements.length; index++) {
    const el = foundElements[index]
    if (!(el instanceof HTMLElement)) {
      continue
    }

    const ep = EP.fromString(el.getAttribute(UTOPIA_PATH_KEY) ?? '')

    const shortenedFoundPath = EP.pathShortenedToOtherPath(ep, path)

    if (
      EP.pathsEqual(
        EP.dynamicPathToStaticPath(shortenedFoundPath),
        EP.dynamicPathToStaticPath(path),
      )
    ) {
      const { closestMatches, foundElementPathDepth, foundElementPathFullDepth } =
        elementsToCollectFor.get(EP.toString(shortenedFoundPath))

      if (closestMatches.length === 0) {
        elementsToCollectFor.set(EP.toString(shortenedFoundPath), {
          closestMatches: [el],
          foundElementPathDepth: EP.depth(ep),
          foundElementPathFullDepth: EP.fullDepth(ep),
        })
      } else {
        if (
          EP.depth(ep) === foundElementPathDepth &&
          EP.fullDepth(ep) === foundElementPathFullDepth
        ) {
          closestMatches.push(el)
        }
      }
    }
  }

  return Array.from(elementsToCollectFor.entries()).map(
    ([dynamicPathString, { closestMatches }]) => {
      const dynamicPath = EP.fromString(dynamicPathString)

      if (closestMatches.length == 1) {
        const foundElement = closestMatches[0]
        // TODO handle measuring SVGs
        const pathsWithStrings = getPathWithStringsOnDomElement(foundElement)
        if (pathsWithStrings.length == 0) {
          throw new Error('No path found on element')
        } else {
          const foundValidPaths = pathsWithStrings.filter((pathWithString) => {
            const staticPath = EP.makeLastPartOfPathStatic(pathWithString.path)
            return validPaths.some((vp) => EP.pathsEqual(vp, staticPath)) // this is from the old implementation, no descendants are included
          })

          const metadata = createElementInstanceMetadataForElementCached.get(
            foundElement,
            scale,
            containerRect.x, // passing this as two values so it can be used as cache key
            containerRect.y,
          )
          const computedStyle = getComputedStyleOptionallyForElement(
            foundElement,
            pluck(foundValidPaths, 'path'),
            selectedViews,
          )

          if (computedStyle != null) {
            metadata.computedStyle = computedStyle.computedStyle
            metadata.attributeMetadatada = computedStyle.attributeMetadatada
          }

          return { metadata: metadata, foundValidDynamicPaths: [dynamicPath] }
        }
      }

      // if there are multiple closestMatches that are the same depth, we want to return a fake metadata with a globalFrame that is the union of all the closestMatches
      const metadatas: Array<DomElementMetadata> = mapDropNulls((el) => {
        if (!(el instanceof HTMLElement)) {
          return null
        }
        return createElementInstanceMetadataForElementCached.get(
          el,
          scale,
          containerRect.x,
          containerRect.y,
        )
      }, closestMatches)

      const mergedGlobalFrame = boundingRectangleArray(
        mapDropNulls((m) => nullIfInfinity(m.globalFrame), metadatas),
      )

      return {
        metadata: domElementMetadata(
          left('unknown'),
          mergedGlobalFrame,
          null,
          mergedGlobalFrame,
          emptySpecialSizeMeasurements,
          null,
        ),
        foundValidDynamicPaths: [dynamicPath],
      }
    },
  )
}

function createFakeMetadataForCanvasRoot(canvasRootPath: ElementPath): DomElementMetadata {
  return domElementMetadata(
    left('Storyboard'),
    infinityCanvasRectangle,
    infinityLocalRectangle,
    infinityCanvasRectangle,
    emptySpecialSizeMeasurements,
    null,
  )
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

function mergeSpyMetadata(path: ElementPath, domMetadata_MUTATE: DomElementMetadata) {}

function collectMetadataForPaths(
  canvasRootContainer: HTMLElement,
  pathsToCollect: Array<ElementPath>,
  validPaths: Array<ElementPath>,
  metadataToUpdate_MUTATE: ElementInstanceMetadataMap,
  options: {
    scale: number
    selectedViews: Array<ElementPath>
    spyCollector: UiJsxCanvasContextData
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

  pathsToCollect.forEach((path) => {
    const domMetadata = collectMetadataForElementPath(
      path,
      validPaths,
      options.selectedViews,
      options.scale,
      containerRect,
    )
    if (domMetadata.length == 0) {
      // TODO find a dynamic spyElement for this path
      const spyElem = options.spyCollector.current.spyValues.metadata[EP.toString(path)]
      if (spyElem != null) {
        metadataToUpdate_MUTATE[EP.toString(path)] = {
          ...spyElem,
        }
      }
      return // we couldn't find a fallback spy element, so we bail out
    }

    domMetadata.forEach(({ metadata, foundValidDynamicPaths }) => {
      foundValidDynamicPaths.forEach((validDynamicPath) => {
        const spyElem =
          options.spyCollector.current.spyValues.metadata[EP.toString(validDynamicPath)]
        if (spyElem == null) {
          // if the element is missing from the spyMetadata, we bail out. this is the same behavior as the old reconstructJSXMetadata implementation
          return
        }

        let jsxElement = alternativeEither(spyElem.element, metadata.element)

        // TODO avoid temporary object creation
        const elementInstanceMetadata: ElementInstanceMetadata = {
          ...metadata,
          element: jsxElement,
          elementPath: spyElem.elementPath,
          componentInstance: spyElem.componentInstance,
          isEmotionOrStyledComponent: spyElem.isEmotionOrStyledComponent,
          label: spyElem.label,
          importInfo: spyElem.importInfo,
          assignedToProp: spyElem.assignedToProp,
          conditionValue: spyElem.conditionValue,
          earlyReturn: spyElem.earlyReturn,
        }
        metadataToUpdate_MUTATE[EP.toString(spyElem.elementPath)] = elementInstanceMetadata
      })
    })
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

  if (elementsToFocusOn == 'rerender-all-elements') {
    return collectMetadataForPaths(canvasRootContainer, validPaths, validPaths, {}, options)
  } else {
    return collectMetadataForPaths(
      canvasRootContainer,
      elementsToFocusOn,
      validPaths,
      { ...options.metadataToUpdate },
      options,
    )
  }
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
  public get(node: N, ...args: A): T {
    const cacheResult = this.cache.get(node)

    // if the cache is empty, or the parameters have changed, recompute the value
    if (
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
  createElementInstanceMetadataForElement,
)

function getComputedStyleOptionallyForElement(
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

  return getComputedStylesCache.get(element)
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
