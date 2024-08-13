import type { ComputedStyle, ElementPath, StyleAttributeMetadata } from 'utopia-shared/src/types'
import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import { pluck } from '../../core/shared/array-utils'
import { getCanvasRectangleFromElement } from '../../core/shared/dom-utils'
import * as EP from '../../core/shared/element-path'
import type { ComputedStyleMetadata, DomElementMetadata } from '../../core/shared/element-template'
import type { CanvasPoint } from '../../core/shared/math-utils'
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

function collectMetadataForElementPath(
  path: ElementPath,
  validPaths: Array<ElementPath>,
  selectedViews: Array<ElementPath>,
  scale: number,
  containerRect: CanvasPoint,
): { metadata: DomElementMetadata; computedStyle: ComputedStyleMetadata | null } | null {
  const foundElement = document.querySelector(
    `[${UTOPIA_PATH_KEY}^="${EP.toString(path)}"]`,
  ) as HTMLElement | null

  if (foundElement != null) {
    // TODO handle measuring SVGs
    if (foundElement instanceof HTMLElement) {
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

        return { metadata: metadata, computedStyle: computedStyle }
      }
    }
    return null
  }

  return null
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
  options: {
    scale: number
    selectedViews: Array<ElementPath>
    metadataToUpdate: { [path: string]: DomElementMetadata }
    computedStylesToUpdate: { [path: string]: ComputedStyleMetadata }
  },
): {
  metadata: { [path: string]: DomElementMetadata }
  computedStyle: { [path: string]: ComputedStyleMetadata }
} {
  const containerRect = getCanvasRectangleFromElement(
    canvasRootContainer,
    options.scale,
    'without-text-content',
    'nearest-half',
  )

  let updatedMetadataMap = { ...options.metadataToUpdate }
  let updatedComputedStyleMap = { ...options.computedStylesToUpdate }

  pathsToCollect.forEach((path) => {
    const metadata = collectMetadataForElementPath(
      path,
      validPaths,
      options.selectedViews,
      options.scale,
      containerRect,
    )
    if (metadata != null) {
      updatedMetadataMap[EP.toString(path)] = metadata.metadata
      if (metadata.computedStyle != null) {
        updatedComputedStyleMap[EP.toString(path)] = metadata.computedStyle
      }
    }
  })

  return {
    metadata: updatedMetadataMap,
    computedStyle: updatedComputedStyleMap,
  }
}

export function collectMetadata(
  elementsToFocusOn: ElementsToRerender,
  options: {
    scale: number
    selectedViews: Array<ElementPath>
    metadataToUpdate: { [path: string]: DomElementMetadata }
    computedStylesToUpdate: { [path: string]: ComputedStyleMetadata }
  },
) {
  getComputedStylesCache.updateObservers()
  createElementInstanceMetadataForElementCached.updateObservers()

  const canvasRootContainer = document.getElementById(CanvasContainerID)
  if (canvasRootContainer == null) {
    return
  }

  const validPaths = getValidPathsFromCanvasContainer(canvasRootContainer)

  let result: {
    metadata: { [path: string]: DomElementMetadata }
    computedStyle: { [path: string]: ComputedStyleMetadata }
  }
  if (elementsToFocusOn == 'rerender-all-elements') {
    result = collectMetadataForPaths(canvasRootContainer, validPaths, validPaths, options)
  } else {
    result = collectMetadataForPaths(canvasRootContainer, elementsToFocusOn, validPaths, options)
  }

  return result
}

const ObserversAvailable = window.MutationObserver != null && ResizeObserver != null

const MutationObserverConfig = { attributes: true, childList: true, subtree: true }

export class ObserverCache<T, N extends Element = Element, A extends Array<any> = Array<any>> {
  private cache = new WeakMap<N, { value: T; params: A }>()

  private getter: (node: N, ...args: A) => T

  private handleMutation = (mutations: Array<MutationRecord>) => {
    // delete the metadata for the element that has been mutated
    for (const mutation of mutations) {
      const target = mutation.target
      this.cache.delete(target as N)
    }
  }
  private handleResize = (entries: Array<ResizeObserverEntry>) => {
    // delete the metadata for the element that has been resized
    for (const entry of entries) {
      const target = entry.target
      this.cache.delete(target as N)
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
    if (cacheResult == null || cacheResult.params.some((param, index) => param !== args[index])) {
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
    attributeMetadata: attributeMetadata,
  }
}
