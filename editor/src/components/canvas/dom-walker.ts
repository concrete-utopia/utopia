import React from 'react'
import { sides } from 'utopia-api/core'
import * as ResizeObserverSyntheticDefault from 'resize-observer-polyfill'
const ResizeObserver = ResizeObserverSyntheticDefault.default ?? ResizeObserverSyntheticDefault

import * as EP from '../../core/shared/element-path'
import {
  DetectedLayoutSystem,
  ElementInstanceMetadata,
  ComputedStyle,
  elementInstanceMetadata,
  SpecialSizeMeasurements,
  specialSizeMeasurements,
  emptySpecialSizeMeasurements,
  emptyComputedStyle,
  StyleAttributeMetadata,
  emptyAttributeMetadatada,
  ElementInstanceMetadataMap,
} from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { getCanvasRectangleFromElement, getDOMAttribute } from '../../core/shared/dom-utils'
import { applicative4Either, isRight, left } from '../../core/shared/either'
import Utils from '../../utils/utils'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  boundingRectangle,
  LocalPoint,
  LocalRectangle,
  localRectangle,
  roundToNearestHalf,
  zeroCanvasRect,
  zeroLocalRect,
  canvasRectangle,
} from '../../core/shared/math-utils'
import {
  CSSNumber,
  parseCSSLength,
  CSSPosition,
  positionValues,
  computedStyleKeys,
} from '../inspector/common/css-utils'
import { camelCaseToDashed } from '../../core/shared/string-utils'
import { UtopiaStoreAPI } from '../editor/store/store-hook'
import {
  UTOPIA_DO_NOT_TRAVERSE_KEY,
  UTOPIA_PATH_KEY,
  UTOPIA_SCENE_ID_KEY,
} from '../../core/model/utopia-constants'

import { PERFORMANCE_MARKS_ALLOWED } from '../../common/env-vars'
import { CanvasContainerID } from './canvas-types'
import { emptySet } from '../../core/shared/set-utils'
import {
  getDeepestPathOnDomElement,
  getPathWithStringsOnDomElement,
} from '../../core/shared/uid-utils'
import { pluck, uniqBy } from '../../core/shared/array-utils'
import { forceNotNull, optionalMap } from '../../core/shared/optional-utils'
import { fastForEach } from '../../core/shared/utils'
import { MapLike } from 'typescript'
import { isFeatureEnabled } from '../../utils/feature-switches'
import type {
  EditorState,
  EditorStorePatched,
  ElementsToRerender,
} from '../editor/store/editor-state'
import { shallowEqual } from '../../core/shared/equality-utils'
import { pick } from '../../core/shared/object-utils'

const MutationObserverConfig = { attributes: true, childList: true, subtree: true }
const ObserversAvailable = (window as any).MutationObserver != null && ResizeObserver != null

function elementLayoutSystem(computedStyle: CSSStyleDeclaration | null): DetectedLayoutSystem {
  if (computedStyle == null) {
    return 'none'
  }
  if (computedStyle.display != null) {
    if (computedStyle.display === 'flex') {
      return 'flex'
    }
    if (computedStyle.display === 'grid') {
      return 'grid'
    }
  }
  return 'flow'
}

function getPosition(computedStyle: CSSStyleDeclaration | null): CSSPosition | null {
  const valueAsAny = computedStyle?.position as any
  return positionValues.includes(valueAsAny) ? valueAsAny : null
}

function isElementNonStatic(computedStyle: CSSStyleDeclaration | null) {
  if (computedStyle == null) {
    return false
  }
  if (computedStyle.position != null && computedStyle.position !== 'static') {
    return true
  }

  return false
}

function isElementAContainingBlockForAbsolute(computedStyle: CSSStyleDeclaration | null) {
  // https://developer.mozilla.org/en-US/docs/Web/CSS/Containing_block#identifying_the_containing_block
  if (computedStyle == null) {
    return false
  }
  if (isElementNonStatic(computedStyle)) {
    return true
  }
  if (computedStyle.transform != null && computedStyle.transform !== 'none') {
    return true
  }
  if (computedStyle.perspective != null && computedStyle.perspective !== 'none') {
    return true
  }
  if (computedStyle.willChange === 'transform' || computedStyle.willChange === 'perspective') {
    return true
  }
  if (computedStyle.filter != null && computedStyle.filter !== 'none') {
    return true
  }
  if (computedStyle.contain === 'paint') {
    return true
  }
  return false
}

const applicativeSidesPxTransform = (t: CSSNumber, r: CSSNumber, b: CSSNumber, l: CSSNumber) =>
  sides(
    t.unit === 'px' ? t.value : undefined,
    r.unit === 'px' ? r.value : undefined,
    b.unit === 'px' ? b.value : undefined,
    l.unit === 'px' ? l.value : undefined,
  )

function isScene(node: Node): node is HTMLElement {
  return (
    node instanceof HTMLElement && node.attributes.getNamedItemNS(null, UTOPIA_SCENE_ID_KEY) != null
  )
}

function findParentScene(target: HTMLElement): string | null {
  const sceneID = getDOMAttribute(target, UTOPIA_SCENE_ID_KEY)
  if (sceneID != null) {
    return sceneID
  } else {
    if (target.parentElement != null) {
      return findParentScene(target.parentElement)
    } else {
      return null
    }
  }
}

function lazyValue<T>(getter: () => T) {
  let alreadyResolved = false
  let resolvedValue: T
  return () => {
    if (!alreadyResolved) {
      resolvedValue = getter()
      alreadyResolved = true
    }
    return resolvedValue
  }
}

function getAttributesComingFromStyleSheets(element: HTMLElement): Set<string> {
  let appliedAttributes = new Set<string>()
  const sheets = document.styleSheets
  try {
    for (const i in sheets) {
      const sheet = sheets[i]
      const rules = sheet.rules ?? sheet.cssRules
      for (const r in rules) {
        const rule = rules[r] as CSSStyleRule
        if (element.matches(rule.selectorText)) {
          const style = rule.style
          for (const attributeName in style) {
            const attributeExists = style[attributeName] !== ''
            if (attributeExists) {
              appliedAttributes.add(attributeName)
            }
          }
        }
      }
    }
  } catch (e) {
    // ignore error, probably JSDOM unit test related
  }
  return appliedAttributes
}

let AttributesFromStyleSheetsCache: Map<HTMLElement, Set<string>> = new Map()

function getCachedAttributesComingFromStyleSheets(
  invalidatedPathsForStylesheetCache: Set<string>,
  elementPath: ElementPath,
  element: HTMLElement,
): Set<string> {
  const pathAsString = EP.toString(elementPath)
  const invalidated = invalidatedPathsForStylesheetCache.has(pathAsString)
  const inCache = AttributesFromStyleSheetsCache.has(element)
  if (inCache && !invalidated) {
    return AttributesFromStyleSheetsCache.get(element)!
  }
  invalidatedPathsForStylesheetCache.delete(pathAsString) // mutation!
  const value = getAttributesComingFromStyleSheets(element)
  AttributesFromStyleSheetsCache.set(element, value)
  return value
}

// todo move to file
export type UpdateMutableCallback<S> = (updater: (mutableState: S) => void) => void

export interface DomWalkerProps {
  selectedViews: Array<ElementPath>
  scale: number
  onDomReport: (
    elementMetadata: ElementInstanceMetadataMap,
    cachedPaths: Array<ElementPath>,
  ) => void
  mountCount: number
  domWalkerInvalidateCount: number
  canvasInteractionHappening: boolean
  additionalElementsToUpdate: Array<ElementPath>
}

// This function adds elementMetadata to the metadataToMutate map. If a metadata instance with the same element path
// already existed in the map, it adds the metadata of a fragment which contains both the existing and the new element.
// NOTE: For performance reasons this function mutates the first parameter and writes the result there. It only mutates the
// map itself, it should never mutate the metadata instances inside the map!
function addElementMetadataToMapWithFragments_MUTATE(
  metadataToMutate: ElementInstanceMetadataMap,
  elementMetadata: Readonly<ElementInstanceMetadata>,
): void {
  const pathString = EP.toString(elementMetadata.elementPath)
  const existingMetadata = metadataToMutate[pathString]

  if (existingMetadata == null) {
    metadataToMutate[pathString] = elementMetadata
  } else {
    // We've hit a fragment, so remove the style etc., but keep the frames for selection
    const merged = elementInstanceMetadata(
      elementMetadata.elementPath,
      left('fragment'),
      boundingRectangle(
        existingMetadata.globalFrame ?? zeroCanvasRect,
        elementMetadata.globalFrame ?? zeroCanvasRect,
      ),
      boundingRectangle(
        existingMetadata.localFrame ?? zeroLocalRect,
        elementMetadata.localFrame ?? zeroLocalRect,
      ),
      false,
      false,
      emptySpecialSizeMeasurements,
      {},
      {},
      null,
      null,
    )

    metadataToMutate[pathString] = merged
  }
}

// This function merges metadataToMutate and otherMetadata maps. If metadata instances with the same element path
// exist in both maps, it adds the metadata of a fragment which contains both the elements.
// NOTE: For performance reasons this function mutates the first parameter and writes the result there. It only mutates
// the map itself, it should never mutate the metadata instances inside the map!
function mergeMetadataMapsWithFragments_MUTATE(
  metadataToMutate: ElementInstanceMetadataMap,
  otherMetadata: Readonly<ElementInstanceMetadataMap>,
): void {
  fastForEach(Object.values(otherMetadata), (elementMetadata) => {
    addElementMetadataToMapWithFragments_MUTATE(metadataToMutate, elementMetadata)
  })
}

export interface DomWalkerMutableStateData {
  invalidatedPaths: Set<string> // warning: all subtrees under each invalidated path should invalidated
  invalidatedPathsForStylesheetCache: Set<string>
  initComplete: boolean

  mutationObserver: MutationObserver
  resizeObserver: ResizeObserver
}

export function createDomWalkerMutableState(
  editorStoreApi: UtopiaStoreAPI,
): DomWalkerMutableStateData {
  const mutableData: DomWalkerMutableStateData = {
    invalidatedPaths: emptySet(),
    invalidatedPathsForStylesheetCache: emptySet(),
    initComplete: true,

    mutationObserver: null as any,
    resizeObserver: null as any,
  }

  const observers = initDomWalkerObservers(mutableData, editorStoreApi)
  mutableData.mutationObserver = observers.mutationObserver
  mutableData.resizeObserver = observers.resizeObserver

  return mutableData
}

export const DomWalkerMutableStateCtx = React.createContext<DomWalkerMutableStateData | null>(null)
function useDomWalkerMutableStateContext() {
  return forceNotNull(
    `DomWalkerMutableStateCtx needs a Provider`,
    React.useContext(DomWalkerMutableStateCtx),
  )
}

interface RunDomWalkerParams {
  // from dom walker props
  selectedViews: Array<ElementPath>
  scale: number
  additionalElementsToUpdate: Array<ElementPath>
  elementsToFocusOn: ElementsToRerender

  domWalkerMutableState: DomWalkerMutableStateData
  rootMetadataInStateRef: { readonly current: ElementInstanceMetadataMap }
}

function runSelectiveDomWalker(
  elementsToFocusOn: Array<ElementPath>,
  domWalkerMutableState: DomWalkerMutableStateData,
  selectedViews: Array<ElementPath>,
  scale: number,
  rootMetadataInStateRef: { readonly current: ElementInstanceMetadataMap },
  containerRectLazy: () => CanvasRectangle,
): { metadata: ElementInstanceMetadataMap; cachedPaths: ElementPath[] } {
  let workingMetadata: ElementInstanceMetadataMap = {}

  const canvasRootContainer = document.getElementById(CanvasContainerID)
  if (canvasRootContainer != null) {
    const validPathsArr = optionalMap(
      (paths) => paths.split(' '),
      canvasRootContainer.getAttribute('data-utopia-valid-paths'),
    )
    const validPaths = new Set(validPathsArr)

    const parentPoint = canvasPoint({ x: 0, y: 0 })

    elementsToFocusOn.forEach((path) => {
      /**
       * if a elementToFocusOn path points to a component instance, such as App/card-instance, the DOM will
       * only contain an element with the path App/card-instance:card-root. To be able to quickly find the "rootest" element
       * that belongs to a path, we use the ^= prefix search in querySelector.
       * The assumption is that querySelector will return the "topmost" DOM-element with the matching prefix,
       * which is the same as the "rootest" element we are looking for
       */
      const element = document.querySelector(
        `[${UTOPIA_PATH_KEY}^="${EP.toString(path)}"]`,
      ) as HTMLElement | null

      if (element != null) {
        const pathsWithStrings = getPathWithStringsOnDomElement(element)
        const foundValidPaths = pathsWithStrings.filter((pathWithString) => {
          const staticPath = EP.toString(EP.makeLastPartOfPathStatic(pathWithString.path))
          return validPaths.has(staticPath)
        })

        const { collectedMetadata } = collectAndCreateMetadataForElement(
          element,
          parentPoint,
          path, // TODO is this good enough?
          scale,
          containerRectLazy,
          foundValidPaths.map((p) => p.path),
          domWalkerMutableState.invalidatedPathsForStylesheetCache,
          selectedViews,
          domWalkerMutableState.invalidatedPaths,
        )

        mergeMetadataMapsWithFragments_MUTATE(workingMetadata, collectedMetadata)
      }
    })
    const otherElementPaths = Object.keys(rootMetadataInStateRef.current).filter(
      (path) => !Object.keys(workingMetadata).includes(path),
    )
    const rootMetadataForOtherElements = pick(otherElementPaths, rootMetadataInStateRef.current)
    mergeMetadataMapsWithFragments_MUTATE(rootMetadataForOtherElements, workingMetadata)

    return {
      metadata: rootMetadataForOtherElements,
      cachedPaths: otherElementPaths.map(EP.fromString),
    }
  }

  return {
    metadata: rootMetadataInStateRef.current,
    cachedPaths: Object.values(rootMetadataInStateRef.current).map((p) => p.elementPath),
  }
}

// Dom walker has 3 modes for performance reasons:
// Fastest is the selective mode, this runs when elementsToFocusOn is not 'rerender-all-elements'. In this case it only collects the metadata of the elements in elementsToFocusOn
// Middle speed is when initComplete is true, in this case it traverses the full dom but only collects the metadata for the not invalidated elements (stored in invalidatedPaths)
// Slowest is the full run, when elementsToFocusOn is 'rerender-all-elements' and initComplete is false
export function runDomWalker({
  domWalkerMutableState,
  selectedViews,
  elementsToFocusOn,
  scale,
  additionalElementsToUpdate,
  rootMetadataInStateRef,
}: RunDomWalkerParams): {
  metadata: ElementInstanceMetadataMap
  cachedPaths: ElementPath[]
  invalidatedPaths: string[]
} | null {
  const needsWalk =
    !domWalkerMutableState.initComplete || domWalkerMutableState.invalidatedPaths.size > 0

  if (!needsWalk) {
    return null // early return to save performance
  }

  const LogDomWalkerPerformance =
    isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED

  const canvasRootContainer = document.getElementById(CanvasContainerID)

  if (canvasRootContainer != null) {
    if (LogDomWalkerPerformance) {
      performance.mark('DOM_WALKER_START')
    }

    const invalidatedPaths = Array.from(domWalkerMutableState.invalidatedPaths)

    // Get some base values relating to the div this component creates.
    if (
      ObserversAvailable &&
      domWalkerMutableState.resizeObserver != null &&
      domWalkerMutableState.mutationObserver != null
    ) {
      document.querySelectorAll(`#${CanvasContainerID} *`).forEach((elem) => {
        domWalkerMutableState.resizeObserver.observe(elem)
      })
      domWalkerMutableState.mutationObserver.observe(canvasRootContainer, MutationObserverConfig)
    }

    // getCanvasRectangleFromElement is costly, so I made it lazy. we only need the value inside globalFrameForElement
    const containerRect = lazyValue(() => {
      return getCanvasRectangleFromElement(canvasRootContainer, scale)
    })

    // This assumes that the canvas root is rendering a Storyboard fragment.
    // The necessary validPaths and the root fragment's template path comes from props,
    // because the fragment is invisible in the DOM.
    const { metadata, cachedPaths } =
      // when we don't rerender all elements we just run the dom walker in selective mode: only update the metatdata
      // of the currently rendered elements (for performance reasons)
      elementsToFocusOn === 'rerender-all-elements'
        ? walkCanvasRootFragment(
            canvasRootContainer,
            rootMetadataInStateRef,
            domWalkerMutableState.invalidatedPaths, // TODO does walkCanvasRootFragment ever uses invalidatedPaths right now?
            domWalkerMutableState.invalidatedPathsForStylesheetCache,
            selectedViews,
            !domWalkerMutableState.initComplete, // TODO do we run walkCanvasRootFragment with initComplete=true anymore? // TODO _should_ we ever run walkCanvasRootFragment with initComplete=false EVER, or instead can we set the canvas root as the invalidated path?
            scale,
            containerRect,
            [...additionalElementsToUpdate, ...selectedViews],
          )
        : runSelectiveDomWalker(
            elementsToFocusOn,
            domWalkerMutableState,
            selectedViews,
            scale,
            rootMetadataInStateRef,
            containerRect,
          )
    if (LogDomWalkerPerformance) {
      performance.mark('DOM_WALKER_END')
      performance.measure(
        `DOM WALKER - cached paths: [${cachedPaths.map(EP.toString).join(', ')}]`,
        'DOM_WALKER_START',
        'DOM_WALKER_END',
      )
    }
    domWalkerMutableState.initComplete = true // Mutation!

    return { metadata: metadata, cachedPaths: cachedPaths, invalidatedPaths: invalidatedPaths }
  } else {
    // TODO flip if-else
    return null
  }
}

function selectCanvasInteractionHappening(store: EditorStorePatched): boolean {
  const interactionSessionActive = store.editor.canvas.interactionSession != null
  const oldDragStateActiveKILLME = store.derived.transientState.filesState != null
  return interactionSessionActive || oldDragStateActiveKILLME
}

export function initDomWalkerObservers(
  domWalkerMutableState: DomWalkerMutableStateData,
  editorStore: UtopiaStoreAPI,
): { resizeObserver: ResizeObserver; mutationObserver: MutationObserver } {
  const resizeObserver = new ResizeObserver((entries: any) => {
    const canvasInteractionHappening = selectCanvasInteractionHappening(editorStore.getState())
    const selectedViews = editorStore.getState().editor.selectedViews
    if (canvasInteractionHappening) {
      // Warning this only adds the selected views instead of the observed element
      fastForEach(selectedViews, (v) => {
        domWalkerMutableState.invalidatedPaths.add(EP.toString(v))
      })
    } else {
      for (let entry of entries) {
        const sceneID = findParentScene(entry.target)
        if (sceneID != null) {
          domWalkerMutableState.invalidatedPaths.add(sceneID) // warning this invalidates the entire scene instead of just the observed element.
        }
      }
    }
  })

  const mutationObserver = new window.MutationObserver((mutations: MutationRecord[]) => {
    const canvasInteractionHappening = selectCanvasInteractionHappening(editorStore.getState())
    const selectedViews = editorStore.getState().editor.selectedViews

    if (canvasInteractionHappening) {
      // Warning this only adds the selected views instead of the observed element
      fastForEach(selectedViews, (v) => {
        domWalkerMutableState.invalidatedPaths.add(EP.toString(v))
      })
    } else {
      for (let mutation of mutations) {
        if (
          mutation.attributeName === 'style' ||
          mutation.addedNodes.length > 0 ||
          mutation.removedNodes.length > 0
        ) {
          if (mutation.target instanceof HTMLElement) {
            const sceneID = findParentScene(mutation.target)
            if (sceneID != null) {
              domWalkerMutableState.invalidatedPaths.add(sceneID) // warning this invalidates the entire scene instead of just the observed element.
            }
          }
        }
      }
    }
  })

  return { resizeObserver, mutationObserver }
}

export function invalidateDomWalkerIfNecessary(
  domWalkerMutableState: DomWalkerMutableStateData,
  oldEditorState: EditorState,
  newEditorState: EditorState,
): void {
  // invalidate initComplete on mountCount increase
  if (
    newEditorState.canvas.domWalkerInvalidateCount >
      oldEditorState.canvas.domWalkerInvalidateCount ||
    newEditorState.canvas.mountCount > oldEditorState.canvas.mountCount
  ) {
    domWalkerMutableState.initComplete = false // Mutation!
    domWalkerMutableState.invalidatedPaths.clear() // Mutation!
  }

  // invalidate scenes when selectedViews change
  if (!shallowEqual(oldEditorState.selectedViews, newEditorState.selectedViews)) {
    newEditorState.selectedViews.forEach((sv) => {
      const scenePath = EP.createBackwardsCompatibleScenePath(sv)
      if (scenePath != null) {
        const sceneID = EP.toString(scenePath)
        domWalkerMutableState.invalidatedPaths.add(sceneID) // Mutation!
        domWalkerMutableState.invalidatedPathsForStylesheetCache.add(EP.toString(sv))
      }
    })
  }
}

export function useDomWalkerInvalidateCallbacks(): [UpdateMutableCallback<Set<string>>] {
  const domWalkerMutableState = useDomWalkerMutableStateContext()
  // For invalidating specific paths only
  const updateInvalidatedPaths: UpdateMutableCallback<Set<string>> = React.useCallback(
    (callback) => {
      callback(domWalkerMutableState.invalidatedPaths)
    },
    [domWalkerMutableState],
  )

  return [updateInvalidatedPaths]
}

function collectMetadataForElement(
  element: HTMLElement,
  parentPoint: CanvasPoint,
  closestOffsetParentPath: ElementPath,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): {
  tagName: string
  globalFrame: CanvasRectangle
  localFrame: LocalRectangle
  specialSizeMeasurementsObject: SpecialSizeMeasurements
} {
  const tagName: string = element.tagName.toLowerCase()
  const globalFrame = globalFrameForElement(element, scale, containerRectLazy)
  const localFrame = localRectangle(Utils.offsetRect(globalFrame, Utils.negate(parentPoint)))

  const specialSizeMeasurementsObject = getSpecialMeasurements(
    element,
    closestOffsetParentPath,
    scale,
    containerRectLazy,
  )

  return {
    tagName: tagName,
    globalFrame: globalFrame,
    localFrame: localFrame,
    specialSizeMeasurementsObject: specialSizeMeasurementsObject,
  }
}

function isAnyPathInvalidated(
  stringPathsForElement: Array<string>,
  invalidatedPaths: ReadonlySet<string>,
): boolean {
  return invalidatedPaths.size > 0 && stringPathsForElement.some((p) => invalidatedPaths.has(p))
}

function collectMetadata(
  element: HTMLElement,
  pathsForElement: Array<ElementPath>,
  stringPathsForElement: Array<string>,
  parentPoint: CanvasPoint,
  closestOffsetParentPath: ElementPath,
  allUnfilteredChildrenPaths: Array<ElementPath>,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  invalidatedPaths: Set<string>,
  invalidatedPathsForStylesheetCache: Set<string>,
  rootMetadataInStateRef: React.MutableRefObject<ElementInstanceMetadataMap>,
  invalidated: boolean,
  selectedViews: Array<ElementPath>,
  additionalElementsToUpdate: Array<ElementPath>,
): {
  collectedMetadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
  collectedPaths: Array<ElementPath>
} {
  if (pathsForElement.length === 0) {
    return {
      collectedMetadata: {},
      cachedPaths: [],
      collectedPaths: [],
    }
  }
  const shouldCollect =
    invalidated ||
    isAnyPathInvalidated(stringPathsForElement, invalidatedPaths) ||
    pathsForElement.some((pathForElement) => {
      return additionalElementsToUpdate.some((additionalElementToUpdate) =>
        EP.pathsEqual(pathForElement, additionalElementToUpdate),
      )
    })
  if (shouldCollect) {
    return collectAndCreateMetadataForElement(
      element,
      parentPoint,
      closestOffsetParentPath,
      scale,
      containerRectLazy,
      pathsForElement,
      invalidatedPathsForStylesheetCache,
      selectedViews,
      invalidatedPaths,
    )
  } else {
    const cachedMetadata = pick(pathsForElement.map(EP.toString), rootMetadataInStateRef.current)

    if (Object.keys(cachedMetadata).length === pathsForElement.length) {
      return {
        collectedMetadata: cachedMetadata,
        cachedPaths: pathsForElement,
        collectedPaths: pathsForElement,
      }
    } else {
      // If any path is missing cached metadata we must forcibly invalidate the element
      return collectMetadata(
        element,
        pathsForElement,
        stringPathsForElement,
        parentPoint,
        closestOffsetParentPath,
        allUnfilteredChildrenPaths,
        scale,
        containerRectLazy,
        invalidatedPaths,
        invalidatedPathsForStylesheetCache,
        rootMetadataInStateRef,
        true,
        selectedViews,
        additionalElementsToUpdate,
      )
    }
  }
}

function collectAndCreateMetadataForElement(
  element: HTMLElement,
  parentPoint: CanvasPoint,
  closestOffsetParentPath: ElementPath,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  pathsForElement: ElementPath[],
  invalidatedPathsForStylesheetCache: Set<string>,
  selectedViews: ElementPath[],
  invalidatedPaths: Set<string>,
) {
  const { tagName, globalFrame, localFrame, specialSizeMeasurementsObject } =
    collectMetadataForElement(
      element,
      parentPoint,
      closestOffsetParentPath,
      scale,
      containerRectLazy,
    )

  const { computedStyle, attributeMetadata } = getComputedStyle(
    element,
    pathsForElement,
    invalidatedPathsForStylesheetCache,
    selectedViews,
  )

  const collectedMetadata: ElementInstanceMetadataMap = {}
  pathsForElement.forEach((path) => {
    const pathStr = EP.toString(path)
    invalidatedPaths.delete(pathStr) // mutation!

    collectedMetadata[pathStr] = elementInstanceMetadata(
      path,
      left(tagName),
      globalFrame,
      localFrame,
      false,
      false,
      specialSizeMeasurementsObject,
      computedStyle,
      attributeMetadata,
      null,
      null,
    )
  })

  return {
    collectedMetadata: collectedMetadata,
    cachedPaths: [],
    collectedPaths: pathsForElement,
  }
}

function getComputedStyle(
  element: HTMLElement,
  paths: Array<ElementPath>,
  invalidatedPathsForStylesheetCache: Set<string>,
  selectedViews: Array<ElementPath>,
): { computedStyle: ComputedStyle | null; attributeMetadata: StyleAttributeMetadata | null } {
  const isSelectedOnAnyPaths = selectedViews.some((sv) =>
    paths.some((path) => EP.pathsEqual(sv, path)),
  )
  if (!isSelectedOnAnyPaths) {
    // the element is not among the selected views, skip computing the style
    return {
      computedStyle: null,
      attributeMetadata: null,
    }
  }
  const elementStyle = window.getComputedStyle(element)
  const attributesSetByStylesheet = getCachedAttributesComingFromStyleSheets(
    invalidatedPathsForStylesheetCache,
    paths[0], // TODO is this sufficient to use the first path element for caching?
    element,
  )
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

function getSpecialMeasurements(
  element: HTMLElement,
  closestOffsetParentPath: ElementPath,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): SpecialSizeMeasurements {
  const elementStyle = window.getComputedStyle(element)
  const layoutSystemForChildren = elementLayoutSystem(elementStyle)
  const position = getPosition(elementStyle)

  const offset = {
    x: roundToNearestHalf(element.offsetLeft),
    y: roundToNearestHalf(element.offsetTop),
  } as LocalPoint

  const coordinateSystemBounds =
    element.offsetParent instanceof HTMLElement
      ? globalFrameForElement(element.offsetParent, scale, containerRectLazy)
      : null

  const immediateParentBounds =
    element.parentElement instanceof HTMLElement
      ? globalFrameForElement(element.parentElement, scale, containerRectLazy)
      : null

  const parentElementStyle =
    element.parentElement == null ? null : window.getComputedStyle(element.parentElement)
  const isParentNonStatic = isElementNonStatic(parentElementStyle)

  const providesBoundsForAbsoluteChildren = isElementAContainingBlockForAbsolute(elementStyle)

  const parentLayoutSystem = elementLayoutSystem(parentElementStyle)
  const parentProvidesLayout = element.parentElement === element.offsetParent
  const parentFlexDirection = parentElementStyle?.flexDirection ?? null
  const flexDirection = elementStyle.flexDirection ?? null

  const margin = applicative4Either(
    applicativeSidesPxTransform,
    parseCSSLength(elementStyle.marginTop),
    parseCSSLength(elementStyle.marginRight),
    parseCSSLength(elementStyle.marginBottom),
    parseCSSLength(elementStyle.marginLeft),
  )

  const padding = applicative4Either(
    applicativeSidesPxTransform,
    parseCSSLength(elementStyle.paddingTop),
    parseCSSLength(elementStyle.paddingRight),
    parseCSSLength(elementStyle.paddingBottom),
    parseCSSLength(elementStyle.paddingLeft),
  )

  let naturalWidth: number | null = null
  let naturalHeight: number | null = null
  if (element.tagName === 'IMG') {
    naturalWidth = roundToNearestHalf((element as HTMLImageElement).naturalWidth)
    naturalHeight = roundToNearestHalf((element as HTMLImageElement).naturalHeight)
  }

  let clientWidth = roundToNearestHalf(element.clientWidth)
  let clientHeight = roundToNearestHalf(element.clientHeight)

  const childrenCount = element.childElementCount

  const borderTopWidth = parseCSSLength(elementStyle.borderTopWidth)
  const borderRightWidth = parseCSSLength(elementStyle.borderRightWidth)
  const borderBottomWidth = parseCSSLength(elementStyle.borderBottomWidth)
  const borderLeftWidth = parseCSSLength(elementStyle.borderLeftWidth)
  const border = {
    top: isRight(borderTopWidth) ? borderTopWidth.value.value : 0,
    right: isRight(borderRightWidth) ? borderRightWidth.value.value : 0,
    bottom: isRight(borderBottomWidth) ? borderBottomWidth.value.value : 0,
    left: isRight(borderLeftWidth) ? borderLeftWidth.value.value : 0,
  }

  const globalFrame = globalFrameForElement(element, scale, containerRectLazy)
  const globalContentBox = canvasRectangle({
    x: globalFrame.x + border.left,
    y: globalFrame.y + border.top,
    width: globalFrame.width - border.left - border.right,
    height: globalFrame.height - border.top - border.bottom,
  })

  return specialSizeMeasurements(
    offset,
    coordinateSystemBounds,
    immediateParentBounds,
    parentProvidesLayout,
    closestOffsetParentPath,
    isParentNonStatic,
    parentLayoutSystem,
    layoutSystemForChildren,
    providesBoundsForAbsoluteChildren,
    elementStyle.display,
    position,
    isRight(margin) ? margin.value : sides(undefined, undefined, undefined, undefined),
    isRight(padding) ? padding.value : sides(undefined, undefined, undefined, undefined),
    naturalWidth,
    naturalHeight,
    clientWidth,
    clientHeight,
    parentFlexDirection,
    flexDirection,
    element.localName,
    childrenCount,
    globalContentBox,
  )
}

function globalFrameForElement(
  element: HTMLElement,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
) {
  const elementRect = getCanvasRectangleFromElement(element, scale)
  return Utils.offsetRect(elementRect, Utils.negate(containerRectLazy()))
}

function walkCanvasRootFragment(
  canvasRoot: HTMLElement,
  rootMetadataInStateRef: React.MutableRefObject<ElementInstanceMetadataMap>,
  invalidatedPaths: Set<string>,
  invalidatedPathsForStylesheetCache: Set<string>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  additionalElementsToUpdate: Array<ElementPath>,
): {
  metadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
} {
  const canvasRootPath: ElementPath | null = optionalMap(
    EP.fromString,
    canvasRoot.getAttribute('data-utopia-root-element-path'),
  )
  const validPaths: Array<ElementPath> | null = optionalMap(
    (paths) => paths.split(' ').map(EP.fromString),
    canvasRoot.getAttribute('data-utopia-valid-paths'),
  )

  if (canvasRootPath == null || validPaths == null) {
    throw new Error(
      'Utopia Internal Error: Running DOM-walker without canvasRootPath or validRootPaths',
    )
  }

  invalidatedPaths.delete(EP.toString(canvasRootPath)) // mutation!

  if (
    ObserversAvailable &&
    invalidatedPaths.size === 0 &&
    Object.keys(rootMetadataInStateRef.current).length > 0 &&
    additionalElementsToUpdate.length === 0 &&
    !invalidated
  ) {
    // no mutation happened on the entire canvas, just return the old metadata
    return {
      metadata: rootMetadataInStateRef.current,
      cachedPaths: [canvasRootPath],
    }
  } else {
    const { rootMetadata, cachedPaths } = walkSceneInner(
      canvasRoot,
      canvasRootPath,
      validPaths,
      rootMetadataInStateRef,
      invalidatedPaths,
      invalidatedPathsForStylesheetCache,
      selectedViews,
      invalidated,
      scale,
      containerRectLazy,
      additionalElementsToUpdate,
    )
    // The Storyboard root being a fragment means it is invisible to us in the DOM walker,
    // so walkCanvasRootFragment will create a fake root ElementInstanceMetadata
    // to provide a home for the the (really existing) childMetadata
    const metadata: ElementInstanceMetadata = elementInstanceMetadata(
      canvasRootPath,
      left('Storyboard'),
      { x: 0, y: 0, width: 0, height: 0 } as CanvasRectangle,
      { x: 0, y: 0, width: 0, height: 0 } as LocalRectangle,
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadatada,
      null,
      null, // this comes from the Spy Wrapper
    )

    addElementMetadataToMapWithFragments_MUTATE(rootMetadata, metadata)

    return { metadata: rootMetadata, cachedPaths: cachedPaths }
  }
}

function walkScene(
  scene: HTMLElement,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ElementInstanceMetadataMap>,
  invalidatedPaths: Set<string>,
  invalidatedPathsForStylesheetCache: Set<string>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  additionalElementsToUpdate: Array<ElementPath>,
): {
  metadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
} {
  if (scene instanceof HTMLElement) {
    // Right now this assumes that only UtopiaJSXComponents can be rendered via scenes,
    // and that they can only have a single root element
    const sceneIndexAttr = scene.attributes.getNamedItemNS(null, UTOPIA_SCENE_ID_KEY)
    const sceneID = sceneIndexAttr?.value ?? null
    const instancePath = sceneID == null ? null : EP.fromString(sceneID)

    if (sceneID != null && instancePath != null && EP.isElementPath(instancePath)) {
      const invalidatedScene =
        invalidated ||
        (ObserversAvailable && invalidatedPaths.size > 0 && invalidatedPaths.has(sceneID))

      invalidatedPaths.delete(sceneID) // mutation!

      const {
        childPaths: rootElements,
        rootMetadata,
        cachedPaths,
      } = walkSceneInner(
        scene,
        instancePath,
        validPaths,
        rootMetadataInStateRef,
        invalidatedPaths,
        invalidatedPathsForStylesheetCache,
        selectedViews,
        invalidatedScene,
        scale,
        containerRectLazy,
        additionalElementsToUpdate,
      )

      const { collectedMetadata: sceneMetadata, cachedPaths: sceneCachedPaths } = collectMetadata(
        scene,
        [instancePath],
        [sceneID],
        canvasPoint({ x: 0, y: 0 }),
        instancePath,
        rootElements,
        scale,
        containerRectLazy,
        invalidatedPaths,
        invalidatedPathsForStylesheetCache,
        rootMetadataInStateRef,
        invalidatedScene,
        selectedViews,
        additionalElementsToUpdate,
      )

      mergeMetadataMapsWithFragments_MUTATE(rootMetadata, sceneMetadata)

      return {
        metadata: rootMetadata,
        cachedPaths: [...cachedPaths, ...sceneCachedPaths],
      }
    }
  }
  return { metadata: {}, cachedPaths: [] } // verify
}

function walkSceneInner(
  scene: HTMLElement,
  closestOffsetParentPath: ElementPath,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ElementInstanceMetadataMap>,
  invalidatedPaths: Set<string>,
  invalidatedPathsForStylesheetCache: Set<string>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  additionalElementsToUpdate: Array<ElementPath>,
): {
  childPaths: Array<ElementPath>
  rootMetadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
} {
  const globalFrame: CanvasRectangle = globalFrameForElement(scene, scale, containerRectLazy)

  let childPaths: Array<ElementPath> = []
  let rootMetadataAccumulator: ElementInstanceMetadataMap = {}
  let cachedPathsAccumulator: Array<ElementPath> = []

  scene.childNodes.forEach((childNode) => {
    const {
      childPaths: childNodePaths,
      rootMetadata,
      cachedPaths,
    } = walkElements(
      childNode,
      globalFrame,
      closestOffsetParentPath,
      validPaths,
      rootMetadataInStateRef,
      invalidatedPaths,
      invalidatedPathsForStylesheetCache,
      selectedViews,
      invalidated,
      scale,
      containerRectLazy,
      additionalElementsToUpdate,
    )

    childPaths.push(...childNodePaths)
    mergeMetadataMapsWithFragments_MUTATE(rootMetadataAccumulator, rootMetadata)
    cachedPathsAccumulator.push(...cachedPaths)
  })

  return {
    childPaths: childPaths,
    rootMetadata: rootMetadataAccumulator,
    cachedPaths: cachedPathsAccumulator,
  }
}

// Walks through the DOM producing the structure and values from within.
function walkElements(
  element: Node,
  parentPoint: CanvasPoint,
  closestOffsetParentPath: ElementPath,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ElementInstanceMetadataMap>,
  invalidatedPaths: Set<string>,
  invalidatedPathsForStylesheetCache: Set<string>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  additionalElementsToUpdate: Array<ElementPath>,
): {
  childPaths: ReadonlyArray<ElementPath>
  rootMetadata: ElementInstanceMetadataMap
  cachedPaths: Array<ElementPath>
} {
  if (isScene(element)) {
    // we found a nested scene, restart the walk
    const { metadata, cachedPaths: cachedPaths } = walkScene(
      element,
      validPaths,
      rootMetadataInStateRef,
      invalidatedPaths,
      invalidatedPathsForStylesheetCache,
      selectedViews,
      invalidated,
      scale,
      containerRectLazy,
      additionalElementsToUpdate,
    )

    const result = {
      childPaths: [],
      rootMetadata: metadata,
      cachedPaths: cachedPaths,
    }
    return result
  }
  if (element instanceof HTMLElement) {
    let closestOffsetParentPathInner: ElementPath = closestOffsetParentPath
    // If this element provides bounds for absolute children, we want to update the closest offset parent path
    if (isElementAContainingBlockForAbsolute(window.getComputedStyle(element))) {
      const deepestPath = getDeepestPathOnDomElement(element)
      if (deepestPath != null) {
        closestOffsetParentPathInner = deepestPath
      }
    }

    const pathsWithStrings = getPathWithStringsOnDomElement(element)
    for (const pathWithString of pathsWithStrings) {
      invalidatedPaths.delete(pathWithString.asString) // mutation!
    }

    const doNotTraverseAttribute = getDOMAttribute(element, UTOPIA_DO_NOT_TRAVERSE_KEY)
    const traverseChildren: boolean = doNotTraverseAttribute !== 'true'

    const globalFrame = globalFrameForElement(element, scale, containerRectLazy)

    // Check this is a path we're interested in, otherwise skip straight to the children
    const foundValidPaths = pathsWithStrings.filter((pathWithString) => {
      const staticPath = EP.makeLastPartOfPathStatic(pathWithString.path)
      return validPaths.some((validPath) => {
        return EP.pathsEqual(staticPath, validPath)
      })
    })

    // Build the metadata for the children of this DOM node.
    let childPaths: Array<ElementPath> = []
    let rootMetadataAccumulator: ElementInstanceMetadataMap = {}
    let cachedPathsAccumulator: Array<ElementPath> = []
    // TODO: we should not traverse the children when all elements of this subtree will be retrieved from cache anyway
    // WARNING: we need to retrieve the metadata of all elements of the subtree from the cache, because the SAVE_DOM_REPORT
    // action replaces (and not merges) the full metadata map
    if (traverseChildren) {
      element.childNodes.forEach((child) => {
        const {
          childPaths: childNodePaths,
          rootMetadata: rootMetadataInner,
          cachedPaths,
        } = walkElements(
          child,
          globalFrame,
          closestOffsetParentPathInner,
          validPaths,
          rootMetadataInStateRef,
          invalidatedPaths,
          invalidatedPathsForStylesheetCache,
          selectedViews,
          invalidated,
          scale,
          containerRectLazy,
          additionalElementsToUpdate,
        )
        childPaths.push(...childNodePaths)
        mergeMetadataMapsWithFragments_MUTATE(rootMetadataAccumulator, rootMetadataInner)
        cachedPathsAccumulator.push(...cachedPaths)
      })
    }

    const uniqueChildPaths = uniqBy(childPaths, EP.pathsEqual)

    const { collectedMetadata, cachedPaths, collectedPaths } = collectMetadata(
      element,
      pluck(foundValidPaths, 'path'),
      pluck(foundValidPaths, 'asString'),
      parentPoint,
      closestOffsetParentPath,
      uniqueChildPaths,
      scale,
      containerRectLazy,
      invalidatedPaths,
      invalidatedPathsForStylesheetCache,
      rootMetadataInStateRef,
      invalidated,
      selectedViews,
      additionalElementsToUpdate,
    )

    mergeMetadataMapsWithFragments_MUTATE(rootMetadataAccumulator, collectedMetadata)
    cachedPathsAccumulator = [...cachedPathsAccumulator, ...cachedPaths]
    return {
      rootMetadata: rootMetadataAccumulator,
      childPaths: collectedPaths,
      cachedPaths: cachedPathsAccumulator,
    }
  } else {
    return { childPaths: [], rootMetadata: {}, cachedPaths: [] }
  }
}
