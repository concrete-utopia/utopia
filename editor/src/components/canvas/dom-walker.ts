import * as React from 'react'
import { sides } from 'utopia-api'
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
} from '../../core/shared/math-utils'
import {
  CSSNumber,
  parseCSSLength,
  CSSPosition,
  positionValues,
} from '../inspector/common/css-utils'
import { CanvasContainerProps } from './ui-jsx-canvas'
import { camelCaseToDashed } from '../../core/shared/string-utils'
import {
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../editor/store/store-hook'
import {
  UTOPIA_DO_NOT_TRAVERSE_KEY,
  UTOPIA_PATHS_KEY,
  UTOPIA_SCENE_ID_KEY,
} from '../../core/model/utopia-constants'

import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { PRODUCTION_ENV } from '../../common/env-vars'
import { CanvasContainerID } from './canvas-types'
import { emptySet } from '../../core/shared/set-utils'
import { getPathWithStringsOnDomElement, PathWithString } from '../../core/shared/uid-utils'
import { mapDropNulls, pluck, uniqBy } from '../../core/shared/array-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { fastForEach } from '../../core/shared/utils'
import { MapLike } from 'typescript'

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
  if (computedStyle.display != null && computedStyle.display === 'flex') {
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

const LogDomWalkerPerformance = !PRODUCTION_ENV && typeof window.performance.mark === 'function'

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
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  elementPath: ElementPath,
  element: HTMLElement,
): Set<string> {
  const pathAsString = EP.toString(elementPath)
  const invalidated = invalidatedPathsForStylesheetCacheRef.current.has(pathAsString)
  const inCache = AttributesFromStyleSheetsCache.has(element)
  if (inCache && !invalidated) {
    return AttributesFromStyleSheetsCache.get(element)!
  }
  invalidatedPathsForStylesheetCacheRef.current.delete(pathAsString) // Mutation!
  const value = getAttributesComingFromStyleSheets(element)
  AttributesFromStyleSheetsCache.set(element, value)
  return value
}

function useResizeObserver(
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  selectedViews: React.MutableRefObject<Array<ElementPath>>,
  canvasInteractionHappening: React.MutableRefObject<boolean>,
) {
  const resizeObserver = React.useMemo((): ResizeObserver | null => {
    if (ObserversAvailable) {
      return new ResizeObserver((entries: any) => {
        if (canvasInteractionHappening.current) {
          // Only add the selected views
          fastForEach(selectedViews.current, (v) =>
            updateInvalidatedPaths((current) => current.add(EP.toString(v))),
          )
        } else {
          for (let entry of entries) {
            const sceneID = findParentScene(entry.target)
            if (sceneID != null) {
              updateInvalidatedScenes((current) => current.add(sceneID))
            }
          }
        }
      })
    } else {
      return null
    }
    /* eslint-disable-next-line react-hooks/exhaustive-deps */
  }, []) // the dependencies are empty because this should only evaluate once
  React.useEffect(() => {
    return function cleanup() {
      if (resizeObserver != null) {
        resizeObserver.disconnect()
      }
    }
  }, [resizeObserver])
  return resizeObserver
}

function useMutationObserver(
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  selectedViews: React.MutableRefObject<Array<ElementPath>>,
  canvasInteractionHappening: React.MutableRefObject<boolean>,
) {
  const mutationObserver = React.useMemo((): MutationObserver | null => {
    if (ObserversAvailable) {
      return new (window as any).MutationObserver((mutations: MutationRecord[]) => {
        if (canvasInteractionHappening.current) {
          // Only add the selected views
          fastForEach(selectedViews.current, (v) =>
            updateInvalidatedPaths((current) => current.add(EP.toString(v))),
          )
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
                  updateInvalidatedScenes((current) => current.add(sceneID))
                }
              }
            }
          }
        }
      })
    } else {
      return null
    }
    /* eslint-disable-next-line react-hooks/exhaustive-deps */
  }, []) // the dependencies are empty because this should only evaluate once
  React.useEffect(() => {
    return function cleanup() {
      if (mutationObserver != null) {
        mutationObserver.disconnect()
      }
    }
  }, [mutationObserver])
  return mutationObserver
}

function useInvalidateScenesWhenSelectedViewChanges(
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
): void {
  return useSelectorWithCallback(
    (store) => store.editor.selectedViews,
    (newSelectedViews) => {
      newSelectedViews.forEach((sv) => {
        const scenePath = EP.createBackwardsCompatibleScenePath(sv)
        if (scenePath != null) {
          const sceneID = EP.toString(scenePath)
          updateInvalidatedScenes((current) => current.add(sceneID))
          invalidatedPathsForStylesheetCacheRef.current.add(EP.toString(sv))
        }
      })
    },
  )
}

function useInvalidateInitCompleteOnMountCount(
  mountCount: number,
  domWalkerInvalidateCount: number,
): [boolean, () => void] {
  const initCompleteRef = React.useRef<boolean>(false)
  const previousMountCountRef = React.useRef<number>(mountCount)
  const previousDomWalkerInvalidateCountRef = React.useRef<number>(domWalkerInvalidateCount)

  const setInitComplete = React.useCallback(() => {
    initCompleteRef.current = true
  }, [])

  if (
    previousMountCountRef.current !== mountCount ||
    previousDomWalkerInvalidateCountRef.current !== domWalkerInvalidateCount
  ) {
    // mount count increased, re-initialize dom-walker
    initCompleteRef.current = false
    previousMountCountRef.current = mountCount
    previousDomWalkerInvalidateCountRef.current = domWalkerInvalidateCount
  }

  return [initCompleteRef.current, setInitComplete]
}
type ValueOrUpdater<S> = S | ((prevState: S) => S)
// todo move to file
export type SetValueCallback<S> = (
  valueOrUpdater: ValueOrUpdater<S>,
  doNotInvalidate?: 'do-not-invalidate',
) => void

function isSimpleValue<S>(valueOrUpdater: ValueOrUpdater<S>): valueOrUpdater is S {
  return typeof valueOrUpdater !== 'function'
}
function useStateAsyncInvalidate<S>(
  onInvalidate: () => void,
  initialState: S,
): [S, SetValueCallback<S>] {
  const stateRef = React.useRef(initialState)
  const setAndMarkInvalidated = React.useCallback(
    (valueOrUpdater: ValueOrUpdater<S>, doNotInvalidate?: 'do-not-invalidate') => {
      let resolvedNewValue: S
      if (isSimpleValue(valueOrUpdater)) {
        // TODO make this type nicer using a type guard
        resolvedNewValue = valueOrUpdater
      } else {
        resolvedNewValue = valueOrUpdater(stateRef.current)
      }
      stateRef.current = resolvedNewValue

      // TODO invalidate
      if (doNotInvalidate !== 'do-not-invalidate') {
        onInvalidate()
      }
    },
    [stateRef, onInvalidate],
  )

  return [stateRef.current, setAndMarkInvalidated]
}

function useThrottledCallback(callback: () => void): () => void {
  const timeoutRef = React.useRef<NodeJS.Timeout | number | null>(null)

  const callbackRef = React.useRef(callback)
  callbackRef.current = callback

  return React.useCallback(() => {
    if (timeoutRef.current == null) {
      timeoutRef.current = setTimeout(() => {
        timeoutRef.current = null
        callbackRef.current()
      }, 0)
    }
  }, [])
}

interface DomWalkerProps {
  selectedViews: Array<ElementPath>
  scale: number
  onDomReport: (
    elementMetadata: ReadonlyArray<ElementInstanceMetadata>,
    cachedPaths: Array<ElementPath>,
  ) => void
  canvasRootElementElementPath: ElementPath
  validRootPaths: Array<ElementPath>
  mountCount: number
  domWalkerInvalidateCount: number
  canvasInteractionHappening: boolean
}

function mergeFragmentMetadata(
  metadata: ReadonlyArray<ElementInstanceMetadata>,
): Array<ElementInstanceMetadata> {
  let working: MapLike<ElementInstanceMetadata> = {}

  fastForEach(metadata, (elementMetadata) => {
    const pathString = EP.toString(elementMetadata.elementPath)
    const existingMetadata = working[pathString]

    if (existingMetadata == null) {
      working[pathString] = elementMetadata
    } else {
      // We've hit a fragment, so remove the style etc., but keep the frames for selection
      const merged = elementInstanceMetadata(
        elementMetadata.elementPath,
        left('fragment'),
        {},
        boundingRectangle(
          existingMetadata.globalFrame ?? zeroCanvasRect,
          elementMetadata.globalFrame ?? zeroCanvasRect,
        ),
        boundingRectangle(
          existingMetadata.localFrame ?? zeroLocalRect,
          elementMetadata.localFrame ?? zeroLocalRect,
        ),
        existingMetadata.children.concat(elementMetadata.children),
        existingMetadata.rootElements.concat(elementMetadata.rootElements),
        false,
        false,
        emptySpecialSizeMeasurements,
        {},
        {},
        null,
        null,
      )

      working[pathString] = merged
    }
  })

  return Object.values(working)
}

export function useDomWalker(
  props: DomWalkerProps,
): [SetValueCallback<Set<string>>, SetValueCallback<Set<string>>, React.Ref<HTMLDivElement>] {
  const containerRef = React.useRef<HTMLDivElement>(null)

  const fireThrottledCallback = useThrottledCallback(() => {
    if (containerRef.current != null) {
      if (LogDomWalkerPerformance) {
        performance.mark('DOM_WALKER_START')
      }
      // Get some base values relating to the div this component creates.
      const refOfContainer = containerRef.current
      if (ObserversAvailable && resizeObserver != null && mutationObserver != null) {
        Array.from(document.querySelectorAll(`#${CanvasContainerID} *`)).map((elem) => {
          resizeObserver.observe(elem)
        })
        mutationObserver.observe(refOfContainer, MutationObserverConfig)
      }

      // getCanvasRectangleFromElement is costly, so I made it lazy. we only need the value inside globalFrameForElement
      const containerRect = lazyValue(() => {
        return getCanvasRectangleFromElement(refOfContainer, props.scale)
      })

      // This assumes that the canvas root is rendering a Storyboard fragment.
      // The necessary validPaths and the root fragment's template path comes from props,
      // because the fragment is invisible in the DOM.
      const { metadata, cachedPaths } = walkCanvasRootFragment(
        refOfContainer,
        0,
        props.canvasRootElementElementPath,
        props.validRootPaths,
        rootMetadataInStateRef,
        invalidatedPaths as ReadonlySet<string>, // this is not the nicest type here, but it should be fine for now :)
        updateInvalidatedPaths,
        invalidatedScenes as ReadonlySet<string>,
        updateInvalidatedScenes,
        invalidatedPathsForStylesheetCacheRef,
        props.selectedViews,
        !initComplete,
        props.scale,
        containerRect,
      )
      if (LogDomWalkerPerformance) {
        performance.mark('DOM_WALKER_END')
        performance.measure('DOM WALKER', 'DOM_WALKER_START', 'DOM_WALKER_END')
      }
      setInitComplete()

      // Fragments will appear as multiple separate entries with duplicate UIDs, so we need to handle those
      const fixedMetadata = mergeFragmentMetadata(metadata)

      props.onDomReport(fixedMetadata, cachedPaths)
    }
  })

  const rootMetadataInStateRef = useRefEditorState(
    (store) => store.editor.domMetadata as ReadonlyArray<ElementInstanceMetadata>,
  )
  const [invalidatedPaths, updateInvalidatedPaths] = useStateAsyncInvalidate<Set<string>>(
    fireThrottledCallback,
    emptySet(),
  ) // For invalidating specific paths only
  const [invalidatedScenes, updateInvalidatedScenes] = useStateAsyncInvalidate<Set<string>>(
    fireThrottledCallback,
    emptySet(),
  ) // For invalidating entire scenes and everything below them
  const invalidatedPathsForStylesheetCacheRef = React.useRef<Set<string>>(emptySet())
  const [initComplete, setInitComplete] = useInvalidateInitCompleteOnMountCount(
    props.mountCount,
    props.domWalkerInvalidateCount,
  )
  const selectedViewsRef = React.useRef(props.selectedViews)
  const canvasInteractionHappeningRef = React.useRef(props.canvasInteractionHappening)

  if (selectedViewsRef.current !== props.selectedViews) {
    selectedViewsRef.current = props.selectedViews
  }
  if (canvasInteractionHappeningRef.current !== props.canvasInteractionHappening) {
    canvasInteractionHappeningRef.current = props.canvasInteractionHappening
  }

  const resizeObserver = useResizeObserver(
    updateInvalidatedPaths,
    updateInvalidatedScenes,
    selectedViewsRef,
    canvasInteractionHappeningRef,
  )
  const mutationObserver = useMutationObserver(
    updateInvalidatedPaths,
    updateInvalidatedScenes,
    selectedViewsRef,
    canvasInteractionHappeningRef,
  )
  useInvalidateScenesWhenSelectedViewChanges(
    updateInvalidatedScenes,
    invalidatedPathsForStylesheetCacheRef,
  )

  React.useLayoutEffect(() => {
    fireThrottledCallback()
  })

  return [updateInvalidatedPaths, updateInvalidatedScenes, containerRef]
}

function collectMetadataForElement(
  element: HTMLElement,
  parentPoint: CanvasPoint,
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

  const specialSizeMeasurementsObject = getSpecialMeasurements(element, scale, containerRectLazy)

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
  allUnfilteredChildrenPaths: Array<ElementPath>,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  invalidatedPaths: ReadonlySet<string>,
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidated: boolean,
  selectedViews: Array<ElementPath>,
): { collectedMetadata: Array<ElementInstanceMetadata>; cachedPaths: Array<ElementPath> } {
  const shouldCollect = invalidated || isAnyPathInvalidated(stringPathsForElement, invalidatedPaths)
  if (shouldCollect && pathsForElement.length > 0) {
    const {
      tagName,
      globalFrame,
      localFrame,
      specialSizeMeasurementsObject,
    } = collectMetadataForElement(element, parentPoint, scale, containerRectLazy)

    const { computedStyle, attributeMetadata } = getComputedStyle(
      element,
      pathsForElement,
      invalidatedPathsForStylesheetCacheRef,
      selectedViews,
    )

    const collectedMetadata = pathsForElement.map((path) => {
      updateInvalidatedPaths((current) => {
        // sneaky mutation to improve performance
        ;(current as Set<string>).delete(EP.toString(path))
        return current
      }, 'do-not-invalidate')
      const rootsOrChildrenToAdd = pathsForElement.filter((otherPath) =>
        EP.isParentOf(path, otherPath),
      )
      const unfilteredChildrenPaths = allUnfilteredChildrenPaths.concat(rootsOrChildrenToAdd)

      let filteredChildPaths: ElementPath[] = []
      let filteredRootElements: ElementPath[] = []
      fastForEach(unfilteredChildrenPaths, (childPath) => {
        if (EP.isParentOf(path, childPath)) {
          if (EP.isRootElementOfInstance(childPath)) {
            filteredRootElements.push(childPath)
          } else {
            filteredChildPaths.push(childPath)
          }
        }
      })

      return elementInstanceMetadata(
        path,
        left(tagName),
        {},
        globalFrame,
        localFrame,
        filteredChildPaths,
        filteredRootElements,
        false,
        false,
        specialSizeMeasurementsObject,
        computedStyle,
        attributeMetadata,
        null,
        null, // This comes from the Spy Wrapper
      )
    })

    return {
      collectedMetadata: collectedMetadata,
      cachedPaths: [],
    }
  } else {
    const cachedMetadata = mapDropNulls((path) => {
      return MetadataUtils.findElementMetadata(path, rootMetadataInStateRef.current)
    }, pathsForElement)

    if (cachedMetadata.length === pathsForElement.length) {
      return {
        collectedMetadata: cachedMetadata,
        cachedPaths: pathsForElement,
      }
    } else {
      // If any path is missing cached metadata we must forcibly invalidate the element
      return collectMetadata(
        element,
        pathsForElement,
        stringPathsForElement,
        parentPoint,
        allUnfilteredChildrenPaths,
        scale,
        containerRectLazy,
        invalidatedPaths,
        updateInvalidatedPaths,
        invalidatedPathsForStylesheetCacheRef,
        rootMetadataInStateRef,
        true,
        selectedViews,
      )
    }
  }
}

function getComputedStyle(
  element: HTMLElement,
  paths: Array<ElementPath>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
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
    invalidatedPathsForStylesheetCacheRef,
    paths[0], // TODO is this sufficient to use the first path element for caching?
    element,
  )
  let computedStyle: ComputedStyle = {}
  let attributeMetadata: StyleAttributeMetadata = {}
  if (elementStyle != null) {
    Object.keys(elementStyle).forEach((key) => {
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

  const providesBoundsForChildren = isElementNonStatic(elementStyle)

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

  return specialSizeMeasurements(
    offset,
    coordinateSystemBounds,
    immediateParentBounds,
    parentProvidesLayout,
    isParentNonStatic,
    parentLayoutSystem,
    layoutSystemForChildren,
    providesBoundsForChildren,
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
  index: number,
  canvasRootPath: ElementPath,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedPaths: ReadonlySet<string>,
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  invalidatedScenes: ReadonlySet<string>,
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): {
  metadata: ReadonlyArray<ElementInstanceMetadata>
  cachedPaths: Array<ElementPath>
} {
  if (
    ObserversAvailable &&
    invalidatedPaths.size === 0 &&
    invalidatedScenes.size === 0 &&
    rootMetadataInStateRef.current.length > 0 &&
    !invalidated
  ) {
    // no mutation happened on the entire canvas, just return the old metadata
    return { metadata: rootMetadataInStateRef.current, cachedPaths: [canvasRootPath] }
  } else {
    const { childPaths: rootElements, rootMetadata, cachedPaths } = walkSceneInner(
      canvasRoot,
      index,
      validPaths,
      rootMetadataInStateRef,
      invalidatedPaths,
      updateInvalidatedPaths,
      invalidatedScenes,
      updateInvalidatedScenes,
      invalidatedPathsForStylesheetCacheRef,
      selectedViews,
      invalidated,
      scale,
      containerRectLazy,
    )
    // The Storyboard root being a fragment means it is invisible to us in the DOM walker,
    // so walkCanvasRootFragment will create a fake root ElementInstanceMetadata
    // to provide a home for the the (really existing) childMetadata
    const metadata: ElementInstanceMetadata = elementInstanceMetadata(
      canvasRootPath,
      left('Storyboard'),
      {},
      { x: 0, y: 0, width: 0, height: 0 } as CanvasRectangle,
      { x: 0, y: 0, width: 0, height: 0 } as LocalRectangle,
      rootElements,
      [],
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadatada,
      null,
      null, // this comes from the Spy Wrapper
    )
    return { metadata: [...rootMetadata, metadata], cachedPaths: cachedPaths }
  }
}

function walkScene(
  scene: HTMLElement,
  index: number,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedPaths: ReadonlySet<string>,
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  invalidatedScenes: ReadonlySet<string>,
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): {
  metadata: ReadonlyArray<ElementInstanceMetadata>
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
        (ObserversAvailable && invalidatedScenes.size > 0 && invalidatedScenes.has(sceneID))

      updateInvalidatedScenes((current) => {
        // mutating here and skipping invalidation
        current.delete(sceneID)
        return current
      }, 'do-not-invalidate')

      const { childPaths: rootElements, rootMetadata, cachedPaths } = walkSceneInner(
        scene,
        index,
        validPaths,
        rootMetadataInStateRef,
        invalidatedPaths,
        updateInvalidatedPaths,
        invalidatedScenes,
        updateInvalidatedScenes,
        invalidatedPathsForStylesheetCacheRef,
        selectedViews,
        invalidatedScene,
        scale,
        containerRectLazy,
      )

      const { collectedMetadata: sceneMetadata, cachedPaths: sceneCachedPaths } = collectMetadata(
        scene,
        [instancePath],
        [sceneID],
        canvasPoint({ x: 0, y: 0 }),
        rootElements,
        scale,
        containerRectLazy,
        invalidatedPaths,
        updateInvalidatedPaths,
        invalidatedPathsForStylesheetCacheRef,
        rootMetadataInStateRef,
        invalidatedScene,
        selectedViews,
      )
      return {
        metadata: [...rootMetadata, ...sceneMetadata],
        cachedPaths: [...cachedPaths, ...sceneCachedPaths],
      }
    }
  }
  return { metadata: [], cachedPaths: [] } // verify
}

function walkSceneInner(
  scene: HTMLElement,
  index: number,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedPaths: ReadonlySet<string>,
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  invalidatedScenes: ReadonlySet<string>,
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): {
  childPaths: Array<ElementPath>
  rootMetadata: ReadonlyArray<ElementInstanceMetadata>
  cachedPaths: Array<ElementPath>
} {
  const globalFrame: CanvasRectangle = globalFrameForElement(scene, scale, containerRectLazy)

  let childPaths: Array<ElementPath> = []
  let rootMetadataAccumulator: Array<ElementInstanceMetadata> = []
  let cachedPathsAccumulator: Array<ElementPath> = []

  scene.childNodes.forEach((childNode) => {
    const { childPaths: childNodePaths, rootMetadata, cachedPaths } = walkElements(
      childNode,
      index,
      0,
      globalFrame,
      validPaths,
      rootMetadataInStateRef,
      invalidatedPaths,
      updateInvalidatedPaths,
      invalidatedScenes,
      updateInvalidatedScenes,
      invalidatedPathsForStylesheetCacheRef,
      selectedViews,
      invalidated,
      scale,
      containerRectLazy,
    )

    childPaths.push(...childNodePaths)
    rootMetadataAccumulator.push(...rootMetadata)
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
  index: number,
  depth: number,
  parentPoint: CanvasPoint,
  validPaths: Array<ElementPath>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedPaths: ReadonlySet<string>,
  updateInvalidatedPaths: SetValueCallback<Set<string>>,
  invalidatedScenes: ReadonlySet<string>,
  updateInvalidatedScenes: SetValueCallback<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<ElementPath>,
  invalidated: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): {
  childPaths: ReadonlyArray<ElementPath>
  rootMetadata: ReadonlyArray<ElementInstanceMetadata>
  cachedPaths: Array<ElementPath>
} {
  if (isScene(element)) {
    // we found a nested scene, restart the walk
    const { metadata, cachedPaths: cachedPaths } = walkScene(
      element,
      index,
      validPaths,
      rootMetadataInStateRef,
      invalidatedPaths,
      updateInvalidatedPaths,
      invalidatedScenes,
      updateInvalidatedScenes,
      invalidatedPathsForStylesheetCacheRef,
      selectedViews,
      invalidated,
      scale,
      containerRectLazy,
    )

    const result = {
      childPaths: [],
      rootMetadata: metadata,
      cachedPaths: cachedPaths,
    }
    return result
  }
  if (element instanceof HTMLElement) {
    // Determine the uid of this element if it has one.
    const pathsWithStrings = getPathWithStringsOnDomElement(element)

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
    let rootMetadataAccumulator: ReadonlyArray<ElementInstanceMetadata> = []
    let cachedPathsAccumulator: Array<ElementPath> = []
    if (traverseChildren) {
      element.childNodes.forEach((child, childIndex) => {
        const {
          childPaths: childNodePaths,
          rootMetadata: rootMetadataInner,
          cachedPaths,
        } = walkElements(
          child,
          childIndex,
          depth + 1,
          globalFrame,
          validPaths,
          rootMetadataInStateRef,
          invalidatedPaths,
          updateInvalidatedPaths,
          invalidatedScenes,
          updateInvalidatedScenes,
          invalidatedPathsForStylesheetCacheRef,
          selectedViews,
          invalidated,
          scale,
          containerRectLazy,
        )
        childPaths.push(...childNodePaths)
        rootMetadataAccumulator = [...rootMetadataAccumulator, ...rootMetadataInner]
        cachedPathsAccumulator.push(...cachedPaths)
      })
    }

    const uniqueChildPaths = uniqBy(childPaths, EP.pathsEqual)

    const { collectedMetadata, cachedPaths } = collectMetadata(
      element,
      pluck(foundValidPaths, 'path'),
      pluck(foundValidPaths, 'asString'),
      parentPoint,
      uniqueChildPaths,
      scale,
      containerRectLazy,
      invalidatedPaths,
      updateInvalidatedPaths,
      invalidatedPathsForStylesheetCacheRef,
      rootMetadataInStateRef,
      invalidated,
      selectedViews,
    )

    rootMetadataAccumulator = [...rootMetadataAccumulator, ...collectedMetadata]
    cachedPathsAccumulator = [...cachedPathsAccumulator, ...cachedPaths]
    return {
      rootMetadata: rootMetadataAccumulator,
      childPaths: collectedMetadata.map((metadata) => metadata.elementPath), // TODO why not extract childPaths from the metadata?
      cachedPaths: cachedPathsAccumulator,
    }
  } else {
    return { childPaths: [], rootMetadata: [], cachedPaths: [] }
  }
}
