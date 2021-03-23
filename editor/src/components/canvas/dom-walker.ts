import * as React from 'react'
import { sides } from 'utopia-api'
import * as TP from '../../core/shared/template-path'
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
import { id, TemplatePath, InstancePath } from '../../core/shared/project-file-types'
import { getCanvasRectangleFromElement, getDOMAttribute } from '../../core/shared/dom-utils'
import { applicative4Either, isRight, left } from '../../core/shared/either'
import Utils from '../../utils/utils'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  LocalPoint,
  LocalRectangle,
  localRectangle,
  roundToNearestHalf,
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
  UTOPIA_LABEL_KEY,
  UTOPIA_ORIGINAL_ID_KEY,
  UTOPIA_UIDS_KEY,
  UTOPIA_UID_ORIGINAL_PARENTS_KEY,
  UTOPIA_UID_PARENTS_KEY,
} from '../../core/model/utopia-constants'
import ResizeObserver from 'resize-observer-polyfill'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { PRODUCTION_ENV } from '../../common/env-vars'
import { CanvasContainerID } from './canvas-types'
import { emptySet } from '../../core/shared/set-utils'
import { useForceUpdate } from '../editor/hook-utils'
import { extractOriginalUidFromIndexedUid, getUIDsOnDomELement } from '../../core/shared/uid-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import { optionalMap } from '../../core/shared/optional-utils'

const MutationObserverConfig = { attributes: true, childList: true, subtree: true }
const ObserversAvailable = (window as any).MutationObserver != null && ResizeObserver != null

function findValidPath(uid: string | null, validPathsString: Array<string>): InstancePath | null {
  if (uid == null) {
    return null
  }
  const validPaths = validPathsString.map(TP.fromString) // Hi Rheese :)
  return (
    mapDropNulls((validPath) => {
      if (TP.isScenePath(validPath)) {
        return null
      }
      const lastPathElement = TP.toUid(validPath)
      if (
        extractOriginalUidFromIndexedUid(uid) === extractOriginalUidFromIndexedUid(lastPathElement)
      ) {
        const parentPath = TP.parentPath(validPath)
        return TP.appendToPath(parentPath, uid)
      }
      return null
    }, validPaths)[0] ?? null
  )
}

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
    node instanceof HTMLElement &&
    node.attributes.getNamedItemNS(null, 'data-utopia-scene-id') != null &&
    node.attributes.getNamedItemNS(null, 'data-utopia-valid-paths') != null
  )
}

function findParentScene(target: HTMLElement): string | null {
  const sceneID = getDOMAttribute(target, 'data-utopia-scene-id')
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
      var rules = sheets[i].rules || sheets[i].cssRules
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
  templatePath: TemplatePath,
  element: HTMLElement,
): Set<string> {
  const pathAsString = TP.toString(templatePath)
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

function useResizeObserver(invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>) {
  const resizeObserver = React.useMemo((): ResizeObserver | null => {
    if (ObserversAvailable) {
      return new ResizeObserver((entries: any) => {
        for (let entry of entries) {
          const sceneID = findParentScene(entry.target)
          if (sceneID != null) {
            invalidatedSceneIDsRef.current.add(sceneID)
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

function useMutationObserver(invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>) {
  const mutationObserver = React.useMemo((): MutationObserver | null => {
    if (ObserversAvailable) {
      return new (window as any).MutationObserver((mutations: MutationRecord[]) => {
        for (let mutation of mutations) {
          if (
            mutation.attributeName === 'style' ||
            mutation.addedNodes.length > 0 ||
            mutation.removedNodes.length > 0
          ) {
            if (mutation.target instanceof HTMLElement) {
              const sceneID = findParentScene(mutation.target)
              if (sceneID != null) {
                invalidatedSceneIDsRef.current.add(sceneID)
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
  invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
): void {
  const forceUpdate = useForceUpdate()
  return useSelectorWithCallback(
    (store) => store.editor.selectedViews,
    (newSelectedViews) => {
      newSelectedViews.forEach((sv) => {
        const scenePath = TP.scenePathPartOfTemplatePath(sv)
        const sceneID = TP.toString(scenePath)
        invalidatedSceneIDsRef.current.add(sceneID)
        invalidatedPathsForStylesheetCacheRef.current.add(TP.toString(sv))
        forceUpdate()
      })
    },
  )
}

function useInvalidateInitCompleteOnMountCount(mountCount: number): [boolean, () => void] {
  const initCompleteRef = React.useRef<boolean>(false)
  const previousMountCountRef = React.useRef<number>(mountCount)

  const setInitComplete = React.useCallback(() => {
    initCompleteRef.current = true
  }, [])

  if (previousMountCountRef.current !== mountCount) {
    // mount count increased, re-initialize dom-walker
    initCompleteRef.current = false
    previousMountCountRef.current = mountCount
  }

  return [initCompleteRef.current, setInitComplete]
}

export function useDomWalker(props: CanvasContainerProps): React.Ref<HTMLDivElement> {
  const containerRef = React.useRef<HTMLDivElement>(null)
  const rootMetadataInStateRef = useRefEditorState(
    (store) => store.editor.domMetadataKILLME as ReadonlyArray<ElementInstanceMetadata>,
  )
  const invalidatedSceneIDsRef = React.useRef<Set<string>>(emptySet())
  const invalidatedPathsForStylesheetCacheRef = React.useRef<Set<string>>(emptySet())
  const [initComplete, setInitComplete] = useInvalidateInitCompleteOnMountCount(props.mountCount)
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'useDomWalker selectedViews',
  )
  const resizeObserver = useResizeObserver(invalidatedSceneIDsRef)
  const mutationObserver = useMutationObserver(invalidatedSceneIDsRef)
  useInvalidateScenesWhenSelectedViewChanges(
    invalidatedSceneIDsRef,
    invalidatedPathsForStylesheetCacheRef,
  )

  React.useLayoutEffect(() => {
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
      const rootMetadata: ReadonlyArray<ElementInstanceMetadata> = walkCanvasRootFragment(
        refOfContainer,
        0,
        props.canvasRootElementTemplatePath,
        props.validRootPaths.map(TP.toString),
        rootMetadataInStateRef,
        invalidatedSceneIDsRef,
        invalidatedPathsForStylesheetCacheRef,
        selectedViews,
        initComplete,
        props.scale,
        containerRect,
      )
      if (LogDomWalkerPerformance) {
        performance.mark('DOM_WALKER_END')
        performance.measure('DOM WALKER', 'DOM_WALKER_START', 'DOM_WALKER_END')
      }
      setInitComplete()
      props.onDomReport(rootMetadata)
    }
  })

  return containerRef
}

function collectMetadata(
  element: HTMLElement,
  uidAttribute: string | null,
  instancePath: InstancePath,
  parentPoint: CanvasPoint,
  children: InstancePath[],
  scale: number,
  containerRectLazy: () => CanvasRectangle,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<TemplatePath>,
): ElementInstanceMetadata {
  const globalFrame = globalFrameForElement(element, scale, containerRectLazy)
  const localFrame = localRectangle(Utils.offsetRect(globalFrame, Utils.negate(parentPoint)))

  const originalUIDAttribute = getDOMAttribute(element, UTOPIA_ORIGINAL_ID_KEY)
  const labelAttribute = getDOMAttribute(element, UTOPIA_LABEL_KEY)
  let elementProps: any = {}
  if (uidAttribute != null) {
    elementProps[UTOPIA_UIDS_KEY] = uidAttribute // TODO Balazs we are making a fake prop with a single UID instead of the UID array – maybe this means changes to mergeComponentMetadata
  }
  if (originalUIDAttribute != null) {
    elementProps[UTOPIA_ORIGINAL_ID_KEY] = originalUIDAttribute
  }
  if (labelAttribute != null) {
    elementProps[UTOPIA_LABEL_KEY] = labelAttribute
  }

  const { computedStyle, attributeMetadata } = getComputedStyle(
    element,
    instancePath,
    invalidatedPathsForStylesheetCacheRef,
    selectedViews,
  )

  return elementInstanceMetadata(
    instancePath,
    left(element.tagName.toLowerCase()),
    elementProps,
    globalFrame,
    localFrame,
    children,
    false,
    false,
    getSpecialMeasurements(element, scale, containerRectLazy),
    computedStyle,
    attributeMetadata,
  )
}

function getComputedStyle(
  element: HTMLElement,
  path: TemplatePath,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<TemplatePath>,
): { computedStyle: ComputedStyle | null; attributeMetadata: StyleAttributeMetadata | null } {
  const isSelected = selectedViews.some((sv) => TP.pathsEqual(sv, path))
  if (!isSelected) {
    // the element is not among the selected views, skip computing the style
    return {
      computedStyle: null,
      attributeMetadata: null,
    }
  }
  const elementStyle = window.getComputedStyle(element)
  const attributesSetByStylesheet = getCachedAttributesComingFromStyleSheets(
    invalidatedPathsForStylesheetCacheRef,
    path,
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
  canvasRootPath: TemplatePath,
  validPaths: Array<string>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<TemplatePath>,
  initComplete: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): ReadonlyArray<ElementInstanceMetadata> {
  if (
    ObserversAvailable &&
    invalidatedSceneIDsRef.current.size === 0 &&
    rootMetadataInStateRef.current.length > 0 &&
    initComplete
  ) {
    // no mutation happened on the entire canvas, just return the old metadata
    return rootMetadataInStateRef.current
  } else {
    const { childPaths: rootElements, rootMetadata } = walkSceneInner(
      canvasRoot,
      index,
      canvasRootPath,
      validPaths,
      rootMetadataInStateRef,
      invalidatedSceneIDsRef,
      invalidatedPathsForStylesheetCacheRef,
      selectedViews,
      initComplete,
      scale,
      containerRectLazy,
    )
    // The Storyboard root being a fragment means it is invisible to us in the DOM walker,
    // so walkCanvasRootFragment will create a fake root ElementInstanceMetadata
    // to provide a home for the the (really existing) childMetadata
    const metadata: ElementInstanceMetadata = elementInstanceMetadata(
      canvasRootPath as InstancePath,
      left('Storyboard'),
      {},
      { x: 0, y: 0, width: 0, height: 0 } as CanvasRectangle,
      { x: 0, y: 0, width: 0, height: 0 } as LocalRectangle,
      rootElements,
      false,
      false,
      emptySpecialSizeMeasurements,
      emptyComputedStyle,
      emptyAttributeMetadatada,
    )
    return [...rootMetadata, metadata]
  }
}

function walkScene(
  scene: HTMLElement,
  index: number,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<TemplatePath>,
  initComplete: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): ReadonlyArray<ElementInstanceMetadata> {
  if (scene instanceof HTMLElement) {
    // Right now this assumes that only UtopiaJSXComponents can be rendered via scenes,
    // and that they can only have a single root element
    const sceneIndexAttr = scene.attributes.getNamedItemNS(null, 'data-utopia-scene-id')
    const validPathsAttr = scene.attributes.getNamedItemNS(null, 'data-utopia-valid-paths')
    const sceneID = sceneIndexAttr?.value ?? null
    const scenePath = sceneID == null ? null : TP.fromString(sceneID)

    if (
      sceneID != null &&
      scenePath != null &&
      TP.isScenePath(scenePath) &&
      validPathsAttr != null
    ) {
      const validPaths = validPathsAttr.value.split(' ')
      let cachedMetadata: ElementInstanceMetadata | null = null
      if (ObserversAvailable && invalidatedSceneIDsRef.current != null) {
        if (!invalidatedSceneIDsRef.current.has(sceneID)) {
          // we can use the cache, if it exists
          const elementFromCurrentMetadata = MetadataUtils.findElementMetadata(
            scenePath,
            rootMetadataInStateRef.current,
          )
          if (elementFromCurrentMetadata != null) {
            cachedMetadata = elementFromCurrentMetadata
          }
        } else {
          // we proceed with the walk
          invalidatedSceneIDsRef.current.delete(sceneID) // Mutation!
        }
      }

      if (cachedMetadata == null || !initComplete) {
        const { childPaths: rootElements, rootMetadata } = walkSceneInner(
          scene,
          index,
          scenePath,
          validPaths,
          rootMetadataInStateRef,
          invalidatedSceneIDsRef,
          invalidatedPathsForStylesheetCacheRef,
          selectedViews,
          initComplete,
          scale,
          containerRectLazy,
        )

        const sceneMetadata = collectMetadata(
          scene,
          null,
          TP.instancePathForElementAtScenePath(scenePath),
          canvasPoint({ x: 0, y: 0 }),
          rootElements,
          scale,
          containerRectLazy,
          invalidatedPathsForStylesheetCacheRef,
          selectedViews,
        )
        return [...rootMetadata, sceneMetadata]
      } else {
        let rootMetadataAccumulator = [cachedMetadata]
        // Push the cached metadata for everything from this scene downwards
        Utils.fastForEach(rootMetadataInStateRef.current, (elem) => {
          if (TP.isAncestorOf(elem.templatePath, scenePath)) {
            rootMetadataAccumulator.push(elem)
          }
        })
        return rootMetadataAccumulator
      }
    }
  }
  return [] // verify
}

function walkSceneInner(
  scene: HTMLElement,
  index: number,
  scenePath: TemplatePath,
  validPaths: Array<string>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<TemplatePath>,
  initComplete: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): { childPaths: Array<InstancePath>; rootMetadata: ReadonlyArray<ElementInstanceMetadata> } {
  const globalFrame: CanvasRectangle = globalFrameForElement(scene, scale, containerRectLazy)

  let childPaths: Array<InstancePath> = []
  let rootMetadataAccumulator: Array<ElementInstanceMetadata> = []

  scene.childNodes.forEach((childNode) => {
    const { childPaths: childNodePaths, rootMetadata } = walkElements(
      childNode,
      index,
      0,
      globalFrame,
      scenePath,
      validPaths,
      rootMetadataInStateRef,
      invalidatedSceneIDsRef,
      invalidatedPathsForStylesheetCacheRef,
      selectedViews,
      initComplete,
      scale,
      containerRectLazy,
    )

    childPaths.push(...childNodePaths)
    rootMetadataAccumulator.push(...rootMetadata)
  })

  return { childPaths: childPaths, rootMetadata: rootMetadataAccumulator }
}

// Walks through the DOM producing the structure and values from within.
function walkElements(
  element: Node,
  index: number,
  depth: number,
  parentPoint: CanvasPoint,
  uniqueParentPath: TemplatePath,
  validPaths: Array<string>,
  rootMetadataInStateRef: React.MutableRefObject<ReadonlyArray<ElementInstanceMetadata>>,
  invalidatedSceneIDsRef: React.MutableRefObject<Set<string>>,
  invalidatedPathsForStylesheetCacheRef: React.MutableRefObject<Set<string>>,
  selectedViews: Array<TemplatePath>,
  initComplete: boolean,
  scale: number,
  containerRectLazy: () => CanvasRectangle,
): {
  childPaths: ReadonlyArray<InstancePath>
  rootMetadata: ReadonlyArray<ElementInstanceMetadata>
} {
  if (isScene(element)) {
    // we found a nested scene, restart the walk
    const result = {
      childPaths: [],
      rootMetadata: walkScene(
        element,
        index,
        rootMetadataInStateRef,
        invalidatedSceneIDsRef,
        invalidatedPathsForStylesheetCacheRef,
        selectedViews,
        initComplete,
        scale,
        containerRectLazy,
      ),
    }
    return result
  }
  if (element instanceof HTMLElement) {
    // Determine the uid of this element if it has one.
    const uids = getUIDsOnDomELement(element)
    if (uids != null) {
      const results = uids.map((uidAttribute) => {
        const doNotTraverseAttribute = getDOMAttribute(element, UTOPIA_DO_NOT_TRAVERSE_KEY)

        const traverseChildren: boolean = doNotTraverseAttribute !== 'true'

        const globalFrame = globalFrameForElement(element, scale, containerRectLazy)

        // Check this is a path we're interested in, otherwise skip straight to the children
        const foundValidPath = findValidPath(uidAttribute, validPaths)
        const pathForChildren = foundValidPath ?? uniqueParentPath

        // Build the metadata for the children of this DOM node.
        let childPaths: Array<InstancePath> = []
        let rootMetadataAccumulator: ReadonlyArray<ElementInstanceMetadata> = []
        if (traverseChildren) {
          element.childNodes.forEach((child, childIndex) => {
            const { childPaths: childNodePaths, rootMetadata: rootMetadataInner } = walkElements(
              child,
              childIndex,
              depth + 1,
              globalFrame,
              pathForChildren,
              validPaths,
              rootMetadataInStateRef,
              invalidatedSceneIDsRef,
              invalidatedPathsForStylesheetCacheRef,
              selectedViews,
              initComplete,
              scale,
              containerRectLazy,
            )
            childPaths.push(...childNodePaths)
            rootMetadataAccumulator = [...rootMetadataAccumulator, ...rootMetadataInner]
          })
        }

        const filteredChildPaths = childPaths.filter((childPath) =>
          TP.pathsEqual(foundValidPath, TP.parentPath(childPath)),
        )

        if (foundValidPath != null) {
          const collectedMetadata = collectMetadata(
            element,
            uidAttribute,
            foundValidPath,
            parentPoint,
            filteredChildPaths,
            scale,
            containerRectLazy,
            invalidatedPathsForStylesheetCacheRef,
            selectedViews,
          )

          rootMetadataAccumulator = [...rootMetadataAccumulator, collectedMetadata]
          return {
            rootMetadata: rootMetadataAccumulator,
            childPaths: [collectedMetadata.templatePath],
          }
        } else {
          return { childPaths: childPaths, rootMetadata: rootMetadataAccumulator }
        }
      })
      return results.reduce((acc, result) => {
        return {
          childPaths: acc.childPaths.concat(result.childPaths),
          rootMetadata: acc.rootMetadata.concat(result.rootMetadata),
        }
      })
    } else {
      return { childPaths: [], rootMetadata: [] }
    }
  } else {
    return { childPaths: [], rootMetadata: [] }
  }
}
