import React from 'react'
import { sides } from 'utopia-api/core'
import * as EP from '../../core/shared/element-path'
import type {
  DetectedLayoutSystem,
  SpecialSizeMeasurements,
  ElementInstanceMetadataMap,
  GridContainerProperties,
  GridElementProperties,
  DomElementMetadata,
  GridAutoOrTemplateBase,
  BorderWidths,
  GridPositionOrSpan,
} from '../../core/shared/element-template'
import {
  specialSizeMeasurements,
  gridContainerProperties,
  gridElementProperties,
  gridAutoOrTemplateFallback,
  domElementMetadata,
  gridAutoOrTemplateDimensions,
  isGridSpan,
} from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { ElementCanvasRectangleCache } from '../../core/shared/dom-utils'
import {
  getCanvasRectangleFromElement,
  getDOMAttribute,
  hugPropertiesFromStyleMap,
} from '../../core/shared/dom-utils'
import {
  applicative4Either,
  defaultEither,
  isRight,
  left,
  eitherToMaybe,
  mapEither,
} from '../../core/shared/either'
import Utils from '../../utils/utils'
import type { CanvasPoint, CanvasRectangle, LocalPoint } from '../../core/shared/math-utils'
import {
  canvasPoint,
  roundToNearestHalf,
  canvasRectangle,
  stretchRect,
} from '../../core/shared/math-utils'
import type { CSSNumber, CSSPosition } from '../inspector/common/css-utils'
import {
  parseCSSLength,
  positionValues,
  parseDirection,
  parseFlexDirection,
  parseCSSPx,
  parseGridPosition,
  parseGridRange,
  parseGridAutoOrTemplateBase,
  parseGridAutoFlow,
  isCSSKeyword,
  isDynamicGridRepeat,
} from '../inspector/common/css-utils'
import type { UtopiaStoreAPI } from '../editor/store/store-hook'
import { UTOPIA_SCENE_ID_KEY, UTOPIA_UID_KEY } from '../../core/model/utopia-constants'
import { emptySet } from '../../core/shared/set-utils'
import {
  getDeepestPathOnDomElement,
  getPathsOnDomElement,
  getPathStringsOnDomElement,
} from '../../core/shared/uid-utils'
import { forceNotNull } from '../../core/shared/optional-utils'
import { fastForEach } from '../../core/shared/utils'
import type { EditorState, EditorStorePatched } from '../editor/store/editor-state'
import { shallowEqual } from '../../core/shared/equality-utils'
import {
  getAlignContent,
  getFlexAlignment,
  getFlexJustifyContent,
  getSelfAlignment,
  MaxContent,
} from '../inspector/inspector-common'
import type { EditorDispatch } from '../editor/action-types'
import { runDOMWalker } from '../editor/actions/action-creators'
import { CanvasContainerOuterId } from './canvas-component-entry'
import { ElementsToRerenderGLOBAL } from './ui-jsx-canvas'
import type { GridCellGlobalFrames } from './canvas-strategies/strategies/grid-helpers'
import { GridMeasurementHelperKey } from './controls/grid-controls-for-strategies'
import { ObserversAvailable, ResizeObserver } from './observers'

const MutationObserverConfig = { attributes: true, childList: true, subtree: true }

function elementLayoutSystem(computedStyle: CSSStyleDeclaration | null): DetectedLayoutSystem {
  if (computedStyle == null) {
    return 'none'
  }
  if (computedStyle.display != null) {
    if (computedStyle.display.includes('flex')) {
      return 'flex'
    }
    if (computedStyle.display.includes('grid')) {
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
  // https://developer.mozilla.org/en-US/docs/Web/CSS/contain
  if (
    computedStyle.contain.includes('layout') ||
    computedStyle.contain.includes('paint') ||
    computedStyle.contain.includes('strict') ||
    computedStyle.contain.includes('content')
  ) {
    return true
  }
  return false
}

export const applicativeSidesPxTransform = (
  t: CSSNumber,
  r: CSSNumber,
  b: CSSNumber,
  l: CSSNumber,
) =>
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

function findParentScene(target: Element): string | null {
  // First check if the node is a Scene element, which could be nested at any level
  const sceneID = getDOMAttribute(target, UTOPIA_SCENE_ID_KEY)
  if (sceneID != null) {
    return sceneID
  } else {
    const parent = target.parentElement

    if (parent != null) {
      const parentPath = getDeepestPathOnDomElement(parent)
      const parentIsStoryboard = parentPath == null || EP.isStoryboardPath(parentPath)
      if (parentIsStoryboard) {
        // If the parent element is the storyboard, then we've reached the top and have to stop
        const allPaths = getPathStringsOnDomElement(target)
        allPaths.sort((a, b) => a.length - b.length)
        const shallowestPath = allPaths[0]
        if (shallowestPath != null) {
          return shallowestPath
        }
      } else {
        return findParentScene(parent)
      }
    }
  }

  return null
}

function findNearestElementWithPath(element: Element | null): ElementPath | null {
  if (element == null) {
    return null
  }
  const path = getDeepestPathOnDomElement(element)
  if (path != null) {
    return path
  }
  const parent = element.parentElement
  return findNearestElementWithPath(parent)
}

export function lazyValue<T>(getter: () => T) {
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

export function getAttributesComingFromStyleSheets(element: HTMLElement): Set<string> {
  let appliedAttributes = new Set<string>()
  const sheets = document.styleSheets
  for (const i in sheets) {
    try {
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
    } catch (e) {
      // ignore error, either JSDOM unit test related or we're trying to read one
      // of the editor stylesheets from a CDN URL in a way that is blocked by our
      // cross-origin policies
    }
  }
  return appliedAttributes
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

export interface DomWalkerMutableStateData {
  invalidatedPaths: Set<string> // warning: all subtrees under each invalidated path should invalidated
  invalidatedPathsForStylesheetCache: Set<string>
  initComplete: boolean
  mutationObserver: MutationObserver
  resizeObserver: ResizeObserver
  gridControlObserver: MutationObserver
}

export function createDomWalkerMutableState(
  editorStoreApi: UtopiaStoreAPI,
  dispatch: EditorDispatch,
): DomWalkerMutableStateData {
  const mutableData: DomWalkerMutableStateData = {
    invalidatedPaths: emptySet(),
    invalidatedPathsForStylesheetCache: emptySet(),
    initComplete: true,
    mutationObserver: null as any,
    resizeObserver: null as any,
    gridControlObserver: null as any,
  }

  const observers = initDomWalkerObservers(mutableData, editorStoreApi, dispatch)
  mutableData.mutationObserver = observers.mutationObserver
  mutableData.resizeObserver = observers.resizeObserver
  mutableData.gridControlObserver = observers.gridControlObserver
  return mutableData
}

export const DomWalkerMutableStateCtx = React.createContext<DomWalkerMutableStateData | null>(null)
function useDomWalkerMutableStateContext() {
  return forceNotNull(
    `DomWalkerMutableStateCtx needs a Provider`,
    React.useContext(DomWalkerMutableStateCtx),
  )
}

export function resubscribeObservers(domWalkerMutableState: {
  mutationObserver: MutationObserver
  resizeObserver: ResizeObserver
  gridControlObserver: MutationObserver
}) {
  const canvasRootContainer = document.getElementById(CanvasContainerOuterId)
  const gridControls = document.getElementById('grid-controls')

  if (
    ObserversAvailable &&
    canvasRootContainer != null &&
    domWalkerMutableState.resizeObserver != null &&
    domWalkerMutableState.mutationObserver != null
  ) {
    document.querySelectorAll(`#${CanvasContainerOuterId} [${UTOPIA_UID_KEY}]`).forEach((elem) => {
      domWalkerMutableState.resizeObserver.observe(elem)
    })
    domWalkerMutableState.mutationObserver.observe(canvasRootContainer, MutationObserverConfig)
    if (gridControls != null) {
      domWalkerMutableState.gridControlObserver.observe(gridControls, MutationObserverConfig)
    }
  }
}

function isCanvasInteractionHappening(store: EditorStorePatched): boolean {
  const interactionSessionActive = store.editor.canvas.interactionSession != null
  return interactionSessionActive || ElementsToRerenderGLOBAL.current !== 'rerender-all-elements'
}

export function initDomWalkerObservers(
  domWalkerMutableState: DomWalkerMutableStateData,
  editorStore: UtopiaStoreAPI,
  dispatch: EditorDispatch,
): {
  resizeObserver: ResizeObserver
  mutationObserver: MutationObserver
  gridControlObserver: MutationObserver
} {
  let domWalkerTimeoutID: number | null = null
  function queueUpDomWalker(restrictToElements: Array<ElementPath> | null): void {
    if (domWalkerTimeoutID == null) {
      domWalkerTimeoutID = window.setTimeout(() => {
        dispatch([runDOMWalker(restrictToElements)])
        domWalkerTimeoutID = null
      })
    }
  }

  // Warning: I modified this code so it runs in all modes, not just in live mode. We still don't trigger
  // the DOM walker during canvas interactions, so the performance impact doesn't seem that bad. But it is
  // necessary, because after remix navigation, and after dynamic changes coming from loaders sometimes the
  // dom walker was not executed after all the changes.
  //
  // This was the original comment here when this only ran in live mode:
  //
  // Warning: These observers only trigger the DOM walker whilst in live mode to ensure metadata is up to date
  // when interacting with the actual running application / components. There are likely edge cases where we
  // also want these to trigger the DOM walker whilst in select mode, but if we find such a case we need to
  // adequately assess the performance impact of doing so, and ideally find a way to only do so when the observed
  // change was not triggered by a user interaction
  const resizeObserver = new ResizeObserver((entries: ResizeObserverEntry[]) => {
    const canvasInteractionHappening = isCanvasInteractionHappening(editorStore.getState())
    const selectedViews = editorStore.getState().editor.selectedViews
    if (canvasInteractionHappening) {
      // Warning this only adds the selected views instead of the observed element
      fastForEach(selectedViews, (v) => {
        domWalkerMutableState.invalidatedPaths.add(EP.toString(v))
      })
    } else {
      let shouldRunDOMWalker = false
      for (let entry of entries) {
        const sceneID = findParentScene(entry.target)
        if (sceneID != null) {
          domWalkerMutableState.invalidatedPaths.add(sceneID) // warning this invalidates the entire scene instead of just the observed element.
          shouldRunDOMWalker = true
        }
      }
      if (shouldRunDOMWalker) {
        queueUpDomWalker(null)
      }
    }
  })

  const mutationObserver = new window.MutationObserver((mutations: MutationRecord[]) => {
    const canvasInteractionHappening = isCanvasInteractionHappening(editorStore.getState())
    const selectedViews = editorStore.getState().editor.selectedViews

    if (canvasInteractionHappening) {
      // Warning this only adds the selected views instead of the observed element
      fastForEach(selectedViews, (v) => {
        domWalkerMutableState.invalidatedPaths.add(EP.toString(v))
      })
    } else {
      let shouldRunDOMWalker = false
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
              shouldRunDOMWalker = true
            }
          }
        }
      }
      if (shouldRunDOMWalker) {
        queueUpDomWalker(null)
      }
    }
  })

  const gridControlObserver = new window.MutationObserver((mutations: MutationRecord[]) => {
    let shouldRunDOMWalkerOnPath = null
    mutations.forEach((mutation) => {
      if (mutation.target instanceof HTMLElement) {
        for (const child of mutation.target.children) {
          const gridPath = child.getAttribute('data-grid-path')
          if (gridPath != null) {
            shouldRunDOMWalkerOnPath = EP.fromString(gridPath)
          }
        }
      }
    })
    if (shouldRunDOMWalkerOnPath != null) {
      queueUpDomWalker([shouldRunDOMWalkerOnPath])
    }
  })

  return { resizeObserver, mutationObserver, gridControlObserver }
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
  closestOffsetParentPath: ElementPath | null,
  scale: number,
  containerRectLazy: CanvasPoint | (() => CanvasPoint),
  elementCanvasRectangleCache: ElementCanvasRectangleCache,
): {
  tagName: string
  globalFrame: CanvasRectangle
  nonRoundedGlobalFrame: CanvasRectangle
  specialSizeMeasurementsObject: SpecialSizeMeasurements
  textContentsMaybe: string | null
} {
  const tagName: string = element.tagName.toLowerCase()
  const globalFrame = globalFrameForElement(
    element,
    scale,
    containerRectLazy,
    'without-text-content',
    'nearest-half',
    elementCanvasRectangleCache,
  )
  const nonRoundedGlobalFrame = globalFrameForElement(
    element,
    scale,
    containerRectLazy,
    'without-text-content',
    'no-rounding',
    elementCanvasRectangleCache,
  )

  const textContentsMaybe = element.children.length === 0 ? element.textContent : null

  const specialSizeMeasurementsObject = getSpecialMeasurements(
    element,
    closestOffsetParentPath,
    scale,
    containerRectLazy,
    elementCanvasRectangleCache,
  )

  return {
    tagName: tagName,
    globalFrame: globalFrame,
    nonRoundedGlobalFrame: nonRoundedGlobalFrame,
    specialSizeMeasurementsObject: specialSizeMeasurementsObject,
    textContentsMaybe: textContentsMaybe,
  }
}

export function collectDomElementMetadataForElement(
  element: HTMLElement,
  scale: number,
  containerRectX: number,
  containerRectY: number,
  elementCanvasRectangleCache: ElementCanvasRectangleCache,
): DomElementMetadata {
  const closestOffsetParentPath: ElementPath | null = findNearestElementWithPath(
    element.offsetParent,
  )

  const {
    tagName,
    globalFrame,
    nonRoundedGlobalFrame,
    specialSizeMeasurementsObject,
    textContentsMaybe,
  } = collectMetadataForElement(
    element,
    closestOffsetParentPath,
    scale,
    canvasPoint({ x: containerRectX, y: containerRectY }),
    elementCanvasRectangleCache,
  )

  return domElementMetadata(
    left(tagName),
    globalFrame,
    nonRoundedGlobalFrame,
    specialSizeMeasurementsObject,
    textContentsMaybe,
  )
}

export function getGridContainerProperties(
  elementStyle: CSSStyleDeclaration | null,
  options?: {
    dynamicCols: boolean
    dynamicRows: boolean
  },
): GridContainerProperties {
  if (elementStyle == null) {
    return {
      gridTemplateColumns: null,
      gridTemplateRows: null,
      gridAutoColumns: null,
      gridAutoRows: null,
      gridAutoFlow: null,
    }
  }

  const gridTemplateColumns = trimDynamicEmptyDimensions(
    defaultEither(
      gridAutoOrTemplateFallback(elementStyle.gridTemplateColumns),
      parseGridAutoOrTemplateBase(elementStyle.gridTemplateColumns),
    ),
    options?.dynamicCols === true,
  )

  const gridTemplateRows = trimDynamicEmptyDimensions(
    defaultEither(
      gridAutoOrTemplateFallback(elementStyle.gridTemplateRows),
      parseGridAutoOrTemplateBase(elementStyle.gridTemplateRows),
    ),
    options?.dynamicRows === true,
  )

  const gridAutoColumns = defaultEither(
    gridAutoOrTemplateFallback(elementStyle.gridAutoColumns),
    parseGridAutoOrTemplateBase(elementStyle.gridAutoColumns),
  )
  const gridAutoRows = defaultEither(
    gridAutoOrTemplateFallback(elementStyle.gridAutoRows),
    parseGridAutoOrTemplateBase(elementStyle.gridAutoRows),
  )
  return gridContainerProperties(
    gridTemplateColumns,
    gridTemplateRows,
    gridAutoColumns,
    gridAutoRows,
    parseGridAutoFlow(elementStyle.gridAutoFlow),
  )
}

function trimDynamicEmptyDimensions(
  template: GridAutoOrTemplateBase,
  isDynamic: boolean,
): GridAutoOrTemplateBase {
  if (!isDynamic) {
    return template
  }
  if (template.type !== 'DIMENSIONS') {
    return template
  }

  const lastNonEmptyColumn = template.dimensions.findLastIndex(
    (d) => d.type === 'KEYWORD' || (d.type === 'NUMBER' && d.value.value !== 0),
  )
  return gridAutoOrTemplateDimensions(template.dimensions.slice(0, lastNonEmptyColumn + 1))
}

function getGridElementProperties(
  container: GridContainerProperties,
  elementStyle: CSSStyleDeclaration,
): GridElementProperties {
  function getPlacementPin(
    value: GridPositionOrSpan | null,
    axis: 'row' | 'column',
    pin: 'start' | 'end',
    style: string,
  ) {
    if (isGridSpan(value) || value != null) {
      return value
    }
    return defaultEither(null, parseGridPosition(container, axis, pin, value ?? null, style))
  }

  const gridColumn = defaultEither(
    null,
    parseGridRange(container, 'column', elementStyle.gridColumn),
  )
  const gridColumnStart = getPlacementPin(
    gridColumn?.start ?? null,
    'column',
    'start',
    elementStyle.gridColumnStart,
  )
  const gridColumnEnd = getPlacementPin(
    gridColumn?.end ?? null,
    'column',
    'end',
    elementStyle.gridColumnEnd,
  )
  const adjustedColumnEnd =
    isGridSpan(gridColumn?.end) || (isCSSKeyword(gridColumnEnd) && gridColumn?.end != null)
      ? gridColumn.end
      : gridColumnEnd

  const gridRow = defaultEither(null, parseGridRange(container, 'row', elementStyle.gridRow))
  const gridRowStart = getPlacementPin(
    gridRow?.start ?? null,
    'row',
    'start',
    elementStyle.gridRowStart,
  )
  const gridRowEnd = getPlacementPin(gridRow?.end ?? null, 'row', 'end', elementStyle.gridRowEnd)
  const adjustedRowEnd =
    isGridSpan(gridRow?.end) || (isCSSKeyword(gridRowEnd) && gridRow?.end != null)
      ? gridRow.end
      : gridRowEnd

  const result = gridElementProperties(
    gridColumnStart,
    adjustedColumnEnd,
    gridRowStart,
    adjustedRowEnd,
  )
  return result
}

function getSpecialMeasurements(
  element: HTMLElement,
  closestOffsetParentPath: ElementPath | null,
  scale: number,
  containerRectLazy: CanvasPoint | (() => CanvasPoint),
  elementCanvasRectangleCache: ElementCanvasRectangleCache,
): SpecialSizeMeasurements {
  const computedStyle = window.getComputedStyle(element)
  const layoutSystemForChildren = elementLayoutSystem(computedStyle)
  const position = getPosition(computedStyle)

  const offset = {
    x: roundToNearestHalf(element.offsetLeft),
    y: roundToNearestHalf(element.offsetTop),
  } as LocalPoint

  const coordinateSystemBounds =
    element.offsetParent instanceof HTMLElement
      ? globalFrameForElement(
          element.offsetParent,
          scale,
          containerRectLazy,
          'without-text-content',
          'nearest-half',
          elementCanvasRectangleCache,
        )
      : null

  const immediateParentBounds =
    element.parentElement instanceof HTMLElement
      ? globalFrameForElement(
          element.parentElement,
          scale,
          containerRectLazy,
          'without-text-content',
          'nearest-half',
          elementCanvasRectangleCache,
        )
      : null

  const parentElementStyle =
    element.parentElement == null ? null : window.getComputedStyle(element.parentElement)
  const isParentNonStatic = isElementNonStatic(parentElementStyle)

  const providesBoundsForAbsoluteChildren = isElementAContainingBlockForAbsolute(computedStyle)

  const parentLayoutSystem = elementLayoutSystem(parentElementStyle)
  const parentProvidesLayout = element.parentElement === element.offsetParent
  const parentFlexDirection = eitherToMaybe(
    parseFlexDirection(parentElementStyle?.flexDirection, null),
  )
  const parentJustifyContent = getFlexJustifyContent(parentElementStyle?.justifyContent ?? null)

  const sizeMainAxis = parentFlexDirection === 'row' ? 'width' : 'height'
  const parentHugsOnMainAxis =
    parentLayoutSystem === 'flex' &&
    element.parentElement != null &&
    element.parentElement.style[sizeMainAxis] === MaxContent

  const flexDirection = eitherToMaybe(parseFlexDirection(computedStyle.flexDirection, null))
  const parentTextDirection = eitherToMaybe(parseDirection(parentElementStyle?.direction, null))

  const justifyContent = getFlexJustifyContent(computedStyle.justifyContent)
  const alignContent = getAlignContent(computedStyle.alignContent)
  const alignItems = getFlexAlignment(computedStyle.alignItems)
  const alignSelf = getSelfAlignment(computedStyle.alignSelf)
  const justifySelf = getSelfAlignment(computedStyle.justifySelf)

  const margin = applicative4Either(
    applicativeSidesPxTransform,
    parseCSSLength(computedStyle.marginTop),
    parseCSSLength(computedStyle.marginRight),
    parseCSSLength(computedStyle.marginBottom),
    parseCSSLength(computedStyle.marginLeft),
  )

  const padding = applicative4Either(
    applicativeSidesPxTransform,
    parseCSSLength(computedStyle.paddingTop),
    parseCSSLength(computedStyle.paddingRight),
    parseCSSLength(computedStyle.paddingBottom),
    parseCSSLength(computedStyle.paddingLeft),
  )

  const parentPadding = applicative4Either(
    applicativeSidesPxTransform,
    parseCSSLength(parentElementStyle?.paddingTop),
    parseCSSLength(parentElementStyle?.paddingRight),
    parseCSSLength(parentElementStyle?.paddingBottom),
    parseCSSLength(parentElementStyle?.paddingLeft),
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

  const borderTopWidth = parseCSSLength(computedStyle.borderTopWidth)
  const borderRightWidth = parseCSSLength(computedStyle.borderRightWidth)
  const borderBottomWidth = parseCSSLength(computedStyle.borderBottomWidth)
  const borderLeftWidth = parseCSSLength(computedStyle.borderLeftWidth)
  const border: BorderWidths = {
    top: isRight(borderTopWidth) ? borderTopWidth.value.value : 0,
    right: isRight(borderRightWidth) ? borderRightWidth.value.value : 0,
    bottom: isRight(borderBottomWidth) ? borderBottomWidth.value.value : 0,
    left: isRight(borderLeftWidth) ? borderLeftWidth.value.value : 0,
  }

  const offsetParent = getClosestOffsetParent(element) as HTMLElement | null
  const elementOrContainingParent =
    providesBoundsForAbsoluteChildren || offsetParent == null ? element : offsetParent

  const globalFrame = globalFrameForElement(
    elementOrContainingParent,
    scale,
    containerRectLazy,
    'without-text-content',
    'nearest-half',
    elementCanvasRectangleCache,
  )

  const globalFrameWithTextContent = globalFrameForElement(
    element,
    scale,
    containerRectLazy,
    'with-text-content',
    'nearest-half',
    elementCanvasRectangleCache,
  )

  const globalContentBoxForChildren = canvasRectangle({
    x: globalFrame.x + border.left,
    y: globalFrame.y + border.top,
    width: globalFrame.width - border.left - border.right,
    height: globalFrame.height - border.top - border.bottom,
  })

  function positionValueIsDefault(value: string) {
    return value === 'auto' || value === '0px'
  }

  const hasPositionOffset =
    !positionValueIsDefault(computedStyle.top) ||
    !positionValueIsDefault(computedStyle.right) ||
    !positionValueIsDefault(computedStyle.bottom) ||
    !positionValueIsDefault(computedStyle.left)
  const hasTransform = computedStyle.transform !== 'none'

  const gap = defaultEither(
    null,
    mapEither((n) => n.value, parseCSSLength(computedStyle.gap)),
  )

  const rowGap = defaultEither(
    null,
    mapEither((n) => n.value, parseCSSLength(computedStyle.rowGap)),
  )

  const columnGap = defaultEither(
    null,
    mapEither((n) => n.value, parseCSSLength(computedStyle.columnGap)),
  )

  const flexGapValue = parseCSSLength(parentElementStyle?.gap)
  const parsedFlexGapValue = isRight(flexGapValue) ? flexGapValue.value.value : 0

  const borderRadius = defaultEither(
    null,
    applicative4Either(
      applicativeSidesPxTransform,
      parseCSSLength(computedStyle.borderTopLeftRadius),
      parseCSSLength(computedStyle.borderTopRightRadius),
      parseCSSLength(computedStyle.borderBottomLeftRadius),
      parseCSSLength(computedStyle.borderBottomRightRadius),
    ),
  )

  const fontSize = computedStyle.fontSize
  const fontWeight = computedStyle.fontWeight
  const fontStyle = computedStyle.fontStyle
  const textDecorationLine = computedStyle.textDecorationLine

  const textBounds = elementContainsOnlyText(element)
    ? stretchRect(
        getCanvasRectangleFromElement(
          element,
          scale,
          'only-text-content',
          'nearest-half',
          elementCanvasRectangleCache,
        ),
        {
          w:
            maybeValueFromComputedStyle(computedStyle.paddingLeft) +
            maybeValueFromComputedStyle(computedStyle.paddingRight) +
            maybeValueFromComputedStyle(computedStyle.marginLeft) +
            maybeValueFromComputedStyle(computedStyle.marginRight),
          h:
            maybeValueFromComputedStyle(computedStyle.paddingTop) +
            maybeValueFromComputedStyle(computedStyle.paddingBottom) +
            maybeValueFromComputedStyle(computedStyle.marginTop) +
            maybeValueFromComputedStyle(computedStyle.marginBottom),
        },
      )
    : null

  const computedStyleMap =
    typeof element.computedStyleMap === 'function' ? element.computedStyleMap() : null // TODO: this is for jest tests, no computedStyleMap in jsdom :()
  const computedHugProperty = hugPropertiesFromStyleMap(
    (styleProp: string) => computedStyleMap?.get(styleProp)?.toString() ?? null,
    globalFrame,
  )

  const paddingValue = isRight(padding)
    ? padding.value
    : sides(undefined, undefined, undefined, undefined)

  const gridCellGlobalFrames =
    layoutSystemForChildren === 'grid'
      ? measureGlobalFramesOfGridCells(
          element,
          scale,
          containerRectLazy,
          elementCanvasRectangleCache,
        )
      : null

  const parentGridCellGlobalFrames =
    element.parentElement != null && elementLayoutSystem(parentElementStyle) === 'grid'
      ? measureGlobalFramesOfGridCells(
          element.parentElement,
          scale,
          containerRectLazy,
          elementCanvasRectangleCache,
        )
      : null

  const containerGridPropertiesFromProps = getGridContainerProperties(element.style)
  const parentContainerGridPropertiesFromProps = getGridContainerProperties(parentElementStyle)
  const containerGridProperties = getGridContainerProperties(computedStyle, {
    dynamicCols: isDynamicGridTemplate(containerGridPropertiesFromProps.gridTemplateColumns),
    dynamicRows: isDynamicGridTemplate(containerGridPropertiesFromProps.gridTemplateRows),
  })

  const parentContainerGridProperties = getGridContainerProperties(parentElementStyle)
  const containerElementPropertiesFromProps = getGridElementProperties(
    parentContainerGridProperties,
    element.style,
  )
  const containerElementProperties = getGridElementProperties(
    parentContainerGridProperties,
    computedStyle,
  )

  return specialSizeMeasurements(
    offset,
    coordinateSystemBounds,
    immediateParentBounds,
    globalFrameWithTextContent,
    parentProvidesLayout,
    closestOffsetParentPath,
    isParentNonStatic,
    parentLayoutSystem,
    layoutSystemForChildren,
    false, // layoutSystemForChildrenInherited
    providesBoundsForAbsoluteChildren,
    computedStyle.display,
    position,
    isRight(margin) ? margin.value : sides(undefined, undefined, undefined, undefined),
    paddingValue,
    naturalWidth,
    naturalHeight,
    clientWidth,
    clientHeight,
    parentFlexDirection,
    parentJustifyContent,
    parsedFlexGapValue,
    isRight(parentPadding)
      ? parentPadding.value
      : sides(undefined, undefined, undefined, undefined),
    parentHugsOnMainAxis,
    gap,
    flexDirection,
    justifyContent,
    alignContent,
    alignItems,
    element.localName,
    childrenCount,
    globalContentBoxForChildren,
    computedStyle.float,
    hasPositionOffset,
    parentTextDirection,
    hasTransform,
    borderRadius,
    fontSize,
    fontWeight,
    fontStyle,
    textDecorationLine,
    textBounds,
    computedHugProperty,
    containerGridProperties,
    parentContainerGridProperties,
    containerElementProperties,
    containerGridPropertiesFromProps,
    parentContainerGridPropertiesFromProps,
    containerElementPropertiesFromProps,
    rowGap,
    columnGap,
    gridCellGlobalFrames,
    parentGridCellGlobalFrames,
    justifySelf,
    alignSelf,
    border,
  )
}

export function isDynamicGridTemplate(template: GridAutoOrTemplateBase | null) {
  return template?.type === 'DIMENSIONS' && template.dimensions.some((d) => isDynamicGridRepeat(d))
}

function elementContainsOnlyText(element: HTMLElement): boolean {
  if (element.childNodes.length === 0) {
    return false
  }
  for (const node of element.childNodes) {
    const isForText =
      node.nodeType === Node.TEXT_NODE ||
      (node.nodeType === Node.ELEMENT_NODE && node.nodeName === 'BR')
    if (!isForText) {
      return false
    }
  }
  return true
}

function maybeValueFromComputedStyle(property: string): number {
  const parsed = parseCSSPx(property)
  return isRight(parsed) ? parsed.value.value : 0
}

function globalFrameForElement(
  element: HTMLElement,
  scale: number,
  containerRectLazy: CanvasPoint | (() => CanvasPoint),
  withContent: 'without-text-content' | 'with-text-content',
  rounding: 'nearest-half' | 'no-rounding',
  elementCanvasRectangleCache: ElementCanvasRectangleCache,
) {
  const elementRect = getCanvasRectangleFromElement(
    element,
    scale,
    withContent,
    rounding,
    elementCanvasRectangleCache,
  )

  return Utils.offsetRect(
    elementRect,
    Utils.negate(typeof containerRectLazy === 'function' ? containerRectLazy() : containerRectLazy),
  )
}

function getClosestOffsetParent(element: HTMLElement): Element | null {
  let currentElement: HTMLElement | null = element

  while (currentElement != null) {
    if (currentElement.offsetParent != null) {
      return currentElement.offsetParent
    }
    currentElement = currentElement.parentElement
  }
  return null
}

function measureGlobalFramesOfGridCells(
  element: HTMLElement,
  scale: number,
  containerRectLazy: CanvasPoint | (() => CanvasPoint),
  elementCanvasRectangleCache: ElementCanvasRectangleCache,
): GridCellGlobalFrames | null {
  const paths = getPathsOnDomElement(element)

  let gridCellGlobalFrames: GridCellGlobalFrames | null = null
  const gridControlElement = (() => {
    for (let p of paths) {
      const maybeGridControlElement = document.getElementById(GridMeasurementHelperKey(p))
      if (maybeGridControlElement != null) {
        return maybeGridControlElement
      }
    }
    return null
  })()

  if (gridControlElement != null) {
    gridCellGlobalFrames = []
    for (const cell of gridControlElement.children) {
      if (!(cell instanceof HTMLElement)) {
        continue
      }
      const rowIndexAttr = cell.getAttribute('data-grid-row')
      const columnIndexAttr = cell.getAttribute('data-grid-column')
      if (rowIndexAttr == null || columnIndexAttr == null) {
        continue
      }
      const rowIndex = parseInt(rowIndexAttr)
      const columnIndex = parseInt(columnIndexAttr)
      if (!isFinite(rowIndex) || !isFinite(columnIndex)) {
        continue
      }
      const row = gridCellGlobalFrames[rowIndex - 1]
      if (row == null) {
        gridCellGlobalFrames[rowIndex - 1] = []
      }
      gridCellGlobalFrames[rowIndex - 1][columnIndex - 1] = globalFrameForElement(
        cell,
        scale,
        containerRectLazy,
        'without-text-content',
        'nearest-half',
        elementCanvasRectangleCache,
      )
    }
  }
  return gridCellGlobalFrames
}
