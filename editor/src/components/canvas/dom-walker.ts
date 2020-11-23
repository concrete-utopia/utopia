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
} from '../../core/shared/element-template'
import { id, TemplatePath, InstancePath } from '../../core/shared/project-file-types'
import { getCanvasRectangleFromElement, getDOMAttribute } from '../../core/shared/dom-utils'
import { applicative4Either, isRight, left } from '../../core/shared/either'
import Utils from '../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  LocalPoint,
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
  UTOPIA_UID_KEY,
  UTOPIA_UID_ORIGINAL_PARENTS_KEY,
  UTOPIA_UID_PARENTS_KEY,
} from '../../core/model/utopia-constants'
import ResizeObserver from 'resize-observer-polyfill'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { PRODUCTION_ENV } from '../../common/env-vars'
import { CanvasContainerID } from './canvas-types'
import { emptySet } from '../../core/shared/set-utils'
import { MapLike } from 'typescript'
import { forEachValue } from '../../core/shared/object-utils'
import { mapArrayToDictionary } from '../../core/shared/array-utils'
import { fastForEach } from '../../core/shared/utils'

const MutationObserverConfig = { attributes: true, childList: true, subtree: true }
const ObserversAvailable = (window as any).MutationObserver != null && ResizeObserver != null

function isValidPath(path: TemplatePath | null, validPaths: Array<string>): boolean {
  return path != null && validPaths.indexOf(TP.toString(path)) > -1
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
): void {
  return useSelectorWithCallback(
    (store) => store.editor.selectedViews,
    (newSelectedViews) => {
      newSelectedViews.forEach((sv) => {
        const scenePath = TP.scenePathForPath(sv)
        const sceneID = TP.toString(scenePath)
        invalidatedSceneIDsRef.current.add(sceneID)
      })
    },
  )
}

const domWalkerComputedStyleProps: { [key: string]: string } = mapArrayToDictionary<
  string,
  string,
  string
>(
  [
    'alignContent',
    'alignItems',
    'alignSelf',
    'bottom',
    'display',
    'flexDirection',
    'flexGrow',
    'flexShrink',
    'flexWrap',
    'Height',
    'justifyContent',
    'left',
    'marginBottom',
    'marginLeft',
    'marginRight',
    'marginTop',
    'maxHeight',
    'maxWidth',
    'minHeight',
    'minWidth',
    'paddingBottom',
    'paddingLeft',
    'paddingRight',
    'paddingTop',
    'position',
    'right',
    'top',
    'Width',
  ],
  (key) => key,
  (key) => camelCaseToDashed(key),
)

export function useDomWalker(props: CanvasContainerProps): React.Ref<HTMLDivElement> {
  const containerRef = React.useRef<HTMLDivElement>(null)
  const rootMetadataInStateRef = useRefEditorState((store) => store.editor.domMetadataKILLME)
  const invalidatedSceneIDsRef = React.useRef<Set<string>>(emptySet())
  const initCompleteRef = React.useRef<boolean>(false)
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'useDomWalker selectedViews',
  )
  const resizeObserver = useResizeObserver(invalidatedSceneIDsRef)
  const mutationObserver = useMutationObserver(invalidatedSceneIDsRef)
  useInvalidateScenesWhenSelectedViewChanges(invalidatedSceneIDsRef)

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
      const containerRect = getCanvasRectangleFromElement(refOfContainer, props.scale)

      let globalFrameCache: Map<HTMLElement, CanvasRectangle> = new Map()
      function globalFrameForElement(element: HTMLElement): CanvasRectangle {
        const fromCache = globalFrameCache.get(element)
        if (fromCache == null) {
          // Get the local frame from the DOM and calculate the global frame.
          const elementRect = getCanvasRectangleFromElement(element, props.scale)
          const result = Utils.offsetRect(elementRect, Utils.negate(containerRect))
          globalFrameCache.set(element, result)
          return result
        } else {
          return fromCache
        }
      }

      function getSpecialMeasurements(
        element: HTMLElement,
        computedStyle: CSSStyleDeclaration,
        parentComputedStyle: CSSStyleDeclaration | null,
      ): SpecialSizeMeasurements {
        const layoutSystemForChildren = elementLayoutSystem(computedStyle)
        const position = getPosition(computedStyle)

        const offset = {
          x: roundToNearestHalf(element.offsetLeft),
          y: roundToNearestHalf(element.offsetTop),
        } as LocalPoint

        const coordinateSystemBounds =
          element.offsetParent instanceof HTMLElement
            ? globalFrameForElement(element.offsetParent)
            : null

        const immediateParentBounds =
          element.parentElement instanceof HTMLElement
            ? globalFrameForElement(element.parentElement)
            : null

        //const parentComputedStyle =
        //  element.parentElement == null ? null : window.getComputedStyle(element.parentElement)
        const isParentNonStatic = isElementNonStatic(parentComputedStyle)

        const providesBoundsForChildren = isElementNonStatic(computedStyle)

        const parentLayoutSystem = elementLayoutSystem(parentComputedStyle)
        const parentProvidesLayout = element.parentElement === element.offsetParent
        const parentFlexDirection = parentComputedStyle?.flexDirection ?? null

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
          position,
          isRight(margin) ? margin.value : sides(undefined, undefined, undefined, undefined),
          isRight(padding) ? padding.value : sides(undefined, undefined, undefined, undefined),
          naturalWidth,
          naturalHeight,
          clientWidth,
          clientHeight,
          parentFlexDirection,
        )
      }

      function getComputedStyle(
        element: HTMLElement,
        path: TemplatePath,
        domComputedStyle: CSSStyleDeclaration,
      ): ComputedStyle | null {
        const isSelected = selectedViews.some((sv) => TP.pathsEqual(sv, path))
        if (!isSelected) {
          // the element is not among the selected views, skip computing the style
          return null
        }
        let computedStyle: ComputedStyle = {}
        if (domComputedStyle != null) {
          fastForEach(Object.keys(domWalkerComputedStyleProps), (computedStyleProp) => {
            const styleProp = domWalkerComputedStyleProps[computedStyleProp]
            // Accessing the value directly often doesn't work, and using `getPropertyValue` requires
            // using dashed case rather than camel case
            const propertyValue = domComputedStyle.getPropertyValue(styleProp)
            if (propertyValue != '') {
              computedStyle[computedStyleProp] = propertyValue
            }
          })
        }

        return computedStyle
      }

      function walkScene(scene: HTMLElement, index: number): void {
        if (scene instanceof HTMLElement) {
          // Right now this assumes that only UtopiaJSXComponents can be rendered via scenes,
          // and that they can only have a single root element
          const sceneIndexAttr = scene.attributes.getNamedItemNS(null, 'data-utopia-scene-id')
          const validPathsAttr = scene.attributes.getNamedItemNS(null, 'data-utopia-valid-paths')

          if (sceneIndexAttr != null && validPathsAttr != null) {
            const scenePath = TP.fromString(sceneIndexAttr.value)
            const validPaths = validPathsAttr.value.split(' ')
            const sceneID = sceneIndexAttr.value
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
                invalidatedSceneIDsRef.current.delete(sceneID)
              }
            }

            if (cachedMetadata == null) {
              const parentComputedStyle =
                scene.parentElement == null ? null : window.getComputedStyle(scene.parentElement)
              const rootElements = walkSceneInner(
                scene,
                index,
                scenePath,
                validPaths,
                parentComputedStyle,
              )

              const computedStyle = window.getComputedStyle(scene)
              const sceneMetadata = collectMetadata(
                scene,
                TP.instancePath([], TP.elementPathForPath(scenePath)),
                null,
                rootElements,
                computedStyle,
                parentComputedStyle,
              )
              rootMetadata.push(sceneMetadata)
            } else {
              rootMetadata.push(cachedMetadata)
              // Push the cached metadata for everything from this scene downwards
              Utils.fastForEach(rootMetadataInStateRef.current, (elem) => {
                if (TP.isAncestorOf(elem.templatePath, scenePath)) {
                  rootMetadata.push(elem)
                }
              })
            }
          }
        }
      }

      function walkCanvasRootFragment(
        canvasRoot: HTMLElement,
        index: number,
        canvasRootPath: TemplatePath,
        validPaths: Array<string>,
      ) {
        if (
          ObserversAvailable &&
          invalidatedSceneIDsRef.current.size === 0 &&
          rootMetadataInStateRef.current.length > 0 &&
          initCompleteRef.current === true
        ) {
          // no mutation happened on the entire canvas, just return the old metadata
          rootMetadata = rootMetadataInStateRef.current
        } else {
          const rootElements = walkSceneInner(canvasRoot, index, canvasRootPath, validPaths, null)
          // The Storyboard root being a fragment means it is invisible to us in the DOM walker,
          // so walkCanvasRootFragment will create a fake root ElementInstanceMetadata
          // to provide a home for the the (really existing) childMetadata
          const metadata: ElementInstanceMetadata = elementInstanceMetadata(
            canvasRootPath as InstancePath,
            left('Storyboard'),
            {},
            null,
            null,
            rootElements,
            false,
            emptySpecialSizeMeasurements,
            emptyComputedStyle,
          )
          rootMetadata.push(metadata)
        }
      }

      function walkSceneInner(
        scene: HTMLElement,
        index: number,
        scenePath: TemplatePath,
        validPaths: Array<string>,
        parentComputedStyle: CSSStyleDeclaration | null,
      ): Array<InstancePath> {
        const globalFrame: CanvasRectangle = globalFrameForElement(scene)

        let childPaths: Array<InstancePath> = []

        scene.childNodes.forEach((childNode) => {
          const childNodePaths = walkElements(
            childNode,
            index,
            0,
            globalFrame,
            scenePath,
            scenePath,
            validPaths,
            parentComputedStyle,
          )

          childPaths.push(...childNodePaths)
        })

        return childPaths
      }

      // Walks through the DOM producing the structure and values from within.
      function walkElements(
        element: Node,
        index: number,
        depth: number,
        parentPoint: CanvasPoint,
        originalParentPath: TemplatePath | null,
        uniqueParentPath: TemplatePath,
        validPaths: Array<string>,
        parentComputedStyle: CSSStyleDeclaration | null,
      ): Array<InstancePath> {
        if (isScene(element)) {
          // we found a nested scene, restart the walk
          walkScene(element, index)
          return []
        }
        if (element instanceof HTMLElement) {
          // Determine the uid of this element if it has one.
          const uidAttribute = getDOMAttribute(element, UTOPIA_UID_KEY)
          const parentUIDsAttribute = getDOMAttribute(element, UTOPIA_UID_PARENTS_KEY)
          const originalUIDAttribute = getDOMAttribute(element, UTOPIA_ORIGINAL_ID_KEY)
          const originalParentUIDsAttribute = getDOMAttribute(
            element,
            UTOPIA_UID_ORIGINAL_PARENTS_KEY,
          )
          const doNotTraverseAttribute = getDOMAttribute(element, UTOPIA_DO_NOT_TRAVERSE_KEY)

          const traverseChildren: boolean = doNotTraverseAttribute !== 'true'

          // Build the path for this element, substituting an index in if there is no uid attribute.
          function makeIndexElement(): id {
            return `index-${index}`
          }
          const pathElement = Utils.defaultIfNullLazy<id>(uidAttribute, makeIndexElement)
          const originalPathElement = Utils.defaultIfNull(uidAttribute, originalUIDAttribute)
          const parentAttribute = Utils.defaultIfNull(
            parentUIDsAttribute,
            originalParentUIDsAttribute,
          )

          // Build the unique path for this element.
          let uniquePath: TemplatePath = uniqueParentPath
          if (parentUIDsAttribute != null) {
            uniquePath = TP.appendToPath(uniquePath, parentUIDsAttribute.split('/'))
          }
          uniquePath = TP.appendToPath(uniquePath, pathElement)

          const globalFrame = globalFrameForElement(element)

          // Build the original path for this element.
          let originalPath: TemplatePath | null = originalParentPath
          if (originalPath != null) {
            if (parentAttribute != null) {
              originalPath = TP.appendToPath(originalPath, parentAttribute.split('/'))
            }
            if (originalPathElement != null) {
              originalPath = TP.appendToPath(originalPath, originalPathElement)
            }
          }

          // Check this is a path we're interested in, otherwise skip straight to the children
          const pathIsValid = isValidPath(Utils.defaultIfNull(uniquePath, originalPath), validPaths)
          const pathForChildren = pathIsValid ? uniquePath : uniqueParentPath

          // Get this upfront here at the uppermost point reasonable.
          const computedStyle = window.getComputedStyle(element)

          // Build the metadata for the children of this DOM node.
          let childPaths: Array<InstancePath> = []
          if (traverseChildren) {
            element.childNodes.forEach((child, childIndex) => {
              const childNodePaths = walkElements(
                child,
                childIndex,
                depth + 1,
                globalFrame,
                originalPath,
                pathForChildren,
                validPaths,
                computedStyle,
              )
              childPaths.push(...childNodePaths)
            })
          }

          if (pathIsValid) {
            const collectedMetadata = collectMetadata(
              element,
              uniquePath,
              parentPoint,
              childPaths,
              computedStyle,
              parentComputedStyle,
            )
            rootMetadata.push(collectedMetadata)
            return [collectedMetadata.templatePath]
          } else {
            return childPaths
          }
        } else {
          return []
        }
      }

      function collectMetadata(
        element: HTMLElement,
        instancePath: InstancePath,
        parentPoint: CanvasPoint | null,
        children: InstancePath[],
        computedStyle: CSSStyleDeclaration,
        parentComputedStyle: CSSStyleDeclaration | null,
      ): ElementInstanceMetadata {
        const globalFrame = globalFrameForElement(element)
        const localFrame =
          parentPoint != null
            ? localRectangle(Utils.offsetRect(globalFrame, Utils.negate(parentPoint)))
            : null

        const uidAttribute = getDOMAttribute(element, UTOPIA_UID_KEY)
        const originalUIDAttribute = getDOMAttribute(element, UTOPIA_ORIGINAL_ID_KEY)
        const labelAttribute = getDOMAttribute(element, UTOPIA_LABEL_KEY)
        let elementProps: any = {}
        if (uidAttribute != null) {
          elementProps[UTOPIA_UID_KEY] = uidAttribute
        }
        if (originalUIDAttribute != null) {
          elementProps[UTOPIA_ORIGINAL_ID_KEY] = originalUIDAttribute
        }
        if (labelAttribute != null) {
          elementProps[UTOPIA_LABEL_KEY] = labelAttribute
        }
        const metadataComputedStyle = getComputedStyle(element, instancePath, computedStyle)
        return elementInstanceMetadata(
          instancePath,
          left(element.tagName.toLowerCase()),
          elementProps,
          globalFrame,
          localFrame,
          children,
          false,
          getSpecialMeasurements(element, computedStyle, parentComputedStyle),
          metadataComputedStyle,
        )
      }

      let rootMetadata: Array<ElementInstanceMetadata> = []
      // This assumes that the canvas root is rendering a Storyboard fragment.
      // The necessary validPaths and the root fragment's template path comes from props,
      // because the fragment is invisible in the DOM.
      walkCanvasRootFragment(
        refOfContainer,
        0,
        props.canvasRootElementTemplatePath,
        props.validRootPaths.map(TP.toString),
      )
      if (LogDomWalkerPerformance) {
        performance.mark('DOM_WALKER_END')
        performance.measure('DOM WALKER', 'DOM_WALKER_START', 'DOM_WALKER_END')
      }
      initCompleteRef.current = true
      props.onDomReport(rootMetadata)
    }
  })

  return containerRef
}
