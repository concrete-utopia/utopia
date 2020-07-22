import * as React from 'react'
import { sides } from 'utopia-api'
import * as TP from '../../core/shared/template-path'
import { UTOPIA_ORIGINAL_ID_KEY } from '../../core/model/element-metadata-utils'
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
import { getCanvasRectangleFromElement } from '../../core/shared/dom-utils'
import { applicative4Either, isRight, left } from '../../core/shared/either'
import Utils from '../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  LocalPoint,
  localRectangle,
} from '../../core/shared/math-utils'
import {
  CSSNumber,
  parseCSSLength,
  CSSPosition,
  positionValues,
} from '../inspector/common/css-utils'
import { CanvasContainerProps } from './ui-jsx-canvas'
import { camelCaseToDashed } from '../../core/shared/string-utils'

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
  if (isElementNonFixed(computedStyle)) {
    return 'nonfixed'
  }
  return 'flow' // or fixed?
}

function getPosition(computedStyle: CSSStyleDeclaration | null): CSSPosition | null {
  const valueAsAny = computedStyle?.position as any
  return positionValues.includes(valueAsAny) ? valueAsAny : null
}

function isElementNonFixed(computedStyle: CSSStyleDeclaration | null) {
  if (computedStyle == null) {
    return false
  }
  if (computedStyle.position != null && computedStyle.position !== 'fixed') {
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

export function useDomWalker(props: CanvasContainerProps): React.Ref<HTMLDivElement> {
  const containerRef = React.useRef<HTMLDivElement>(null)

  React.useLayoutEffect(() => {
    if (containerRef.current != null) {
      // Get some base values relating to the div this component creates.
      const refOfContainer = containerRef.current

      const containerRect = getCanvasRectangleFromElement(refOfContainer, props.scale)

      function globalFrameForElement(element: HTMLElement): CanvasRectangle {
        // Get the local frame from the DOM and calculate the global frame.
        const elementRect = getCanvasRectangleFromElement(element, props.scale)
        return Utils.offsetRect(elementRect, Utils.negate(containerRect))
      }

      function getSpecialMeasurements(element: HTMLElement): SpecialSizeMeasurements {
        const elementStyle = window.getComputedStyle(element)
        const layoutSystemForChildren = elementLayoutSystem(elementStyle)
        const position = getPosition(elementStyle)

        const offset = {
          x: element.offsetLeft,
          y: element.offsetTop,
        } as LocalPoint

        const coordinateSystemBounds =
          element.offsetParent instanceof HTMLElement
            ? globalFrameForElement(element.offsetParent)
            : null

        const immediateParentBounds =
          element.parentElement instanceof HTMLElement
            ? globalFrameForElement(element.parentElement)
            : null

        const parentElementStyle =
          element.parentElement == null ? null : window.getComputedStyle(element.parentElement)
        const isParentNonFixed = isElementNonFixed(parentElementStyle)

        const parentLayoutSystem = elementLayoutSystem(parentElementStyle)
        const parentProvidesLayout = element.parentElement === element.offsetParent

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
          naturalWidth = (element as HTMLImageElement).naturalWidth
          naturalHeight = (element as HTMLImageElement).naturalHeight
        }

        let clientWidth = element.clientWidth
        let clientHeight = element.clientHeight

        return specialSizeMeasurements(
          offset,
          coordinateSystemBounds,
          immediateParentBounds,
          parentProvidesLayout,
          isParentNonFixed,
          parentLayoutSystem,
          layoutSystemForChildren,
          position,
          isRight(margin) ? margin.value : sides(undefined, undefined, undefined, undefined),
          isRight(padding) ? padding.value : sides(undefined, undefined, undefined, undefined),
          naturalWidth,
          naturalHeight,
          clientWidth,
          clientHeight,
        )
      }

      function getComputedStyle(element: HTMLElement): ComputedStyle {
        const elementStyle = window.getComputedStyle(element)
        let computedStyle: ComputedStyle = {}
        if (elementStyle != null) {
          Object.keys(elementStyle).forEach((key) => {
            // Accessing the value directly often doesn't work, and using `getPropertyValue` requires
            // using dashed case rather than camel case
            const caseCorrectedKey = camelCaseToDashed(key)
            const propertyValue = elementStyle.getPropertyValue(caseCorrectedKey)
            if (propertyValue != '') {
              computedStyle[key] = propertyValue
            }
          })
        }

        return computedStyle
      }

      function getDOMAttribute(element: HTMLElement, attributeName: string): string | null {
        const attr = element.attributes.getNamedItemNS(null, attributeName)
        if (attr == null) {
          return null
        } else {
          return attr.value
        }
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
            const metadata = walkSceneInner(scene, index, scenePath, validPaths)
            rootMetadata.push(...metadata)

            const sceneMetadata = collectMetadata(
              scene,
              TP.instancePath([], TP.elementPathForPath(scenePath)),
              null,
              metadata,
            )
            rootMetadata.push(sceneMetadata)
          }
        }
      }

      function walkCanvasRootFragment(
        canvasRoot: HTMLElement,
        index: number,
        canvasRootPath: TemplatePath,
        validPaths: Array<string>,
      ) {
        const childMetadata = walkSceneInner(canvasRoot, index, canvasRootPath, validPaths)
        // The Storyboard root being a fragment means it is invisible to us in the DOM walker,
        // so walkCanvasRootFragment will create a fake root ElementInstanceMetadata
        // to provide a home for the the (really existing) childMetadata
        const metadata: ElementInstanceMetadata = elementInstanceMetadata(
          canvasRootPath as InstancePath,
          left('Storyboard'),
          {},
          null,
          null,
          childMetadata,
          false,
          emptySpecialSizeMeasurements,
          emptyComputedStyle,
        )
        rootMetadata.push(metadata)
      }

      function walkSceneInner(
        scene: HTMLElement,
        index: number,
        scenePath: TemplatePath,
        validPaths: Array<string>,
      ): Array<ElementInstanceMetadata> {
        const globalFrame: CanvasRectangle = globalFrameForElement(scene)

        let metadatas: Array<ElementInstanceMetadata> = []

        scene.childNodes.forEach((childNode) => {
          const metadata = walkElements(
            childNode,
            index,
            0,
            globalFrame,
            scenePath,
            scenePath,
            validPaths,
          )

          metadatas.push(...metadata)
        })

        return metadatas
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
      ): Array<ElementInstanceMetadata> {
        if (isScene(element)) {
          // we found a nested scene, restart the walk
          walkScene(element, index)
          return []
        }
        if (element instanceof HTMLElement) {
          const globalFrame = globalFrameForElement(element)

          // Determine the uid of this element if it has one.
          const uidAttribute = getDOMAttribute(element, 'data-uid')
          const originalUIDAttribute = getDOMAttribute(element, UTOPIA_ORIGINAL_ID_KEY)
          const doNotTraverseAttribute = getDOMAttribute(element, 'data-utopia-do-not-traverse')

          const traverseChildren: boolean = doNotTraverseAttribute !== 'true'

          // Build the path for this element, substituting an index in if there is no uid attribute.
          function makeIndexElement(): id {
            return `index-${index}`
          }
          const pathElement = Utils.defaultIfNullLazy<id>(uidAttribute, makeIndexElement)
          const uniquePath: TemplatePath = TP.appendToPath(uniqueParentPath, pathElement)

          let originalPath: TemplatePath | null = null
          const originalPathElement = Utils.defaultIfNull(uidAttribute, originalUIDAttribute)
          if (originalPathElement != null && originalParentPath != null) {
            originalPath = TP.appendToPath(originalParentPath, originalPathElement)
          }

          // Check this is a path we're interested in, otherwise skip straight to the children
          const pathIsValid = isValidPath(Utils.defaultIfNull(uniquePath, originalPath), validPaths)
          const pathForChildren = pathIsValid ? uniquePath : uniqueParentPath

          // Build the metadata for the children of this DOM node.
          let metadataOfChildren: Array<ElementInstanceMetadata> = []
          if (traverseChildren) {
            element.childNodes.forEach((child, childIndex) => {
              const childMetadata = walkElements(
                child,
                childIndex,
                depth + 1,
                globalFrame,
                originalPath,
                pathForChildren,
                validPaths,
              )
              if (childMetadata != null) {
                metadataOfChildren.push(...childMetadata)
              }
            })
          }

          if (pathIsValid) {
            return [collectMetadata(element, uniquePath, parentPoint, metadataOfChildren)]
          } else {
            return metadataOfChildren
          }
        } else {
          return []
        }
      }

      function collectMetadata(
        element: HTMLElement,
        instancePath: InstancePath,
        parentPoint: CanvasPoint | null,
        childrenMetadata: ElementInstanceMetadata[],
      ): ElementInstanceMetadata {
        const globalFrame = globalFrameForElement(element)
        const localFrame =
          parentPoint != null
            ? localRectangle(Utils.offsetRect(globalFrame, Utils.negate(parentPoint)))
            : null

        const uidAttribute = getDOMAttribute(element, 'data-uid')
        const originalUIDAttribute = getDOMAttribute(element, UTOPIA_ORIGINAL_ID_KEY)
        const labelAttribute = getDOMAttribute(element, 'data-label')
        let elementProps: any = {}
        if (uidAttribute != null) {
          elementProps['data-uid'] = uidAttribute
        }
        if (originalUIDAttribute != null) {
          elementProps[UTOPIA_ORIGINAL_ID_KEY] = originalUIDAttribute
        }
        if (labelAttribute != null) {
          elementProps['data-label'] = labelAttribute
        }
        return elementInstanceMetadata(
          instancePath,
          left(element.tagName.toLowerCase()),
          elementProps,
          globalFrame,
          localFrame,
          childrenMetadata,
          false,
          getSpecialMeasurements(element),
          getComputedStyle(element),
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

      props.onDomReport(rootMetadata)
    }
  })

  return containerRef
}
