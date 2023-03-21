import { CSSProperties } from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributesFromMap,
  jsxAttributeValue,
  jsxElement,
  JSXElement,
} from '../../../../core/shared/element-template'
import { is } from '../../../../core/shared/equality-utils'
import { memoize } from '../../../../core/shared/memoize'
import { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { defaultUnstyledDivElement } from '../../../editor/defaults'
import { AllElementProps } from '../../../editor/store/editor-state'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { flattenSelection } from './shared-move-strategies-helpers'

export function retargetStrategyToChildrenOfContentAffectingElements(
  canvasState: InteractionCanvasState,
): Array<ElementPath> {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  const targetsWithoutDescedants = flattenSelection(targets)

  return replaceContentAffectingPathsWithTheirChildrenRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    targetsWithoutDescedants,
  )
}

export const replaceContentAffectingPathsWithTheirChildrenRecursive = memoize(
  replaceContentAffectingPathsWithTheirChildrenRecursiveInner,
  { maxSize: 1, equals: is },
)

function replaceContentAffectingPathsWithTheirChildrenRecursiveInner(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  let pathsWereReplaced = false

  const updatedPaths = paths.flatMap((path) => {
    const elementIsContentAffecting = treatElementAsContentAffecting(
      metadata,
      allElementProps,
      path,
    )

    if (elementIsContentAffecting) {
      const children = MetadataUtils.getChildrenPathsUnordered(metadata, path) // I think it's fine to get the unordered children here?
      if (children.length === 0) {
        // with no children, actually let's just return the original element
        return path
      }

      pathsWereReplaced = true
      return replaceContentAffectingPathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        children,
      )
    }

    return path
  })

  return pathsWereReplaced ? updatedPaths : paths
}

export const AllContentAffectingNonDomElementTypes = ['fragment', 'conditional'] as const
export const AllContentAffectingTypes = [
  ...AllContentAffectingNonDomElementTypes,
  'sizeless-div',
] as const
export type ContentAffectingType = typeof AllContentAffectingTypes[number] // <- this gives us the union type of the Array's entries

export function getElementContentAffectingType(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
): ContentAffectingType | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

  const elementProps = allElementProps[EP.toString(path)]

  if (MetadataUtils.isFragmentFromMetadata(elementMetadata)) {
    return 'fragment'
  }

  if (MetadataUtils.isConditionalFromMetadata(elementMetadata)) {
    return 'conditional'
  }

  if (MetadataUtils.isFlexLayoutedContainer(elementMetadata)) {
    // for now, do not treat flex parents ever as content-affecting / group-like
    return null
  }

  if (EP.isStoryboardPath(path)) {
    // the Storyboard is not children-affecting
    return null
  }

  const childrenCount = MetadataUtils.getChildrenUnordered(metadata, path).length
  if (childrenCount === 0) {
    // do not treat elements with zero children as content-affecting
    return null
  }

  const hasNoWidthAndHeightProps =
    elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

  if (hasNoWidthAndHeightProps) {
    return 'sizeless-div'
  }

  return null
}

export function treatElementAsContentAffecting(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
): boolean {
  return getElementContentAffectingType(metadata, allElementProps, path) != null
}

export function detectBestWrapperElement(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  makeUid: () => string,
): { element: JSXElement; importsToAdd: Imports; style: CSSProperties } {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const uid = makeUid()
  if (
    element == null ||
    element.specialSizeMeasurements.parentFlexDirection == null ||
    element.specialSizeMeasurements.parentLayoutSystem !== 'flex'
  ) {
    return { element: defaultUnstyledDivElement(uid), importsToAdd: {}, style: {} }
  }

  const style: CSSProperties = {
    display: 'flex',
    flexDirection: element.specialSizeMeasurements.parentFlexDirection,
    contain: 'layout',
  }

  if (
    element.specialSizeMeasurements.parentFlexGap != null &&
    element.specialSizeMeasurements.parentFlexGap !== 0
  ) {
    style.gap = element.specialSizeMeasurements.parentFlexGap
  }

  const props = jsxAttributesFromMap({
    'data-uid': jsxAttributeValue(uid, emptyComments),
    style: jsxAttributeValue(style, emptyComments),
  })

  return {
    element: jsxElement('div', uid, props, []),
    importsToAdd: {},
    style: style,
  }
}
