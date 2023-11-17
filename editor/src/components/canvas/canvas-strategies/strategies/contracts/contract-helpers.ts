import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { toString } from '../../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import { assertNever } from '../../../../../core/shared/utils'
import invariant from '../../../../../third-party/remix/invariant'
import type { AllElementProps } from '../../../../editor/store/editor-state'
import { getElementFragmentLikeType } from '../fragment-like-helpers'
import { treatElementAsGroupLike } from '../group-helpers'

export type EditorContract = 'fragment' | 'frame' | 'group' | 'wrapper-div'

export function getEditorContractForElement(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): EditorContract {
  const fragmentLikeType = getElementFragmentLikeType(metadata, allElementProps, pathTrees, path)
  const isGroupLike = treatElementAsGroupLike(metadata, path)
  if (isGroupLike) {
    return 'group'
  }
  if (fragmentLikeType === 'fragment' || fragmentLikeType === 'conditional') {
    return 'fragment'
  }

  if (fragmentLikeType === 'sizeless-div') {
    return 'wrapper-div'
  }

  // let's check if the element is collapsed in any direction, and return wrapper-div for those cases too
  const collapsedWrapper = isCollapsedWrapper(metadata, pathTrees, path)

  if (collapsedWrapper) {
    return 'wrapper-div'
  }

  if (fragmentLikeType == null) {
    return 'frame'
  }

  assertNever(fragmentLikeType)
}

function isCollapsedWrapper(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): boolean {
  const children = MetadataUtils.getChildrenOrdered(metadata, pathTrees, path)
  const atLeastOnAbsoluteChild = children.some(MetadataUtils.isPositionAbsolute)

  const element = MetadataUtils.findElementByElementPath(metadata, path)

  if (element == null) {
    return false
  }

  invariant(element != null, `Found null metadata at ${toString(path)}`)

  const widthCollapsed = element.specialSizeMeasurements.computedHugProperty.width === 'collapsed'
  const heightCollapsed = element.specialSizeMeasurements.computedHugProperty.height === 'collapsed'

  // the element will be considered a Collapsed Wrapper if:
  // 1. it has at least one absolute child
  // 2. its Width or Height HugProperty is set to Collapsed
  return atLeastOnAbsoluteChild && (widthCollapsed || heightCollapsed)
}
