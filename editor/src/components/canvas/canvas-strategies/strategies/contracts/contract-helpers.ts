import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import { assertNever } from '../../../../../core/shared/utils'
import type { AllElementProps } from '../../../../editor/store/editor-state'
import { getElementFragmentLikeType } from '../fragment-like-helpers'
import { treatElementAsGroupLike } from '../group-helpers'

export type EditorContract = 'fragment' | 'frame' | 'group' | 'wrapper-div' | 'not-quite-frame'

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
    return 'not-quite-frame'
  }

  if (fragmentLikeType === 'wrapper-div') {
    return 'wrapper-div'
  }

  if (fragmentLikeType == null) {
    return 'frame'
  }

  assertNever(fragmentLikeType)
}
