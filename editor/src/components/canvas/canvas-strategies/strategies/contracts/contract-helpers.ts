import { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { AllElementProps } from '../../../../editor/store/editor-state'
import { FragmentLikeType, getElementFragmentLikeType } from '../fragment-like-helpers'
import { treatElementAsGroupLike } from '../group-helpers'

export type EditorContract = 'fragment' | 'frame' | 'group' | 'not-quite-frame'

export function getEditorContractForElement(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): EditorContract {
  const fragmentLikeType = getElementFragmentLikeType(metadata, allElementProps, pathTrees, path)
  const isGroupLike = treatElementAsGroupLike(metadata, pathTrees, path)
  if (isGroupLike) {
    return 'group'
  }
  if (fragmentLikeType === 'fragment' || fragmentLikeType === 'conditional') {
    return 'fragment'
  }
  if (fragmentLikeType === 'sizeless-div') {
    return 'not-quite-frame'
  }
  return 'frame'
}
