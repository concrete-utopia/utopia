import type { EditorState, TrueUpTarget } from '../../components/editor/store/editor-state'
import * as EP from '../../core/shared/element-path'
import type { ElementPathTrees } from '../shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../shared/element-template'
import type { ElementPath } from '../shared/project-file-types'
import { assertNever } from '../shared/utils'
import { MetadataUtils } from './element-metadata-utils'

export function trueUpTargetToDescription(trueUpTarget: TrueUpTarget): string {
  switch (trueUpTarget.type) {
    case 'TRUE_UP_ELEMENT_CHANGED':
      return `True up element ${EP.toString(trueUpTarget.target)}`
    case 'TRUE_UP_CHILDREN_OF_ELEMENT_CHANGED':
      return `True up children of ${EP.toString(trueUpTarget.targetParent)}`
    default:
      assertNever(trueUpTarget)
  }
}

export function trueUpTargetToTargets(
  metadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  trueUpTarget: TrueUpTarget,
): Array<ElementPath> {
  switch (trueUpTarget.type) {
    case 'TRUE_UP_ELEMENT_CHANGED':
      return [trueUpTarget.target]
    case 'TRUE_UP_CHILDREN_OF_ELEMENT_CHANGED':
      return MetadataUtils.getChildrenPathsOrdered(metadata, pathTree, trueUpTarget.targetParent)
    default:
      assertNever(trueUpTarget)
  }
}

export function addToTrueUpGroups(
  editor: EditorState,
  ...targets: Array<TrueUpTarget>
): EditorState {
  return {
    ...editor,
    trueUpGroupsForElementAfterDomWalkerRuns: [
      ...editor.trueUpGroupsForElementAfterDomWalkerRuns,
      ...targets,
    ],
  }
}
