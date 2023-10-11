import type { PushIntendedBoundsTarget } from '../../components/canvas/commands/push-intended-bounds-and-update-groups-command'
import {
  pushIntendedBoundsEmpty,
  pushIntendedBoundsGroup,
} from '../../components/canvas/commands/push-intended-bounds-and-update-groups-command'
import type { EditorState, TrueUpTarget } from '../../components/editor/store/editor-state'
import * as EP from '../../core/shared/element-path'
import type { ElementPathTrees } from '../shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../shared/element-template'
import type { CanvasRectangle } from '../shared/math-utils'
import { isInfinityRectangle } from '../shared/math-utils'
import type { ElementPath } from '../shared/project-file-types'
import { assertNever } from '../shared/utils'
import { MetadataUtils } from './element-metadata-utils'

export function trueUpTargetToDescription(trueUpTarget: TrueUpTarget): string {
  switch (trueUpTarget.type) {
    case 'TRUE_UP_ELEMENT_CHANGED':
      return `True up element ${EP.toString(trueUpTarget.target)}`
    case 'TRUE_UP_CHILDREN_OF_ELEMENT_CHANGED':
      return `True up children of ${EP.toString(trueUpTarget.targetParent)}`
    case 'TRUE_UP_EMPTY_ELEMENT':
      return `True up empty element ${EP.toString(trueUpTarget.target)}`
    default:
      assertNever(trueUpTarget)
  }
}

export function trueUpTargetToNormalizeBounds(
  metadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  trueUpTarget: TrueUpTarget,
): Array<PushIntendedBoundsTarget | null> {
  function getFrame(target: ElementPath): CanvasRectangle | null {
    const globalFrame = MetadataUtils.findElementByElementPath(metadata, target)?.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return null
    }
    return globalFrame
  }
  switch (trueUpTarget.type) {
    case 'TRUE_UP_ELEMENT_CHANGED':
      const elementFrame = getFrame(trueUpTarget.target)
      return [
        elementFrame != null ? pushIntendedBoundsGroup(trueUpTarget.target, elementFrame) : null,
      ]
    case 'TRUE_UP_CHILDREN_OF_ELEMENT_CHANGED':
      return MetadataUtils.getChildrenPathsOrdered(
        metadata,
        pathTree,
        trueUpTarget.targetParent,
      ).map((path) => {
        const childFrame = getFrame(path)
        return childFrame != null ? pushIntendedBoundsGroup(path, childFrame) : null
      })
    case 'TRUE_UP_EMPTY_ELEMENT':
      return [pushIntendedBoundsEmpty(trueUpTarget.target, trueUpTarget.frame)]
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
