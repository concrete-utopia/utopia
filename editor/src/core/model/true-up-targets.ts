import type {
  PushIntendedBoundsTargetGroup,
  PushIntendedBoundsTargetHuggingElement,
} from '../../components/canvas/commands/push-intended-bounds-and-update-targets-command'
import {
  pushIntendedBoundsHuggingElement,
  pushIntendedBoundsGroup,
} from '../../components/canvas/commands/push-intended-bounds-and-update-targets-command'
import type { EditorState, TrueUpTarget } from '../../components/editor/store/editor-state'
import * as EP from '../shared/element-path'
import type { ElementPathTrees } from '../shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../shared/element-template'
import type { CanvasRectangle } from '../shared/math-utils'
import { isInfinityRectangle } from '../shared/math-utils'
import type { ElementPath } from '../shared/project-file-types'
import { assertNever } from '../shared/utils'
import { MetadataUtils } from './element-metadata-utils'

export function trueUpTargetToDescription(trueUpTarget: TrueUpTarget): string {
  switch (trueUpTarget.type) {
    case 'TRUE_UP_GROUP_ELEMENT_CHANGED':
      return `True up element ${EP.toString(trueUpTarget.target)}`
    case 'TRUE_UP_CHILDREN_OF_GROUP_CHANGED':
      return `True up children of ${EP.toString(trueUpTarget.targetParent)}`
    case 'TRUE_UP_HUGGING_ELEMENT':
      return `True up hugging element ${EP.toString(trueUpTarget.target)}`
    default:
      assertNever(trueUpTarget)
  }
}

export function trueUpTargetToPushIntendedBoundsTarget(
  metadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  trueUpTarget: TrueUpTarget,
): Array<PushIntendedBoundsTargetGroup | PushIntendedBoundsTargetHuggingElement | null> {
  function getFrame(target: ElementPath): CanvasRectangle | null {
    const globalFrame = MetadataUtils.findElementByElementPath(metadata, target)?.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return null
    }
    return globalFrame
  }
  switch (trueUpTarget.type) {
    case 'TRUE_UP_GROUP_ELEMENT_CHANGED':
      const elementFrame = getFrame(trueUpTarget.target)
      return [
        elementFrame != null ? pushIntendedBoundsGroup(trueUpTarget.target, elementFrame) : null,
      ]
    case 'TRUE_UP_CHILDREN_OF_GROUP_CHANGED':
      return MetadataUtils.getChildrenPathsOrdered(
        metadata,
        pathTree,
        trueUpTarget.targetParent,
      ).map((path) => {
        const childFrame = getFrame(path)
        return childFrame != null ? pushIntendedBoundsGroup(path, childFrame) : null
      })
    case 'TRUE_UP_HUGGING_ELEMENT':
      return [pushIntendedBoundsHuggingElement(trueUpTarget.target, trueUpTarget.frame)]
    default:
      assertNever(trueUpTarget)
  }
}

export function addToTrueUpElements(
  editor: EditorState,
  ...targets: Array<TrueUpTarget>
): EditorState {
  return {
    ...editor,
    trueUpElementsAfterDomWalkerRuns: [...editor.trueUpElementsAfterDomWalkerRuns, ...targets],
  }
}
