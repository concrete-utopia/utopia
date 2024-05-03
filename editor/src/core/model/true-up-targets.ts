import type { CanvasFrameAndTarget } from '../../components/canvas/canvas-types'
import type { PushIntendedBoundsAndUpdateGroups } from '../../components/canvas/commands/push-intended-bounds-and-update-groups-command'
import { pushIntendedBoundsAndUpdateGroups } from '../../components/canvas/commands/push-intended-bounds-and-update-groups-command'
import {
  pushIntendedBoundsAndUpdateHuggingElements,
  type PushIntendedBoundsAndUpdateHuggingElements,
} from '../../components/canvas/commands/push-intended-bounds-and-update-hugging-elements-command'
import type { EditorState, TrueUpTarget } from '../../components/editor/store/editor-state'
import { mapDropNulls } from '../shared/array-utils'
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

type PushIntendedBoundsCommand =
  | PushIntendedBoundsAndUpdateGroups
  | PushIntendedBoundsAndUpdateHuggingElements

export function getCommandsForPushIntendedBounds(
  metadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  targets: TrueUpTarget[],
  isStartingMetadata: 'starting-metadata' | 'live-metadata',
): Array<PushIntendedBoundsCommand> {
  function getFrame(target: ElementPath): CanvasRectangle | null {
    const globalFrame = MetadataUtils.findElementByElementPath(metadata, target)?.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return null
    }
    return globalFrame
  }

  let groupTargets: Array<CanvasFrameAndTarget> = []
  let huggingElementTargets: Array<CanvasFrameAndTarget> = []

  for (const trueUpTarget of targets) {
    switch (trueUpTarget.type) {
      case 'TRUE_UP_GROUP_ELEMENT_CHANGED':
        const elementFrame = getFrame(trueUpTarget.target)
        if (elementFrame != null) {
          groupTargets.push({ target: trueUpTarget.target, frame: elementFrame })
        }
        break
      case 'TRUE_UP_CHILDREN_OF_GROUP_CHANGED':
        const children = MetadataUtils.getChildrenPathsOrdered(
          metadata,
          pathTree,
          trueUpTarget.targetParent,
        )
        groupTargets.push(
          ...mapDropNulls((child) => {
            const frame = getFrame(child)
            return frame != null ? { target: child, frame: frame } : null
          }, children),
        )
        break
      case 'TRUE_UP_HUGGING_ELEMENT':
        huggingElementTargets.push(trueUpTarget)
        break
      default:
        assertNever(trueUpTarget)
    }
  }

  return [
    pushIntendedBoundsAndUpdateGroups(groupTargets, isStartingMetadata),
    // disabling this as the collapsed elements detection is way too agressive and converts too many elements to absolute
    // pushIntendedBoundsAndUpdateHuggingElements(huggingElementTargets),
  ]
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
