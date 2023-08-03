import type { ProjectContentTreeRoot } from '../../../components/assets'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { AllElementProps, EditorState, TrueUpTarget } from '../../editor/store/editor-state'
import { trueUpElementChanged } from '../../editor/store/editor-state'
import { allowGroupTrueUp } from '../canvas-strategies/strategies/group-helpers'
import type { BaseCommand, CommandFunction } from './commands'
import { trueUpTargetToDescription } from '../../../core/model/groups'

export interface QueueGroupTrueUp extends BaseCommand {
  type: 'QUEUE_GROUP_TRUE_UP'
  target: TrueUpTarget
}

export function queueGroupTrueUp(target: TrueUpTarget): QueueGroupTrueUp {
  return {
    type: 'QUEUE_GROUP_TRUE_UP',
    whenToRun: 'on-complete',
    target: target,
  }
}

export const runQueueGroupTrueUp: CommandFunction<QueueGroupTrueUp> = (
  editorState: EditorState,
  command: QueueGroupTrueUp,
) => {
  return {
    commandDescription: `Once the interaction has finished: ${trueUpTargetToDescription(
      command.target,
    )}`,
    editorStatePatches: [{ trueUpGroupsForElementAfterDomWalkerRuns: { $push: [command.target] } }],
  }
}

// If the target is in a group, then this will add a command for including the siblings in a trueing up.
export function getRequiredGroupTrueUps(
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  target: ElementPath,
): Array<QueueGroupTrueUp> {
  const parentPath = EP.parentPath(target)
  if (allowGroupTrueUp(projectContents, metadata, pathTrees, allElementProps, parentPath)) {
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, pathTrees, target)
    return siblings.map((sibling) => queueGroupTrueUp(trueUpElementChanged(sibling.elementPath)))
  } else {
    return []
  }
}
