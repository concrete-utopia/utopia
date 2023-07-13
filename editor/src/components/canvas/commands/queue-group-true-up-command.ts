import type { ElementPathTrees } from 'src/core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from 'src/core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState } from '../../editor/store/editor-state'
import { treatElementAsGroupLike } from '../canvas-strategies/strategies/group-helpers'
import type { BaseCommand, CommandFunction } from './commands'

export interface QueueGroupTrueUp extends BaseCommand {
  type: 'QUEUE_GROUP_TRUE_UP'
  element: ElementPath
}

export function queueGroupTrueUp(element: ElementPath): QueueGroupTrueUp {
  return {
    type: 'QUEUE_GROUP_TRUE_UP',
    whenToRun: 'on-complete',
    element: element,
  }
}

export const runQueueGroupTrueUp: CommandFunction<QueueGroupTrueUp> = (
  editorState: EditorState,
  command: QueueGroupTrueUp,
) => {
  return {
    commandDescription: `Queue element for group true-up once the interaction has finished: ${EP.toString(
      command.element,
    )}`,
    editorStatePatches: [
      { trueUpGroupsForElementAfterDomWalkerRuns: { $push: [command.element] } },
    ],
  }
}

// If the target is in a group, then this will add a command for including the siblings in a trueing up.
export function getRequiredGroupTrueUps(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  target: ElementPath,
): Array<QueueGroupTrueUp> {
  const parentPath = EP.parentPath(target)
  if (treatElementAsGroupLike(metadata, pathTrees, parentPath)) {
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, pathTrees, target)
    return siblings.map((sibling) => queueGroupTrueUp(sibling.elementPath))
  } else {
    return []
  }
}
