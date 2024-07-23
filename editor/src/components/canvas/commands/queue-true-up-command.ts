import type { ProjectContentTreeRoot } from '../../assets'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { AllElementProps, EditorState, TrueUpTarget } from '../../editor/store/editor-state'
import { trueUpGroupElementChanged } from '../../editor/store/editor-state'
import { allowGroupTrueUp } from '../canvas-strategies/strategies/group-helpers'
import type { BaseCommand, CommandFunction } from './commands'
import { trueUpTargetToDescription } from '../../../core/model/true-up-targets'

export interface QueueTrueUpElement extends BaseCommand {
  type: 'QUEUE_TRUE_UP_ELEMENT'
  targets: Array<TrueUpTarget>
}

export function queueTrueUpElement(targets: Array<TrueUpTarget>): QueueTrueUpElement {
  return {
    type: 'QUEUE_TRUE_UP_ELEMENT',
    whenToRun: 'on-complete',
    targets: targets,
  }
}

export const runQueueTrueUpElement: CommandFunction<QueueTrueUpElement> = (
  _: EditorState,
  command: QueueTrueUpElement,
) => {
  return {
    commandDescription: `Once the interaction has finished: ${command.targets
      .map((target) => trueUpTargetToDescription(target))
      .join(', ')}`,
    editorStatePatches: [{ trueUpElementsAfterDomWalkerRuns: { $push: command.targets } }],
  }
}

// If the target is in a group, then this will add a command for including the siblings in a trueing up.
export function getRequiredGroupTrueUps(
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  target: ElementPath,
): Array<QueueTrueUpElement> {
  const parentPath = EP.parentPath(target)
  if (
    allowGroupTrueUp(
      projectContents,
      metadata,
      domReconstructedMetadata,
      pathTrees,
      allElementProps,
      parentPath,
    )
  ) {
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, pathTrees, target)
    return [
      queueTrueUpElement(siblings.map((sibling) => trueUpGroupElementChanged(sibling.elementPath))),
    ]
  } else {
    return []
  }
}
