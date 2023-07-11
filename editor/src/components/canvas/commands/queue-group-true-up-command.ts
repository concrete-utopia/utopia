import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState } from '../../editor/store/editor-state'
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
