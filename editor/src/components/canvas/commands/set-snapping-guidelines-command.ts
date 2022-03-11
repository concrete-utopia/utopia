import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { GuidelineWithSnappingVector } from '../guideline'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface SetSnappingGuidelines extends BaseCommand {
  type: 'SET_SNAPPING_GUIDELINES'
  value: Array<GuidelineWithSnappingVector>
}

export function setSnappingGuidelines(
  transient: TransientOrNot,
  value: Array<GuidelineWithSnappingVector>,
): SetSnappingGuidelines {
  return {
    type: 'SET_SNAPPING_GUIDELINES',
    transient: transient,
    value: value,
  }
}

export const runSetSnappingGuidelines: CommandFunction<SetSnappingGuidelines> = (
  _: EditorState,
  command: SetSnappingGuidelines,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        snappingGuidelines: { $set: command.value },
      },
    },
  }
  return {
    editorStatePatch: editorStatePatch,
    commandDescription: `Set Snapping Guidelines`,
  }
}
