import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../guideline'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface SetSnappingGuidelines extends BaseCommand {
  type: 'SET_SNAPPING_GUIDELINES'
  value: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
}

export function setSnappingGuidelines(
  whenToRun: WhenToRun,
  value: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>,
): SetSnappingGuidelines {
  return {
    type: 'SET_SNAPPING_GUIDELINES',
    whenToRun: whenToRun,
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
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set Snapping Guidelines`,
  }
}
