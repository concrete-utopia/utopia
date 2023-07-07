import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction } from './commands'

export interface HighlightElementsCommand extends BaseCommand {
  type: 'HIGHLIGHT_ELEMENTS_COMMAND'
  value: ElementPath[]
}

export function highlightElementsCommand(value: ElementPath[]): HighlightElementsCommand {
  return {
    type: 'HIGHLIGHT_ELEMENTS_COMMAND',
    whenToRun: 'mid-interaction',
    value: value,
  }
}

export const runHighlightElementsCommand: CommandFunction<HighlightElementsCommand> = (
  _: EditorState,
  command: HighlightElementsCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    navigator: {
      highlightedTargets: { $set: command.value },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Highlight element(s): ${command.value.map(EP.toString).join()}`,
  }
}
