import type { ElementPath } from 'utopia-shared/src/types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'

export interface ShowGridControlsCommand extends BaseCommand {
  type: 'SHOW_GRID_CONTROLS'
  target: ElementPath
}

export function showGridControls(
  whenToRun: WhenToRun,
  target: ElementPath,
): ShowGridControlsCommand {
  return {
    type: 'SHOW_GRID_CONTROLS',
    whenToRun: whenToRun,
    target: target,
  }
}

export const runShowGridControlsCommand: CommandFunction<ShowGridControlsCommand> = (
  _: EditorState,
  command: ShowGridControlsCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        gridControls: { $set: command.target },
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Show Grid Controls`,
  }
}
