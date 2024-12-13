import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import type { EditorState, EditorStatePatch, GridIdentifier } from '../../editor/store/editor-state'
import type { GridCellCoordinates } from '../canvas-strategies/strategies/grid-cell-bounds'

export interface ShowGridControlsCommand extends BaseCommand {
  type: 'SHOW_GRID_CONTROLS'
  target: GridIdentifier
  targetCell: GridCellCoordinates | null
  rootCell: GridCellCoordinates | null
}

export function showGridControls(
  whenToRun: WhenToRun,
  target: GridIdentifier,
  targetCell: GridCellCoordinates | null,
  rootCell: GridCellCoordinates | null,
): ShowGridControlsCommand {
  return {
    type: 'SHOW_GRID_CONTROLS',
    whenToRun: whenToRun,
    target: target,
    targetCell: targetCell,
    rootCell: rootCell,
  }
}

export const runShowGridControlsCommand: CommandFunction<ShowGridControlsCommand> = (
  _: EditorState,
  command: ShowGridControlsCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        gridControlData: {
          $set: {
            grid: command.target,
            targetCell: command.targetCell,
            rootCell: command.rootCell,
          },
        },
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Show Grid Controls`,
  }
}
