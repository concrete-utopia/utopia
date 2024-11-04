import type { ElementPath } from 'utopia-shared/src/types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import type { EditorState, EditorStatePatch, ValidOrInvalid } from '../../editor/store/editor-state'
import type { GridCellCoordinates } from '../canvas-strategies/strategies/grid-cell-bounds'

export interface ShowGridControlsCommand extends BaseCommand {
  type: 'SHOW_GRID_CONTROLS'
  target: ElementPath
  targetCell: GridCellCoordinates | null
  rootCell: GridCellCoordinates | null
  rootCellTargetValidity: ValidOrInvalid
}

export function showGridControls(
  whenToRun: WhenToRun,
  target: ElementPath,
  targetCell: GridCellCoordinates | null,
  rootCell: GridCellCoordinates | null,
  rootCellTargetValidity: ValidOrInvalid,
): ShowGridControlsCommand {
  return {
    type: 'SHOW_GRID_CONTROLS',
    whenToRun: whenToRun,
    target: target,
    targetCell: targetCell,
    rootCell: rootCell,
    rootCellTargetValidity: rootCellTargetValidity,
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
            rootCellTargetValidity: command.rootCellTargetValidity,
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
