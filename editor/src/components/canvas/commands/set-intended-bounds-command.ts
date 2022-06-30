import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CanvasFrameAndTarget } from '../canvas-types'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface SetIntendedBounds extends BaseCommand {
  type: 'SET_INTENDED_BOUNDS'
  value: Array<CanvasFrameAndTarget>
}

export function setIntendedBounds(
  transient: TransientOrNot,
  value: Array<CanvasFrameAndTarget>,
): SetIntendedBounds {
  return {
    type: 'SET_INTENDED_BOUNDS',
    transient: transient,
    value: value,
  }
}

export const runSetIntendedBounds: CommandFunction<SetIntendedBounds> = (
  _: EditorState,
  command: SetIntendedBounds,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        strategyIntendedBounds: { $set: command.value },
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set Intended Bounds`,
  }
}
