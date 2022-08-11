import { toString } from '../../../core/shared/element-path'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CanvasFrameAndTarget } from '../canvas-types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface PushIntendedBounds extends BaseCommand {
  type: 'PUSH_INTENDED_BOUNDS'
  value: Array<CanvasFrameAndTarget>
}

export function pushIntendedBounds(value: Array<CanvasFrameAndTarget>): PushIntendedBounds {
  return {
    type: 'PUSH_INTENDED_BOUNDS',
    whenToRun: 'mid-interaction',
    value: value,
  }
}

export const runPushIntendedBounds: CommandFunction<PushIntendedBounds> = (
  _: EditorState,
  command: PushIntendedBounds,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        strategyIntendedBounds: { $push: command.value },
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set Intended Bounds for ${command.value
      .map((c) => toString(c.target))
      .join(', ')}`,
  }
}
