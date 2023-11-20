import type { CanvasRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import type { EditorState } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction } from './commands'

export type ActiveFrameAction =
  | 'move'
  | 'resize'
  | 'set-gap'
  | 'set-padding'
  | 'set-radius'
  | 'reorder'

export function activeFrameActionToString(action: ActiveFrameAction): string {
  switch (action) {
    case 'move':
      return 'Move'
    case 'reorder':
      return 'Reorder'
    case 'resize':
      return 'Resize'
    case 'set-gap':
      return 'Set flex gap'
    case 'set-padding':
      return 'Set padding'
    case 'set-radius':
      return 'Set radius'
    default:
      assertNever(action)
  }
}

export type ActiveFrame = {
  action: ActiveFrameAction
  frame?: CanvasRectangle
  path?: ElementPath
}

export interface SetActiveFrames extends BaseCommand {
  type: 'SET_ACTIVE_FRAMES'
  activeFrames: Array<ActiveFrame>
}

export function setActiveFrames(activeFrames: Array<ActiveFrame>): SetActiveFrames {
  return {
    type: 'SET_ACTIVE_FRAMES',
    whenToRun: 'mid-interaction',
    activeFrames: activeFrames,
  }
}

export const runSetActiveFrames: CommandFunction<SetActiveFrames> = (
  _: EditorState,
  command: SetActiveFrames,
) => {
  return {
    commandDescription: `Set active frames`,
    editorStatePatches: [{ activeFrames: { $push: command.activeFrames } }],
  }
}
