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
  | 'duplicate'
  | 'reparent'

export function activeFrameActionToString(action: ActiveFrameAction): string {
  switch (action) {
    case 'move':
      return 'Move'
    case 'reorder':
      return 'Reorder'
    case 'resize':
      return 'Resize'
    case 'set-gap':
      return 'Gap'
    case 'set-padding':
      return 'Padding'
    case 'set-radius':
      return 'Radius'
    case 'duplicate':
      return 'Duplicate'
    case 'reparent':
      return 'Reparent'
    default:
      assertNever(action)
  }
}

export type ActiveFrame = {
  action: ActiveFrameAction
  target: ActiveFrameTarget
  source: CanvasRectangle
}

export type ActiveFrameTargetRect = {
  type: 'ACTIVE_FRAME_TARGET_RECT'
  rect: CanvasRectangle
}

export function isActiveFrameTargetRect(
  target: ActiveFrameTarget,
): target is ActiveFrameTargetRect {
  return target.type === 'ACTIVE_FRAME_TARGET_RECT'
}

export function activeFrameTargetRect(rect: CanvasRectangle): ActiveFrameTargetRect {
  return {
    type: 'ACTIVE_FRAME_TARGET_RECT',
    rect: rect,
  }
}

export type ActiveFrameTargetPath = {
  type: 'ACTIVE_FRAME_TARGET_PATH'
  path: ElementPath
}

export function isActiveFrameTargetPath(
  target: ActiveFrameTarget,
): target is ActiveFrameTargetPath {
  return target.type === 'ACTIVE_FRAME_TARGET_PATH'
}

export function activeFrameTargetPath(path: ElementPath): ActiveFrameTargetPath {
  return {
    type: 'ACTIVE_FRAME_TARGET_PATH',
    path: path,
  }
}

export type ActiveFrameTarget = ActiveFrameTargetRect | ActiveFrameTargetPath

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
