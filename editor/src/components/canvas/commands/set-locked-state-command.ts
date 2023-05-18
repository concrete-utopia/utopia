import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { SelectionLocked } from '../canvas-types'
import type { BaseCommand, CommandFunction } from './commands'

export interface SetLockedState extends BaseCommand {
  type: 'SET_LOCKED_STATE'
  paths: Array<ElementPath>
  state: SelectionLocked
}

export function setLockedState(paths: Array<ElementPath>, state: SelectionLocked): SetLockedState {
  return {
    type: 'SET_LOCKED_STATE',
    whenToRun: 'always',
    paths: paths,
    state: state,
  }
}

export const runSetLockedState: CommandFunction<SetLockedState> = (
  editorState: EditorState,
  command: SetLockedState,
) => {
  const simpleLockedPaths = editorState.lockedElements.simpleLock
  const hierarchyLockedPaths = editorState.lockedElements.hierarchyLock

  const editorStatePatch: EditorStatePatch =
    command.state === 'locked'
      ? {
          lockedElements: {
            simpleLock: { $set: [...simpleLockedPaths, ...command.paths] },
            hierarchyLock: {
              $set: arrayDifference(hierarchyLockedPaths, command.paths),
            },
          },
        }
      : command.state === 'locked-hierarchy'
      ? {
          lockedElements: {
            simpleLock: {
              $set: arrayDifference(simpleLockedPaths, command.paths),
            },
            hierarchyLock: { $set: [...hierarchyLockedPaths, ...command.paths] },
          },
        }
      : command.state === 'selectable'
      ? {
          lockedElements: {
            simpleLock: {
              $set: arrayDifference(simpleLockedPaths, command.paths),
            },
            hierarchyLock: { $set: arrayDifference(hierarchyLockedPaths, command.paths) },
          },
        }
      : assertNever(command.state)

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set locked state for paths: ${command.paths
      .map((c) => EP.toString(c))
      .join(', ')} to ${command.state}`,
  }
}

function arrayDifference(original: ElementPath[], elementsToRemove: ElementPath[]): ElementPath[] {
  const elementsToRemoveSet = new Set(elementsToRemove.map(EP.toString))
  return original.filter((elem) => !elementsToRemoveSet.has(EP.toString(elem)))
}
