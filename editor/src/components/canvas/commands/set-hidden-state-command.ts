import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction } from './commands'

export interface SetHiddenState extends BaseCommand {
  type: 'SET_HIDDEN_STATE'
  paths: Array<ElementPath>
  hidden: boolean
}

export function setHiddenState(paths: Array<ElementPath>, hidden: boolean): SetHiddenState {
  return {
    type: 'SET_HIDDEN_STATE',
    whenToRun: 'mid-interaction',
    paths: paths,
    hidden: hidden,
  }
}

export const runSetHiddenState: CommandFunction<SetHiddenState> = (
  editorState: EditorState,
  command: SetHiddenState,
) => {
  const hiddenInstances = editorState.hiddenInstances
  const commandPathsSet = new Set(command.paths.map(EP.toString))
  const newHiddenInstances = command.hidden
    ? [...hiddenInstances, ...command.paths]
    : hiddenInstances.filter((hiddenPath) => !commandPathsSet.has(EP.toString(hiddenPath)))

  const editorStatePatch: EditorStatePatch = {
    hiddenInstances: { $set: newHiddenInstances },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set hidden state for paths: ${command.paths
      .map((c) => EP.toString(c))
      .join(', ')} to ${command.hidden ? 'hidden' : 'shown'}`,
  }
}
