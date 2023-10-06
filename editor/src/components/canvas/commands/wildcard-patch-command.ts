import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface WildcardPatch extends BaseCommand {
  type: 'WILDCARD_PATCH'
  patch: EditorStatePatch
}

export function wildcardPatch(whenToRun: WhenToRun, patch: EditorStatePatch): WildcardPatch {
  return {
    type: 'WILDCARD_PATCH',
    whenToRun: whenToRun,
    patch: patch,
  }
}

export const runWildcardPatch: CommandFunction<WildcardPatch> = (
  _: EditorState,
  command: WildcardPatch,
) => {
  return {
    editorStatePatches: [command.patch],
    commandDescription: `Wildcard Patch: ${JSON.stringify(command.patch, null, 2)}`,
  }
}
