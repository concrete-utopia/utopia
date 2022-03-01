import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, PathMappings, TransientOrNot } from './commands'

export interface WildcardPatch extends BaseCommand {
  type: 'WILDCARD_PATCH'
  patch: EditorStatePatch
}

export function wildcardPatch(transient: TransientOrNot, patch: EditorStatePatch): WildcardPatch {
  return {
    type: 'WILDCARD_PATCH',
    transient: transient,
    patch: patch,
  }
}

export const runWildcardPatch: CommandFunction<WildcardPatch> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: WildcardPatch,
) => {
  return {
    editorStatePatch: command.patch,
    pathMappings: pathMappings,
    commandDescription: `Wildcard Patch: ${JSON.stringify(command.patch, null, 2)}`,
  }
}
