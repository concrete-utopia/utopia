import * as EP from '../../../core/shared/element-path'
import { isUtopiaJSXComponent } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { duplicate } from '../canvas-utils'
import type { BaseCommand, CommandFunction, CommandState, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'

export type Anchor = 'before' | 'after'

export interface DuplicateElement extends BaseCommand {
  type: 'DUPLICATE_ELEMENT'
  target: ElementPath
  newUid: string
  anchor?: Anchor
}

export function duplicateElement(
  whenToRun: WhenToRun,
  target: ElementPath,
  newUid: string,
  anchor?: Anchor,
): DuplicateElement {
  return {
    type: 'DUPLICATE_ELEMENT',
    whenToRun: whenToRun,
    target: target,
    newUid: newUid,
    anchor,
  }
}

export const runDuplicateElement: CommandFunction<DuplicateElement> = (
  editorState: EditorState,
  command: DuplicateElement,
  commandState: CommandState,
) => {
  const targetParent = EP.parentPath(command.target)
  const guessedNewPath = EP.appendToPath(targetParent, command.newUid)

  const duplicateResult = duplicate(
    [command.target],
    targetParent,
    editorState,
    [{ originalPath: command.target, newUID: command.newUid }],
    command.anchor,
  )

  const originalParsedFile = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    null,
    (parseSuccess, _, __, filePath) => ({ parseSuccess, filePath }),
  )

  if (duplicateResult == null || originalParsedFile == null) {
    return {
      editorStatePatches: [],
      commandState: commandState,
      commandDescription: `Duplicate Element Failed`,
    }
  }

  const actualNewPath = duplicateResult.reparentedPathsLookup[EP.toString(guessedNewPath)]

  const newUtopiaComponents = withUnderlyingTargetFromEditorState(
    actualNewPath,
    duplicateResult.updatedEditorState,
    [],
    (parsedFile) => {
      return parsedFile.topLevelElements.filter(isUtopiaJSXComponent) // TODO why can't getPatchForComponentChange just take all top level elements?
    },
  )

  const editorStatePatch: EditorStatePatch = getPatchForComponentChange(
    originalParsedFile?.parseSuccess.topLevelElements,
    newUtopiaComponents,
    originalParsedFile?.parseSuccess.imports,
    originalParsedFile?.filePath,
  )

  const jsxMetadataPatch: EditorStatePatch = {
    jsxMetadata: { $set: duplicateResult.updatedEditorState.jsxMetadata },
  }

  return {
    editorStatePatches: [editorStatePatch, jsxMetadataPatch],
    commandState: {
      ...commandState,
      reparentedPathsLookup: {
        ...commandState.reparentedPathsLookup,
        ...duplicateResult.reparentedPathsLookup,
      },
    },
    commandDescription: `Duplicate Element ${EP.toUid(command.target)}`,
  }
}
