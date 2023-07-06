import { mergeImports } from '../../../core/workers/common/project-file-utils'
import type { EditorState } from '../../../components/editor/store/editor-state'
import {
  modifyParseSuccessAtPath,
  modifyUnderlyingElementForOpenFile,
} from '../../../components/editor/store/editor-state'
import type { Imports } from '../../../core/shared/project-file-types'
import {
  isParseSuccess,
  isTextFile,
  RevisionsState,
  TextFile,
} from '../../../core/shared/project-file-types'
import type { BaseCommand, WhenToRun, CommandFunction, CommandFunctionResult } from './commands'
import { Spec } from 'immutability-helper'
import { ProjectContentTreeRoot } from '../../../components/assets'
import { patchParseSuccessAtFilePath } from './patch-utils'

export interface AddImportsToFile extends BaseCommand {
  type: 'ADD_IMPORTS_TO_FILE'
  targetFile: string
  imports: Imports
}

export function addImportsToFile(
  whenToRun: WhenToRun,
  targetFile: string,
  imports: Imports,
): AddImportsToFile {
  return {
    type: 'ADD_IMPORTS_TO_FILE',
    whenToRun: whenToRun,
    targetFile: targetFile,
    imports: imports,
  }
}

export const runAddImportsToFile: CommandFunction<AddImportsToFile> = (
  editorState: EditorState,
  command: AddImportsToFile,
): CommandFunctionResult => {
  const editorStatePatch = patchParseSuccessAtFilePath(
    command.targetFile,
    editorState,
    (parseSuccess) => {
      const updatedImports = mergeImports(command.targetFile, parseSuccess.imports, command.imports)
      return {
        imports: {
          $set: updatedImports,
        },
      }
    },
  )

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Added imports to ${command.targetFile}.`,
  }
}
