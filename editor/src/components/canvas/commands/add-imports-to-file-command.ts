import { mergeImports } from '../../../core/workers/common/project-file-utils'
import {
  EditorState,
  modifyParseSuccessAtPath,
  modifyUnderlyingElementForOpenFile,
} from '../../../components/editor/store/editor-state'
import {
  Imports,
  isParseSuccess,
  isTextFile,
  RevisionsState,
  TextFile,
} from '../../../core/shared/project-file-types'
import { BaseCommand, WhenToRun, CommandFunction, CommandFunctionResult } from './commands'
import { Spec } from 'immutability-helper'
import { ProjectContentTreeRoot } from '../../../components/assets'
import { createProjectContentsPatch } from './patch-utils'

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
  const updatedEditorState = modifyParseSuccessAtPath(
    command.targetFile,
    editorState,
    (parseSuccess) => {
      return {
        ...parseSuccess,
        imports: mergeImports(command.targetFile, parseSuccess.imports, command.imports),
      }
    },
  )

  const projectContentTreeRootPatch: Spec<ProjectContentTreeRoot> = createProjectContentsPatch(
    command.targetFile,
    editorState.projectContents,
    (file) => {
      if (isTextFile(file)) {
        if (isParseSuccess(file.fileContents.parsed)) {
          const updatedImports = mergeImports(
            command.targetFile,
            file.fileContents.parsed.imports,
            command.imports,
          )
          const filePatch: Spec<TextFile> = {
            fileContents: {
              revisionsState: {
                $set: RevisionsState.ParsedAhead,
              },
              parsed: {
                imports: {
                  $set: updatedImports,
                },
              },
            },
          }
          return filePatch
        }
      }

      return null
    },
  )

  const editorStatePatch: Spec<EditorState> = {
    projectContents: projectContentTreeRootPatch,
  }

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Added imports to ${command.targetFile}.`,
  }
}
