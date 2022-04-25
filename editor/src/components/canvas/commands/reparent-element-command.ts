import { Spec } from 'immutability-helper'
import { openFileMessage } from 'utopia-vscode-common'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import { drop, last, dropLast } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath, RevisionsState } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { normalisePathToUnderlyingTarget } from '../../custom-code/code-file'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface ReparentElement extends BaseCommand {
  type: 'REPARENT_ELEMENT'
  target: ElementPath
  newParent: ElementPath
}

export function reparentElement(
  transient: TransientOrNot,
  target: ElementPath,
  newParent: ElementPath,
): ReparentElement {
  return {
    type: 'REPARENT_ELEMENT',
    transient: transient,
    target: target,
    newParent: newParent,
  }
}

export const runReparentElement: CommandFunction<ReparentElement> = (
  editorState: EditorState,
  command: ReparentElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      forUnderlyingTargetFromEditorState(
        command.newParent,
        editorState,
        (successInner, underlyingElementInner, underlyingTargetInner, underlyingFilePathInner) => {
          const components = getUtopiaJSXComponentsFromSuccess(success)
          const withElementRemoved = removeElementAtPath(command.target, components)
          const componentsFromParentFile = getUtopiaJSXComponentsFromSuccess(successInner)

          const withElementInserted = insertElementAtPath(
            editorState.projectContents,
            underlyingFilePathInner,
            command.newParent,
            underlyingElement,
            componentsFromParentFile,
            null,
          )

          const updatedTopLevelElementsChildFile = applyUtopiaJSXComponentsChanges(
            success.topLevelElements,
            withElementRemoved,
          )
          const updatedTopLevelElementsParentFile = applyUtopiaJSXComponentsChanges(
            success.topLevelElements,
            withElementInserted,
          )
          const projectContentFilePatchChildFile: Spec<ProjectContentFile> = {
            content: {
              fileContents: {
                revisionsState: {
                  $set: RevisionsState.ParsedAhead,
                },
                parsed: {
                  topLevelElements: {
                    $set: updatedTopLevelElementsChildFile,
                  },
                  imports: {
                    $set: success.imports,
                  },
                },
              },
            },
          }
          const projectContentFilePatchParentFile: Spec<ProjectContentFile> = {
            content: {
              fileContents: {
                revisionsState: {
                  $set: RevisionsState.ParsedAhead,
                },
                parsed: {
                  topLevelElements: {
                    $set: updatedTopLevelElementsParentFile,
                  },
                  imports: {
                    $set: success.imports,
                  },
                },
              },
            },
          }
          // ProjectContentTreeRoot is a bit awkward to patch.
          const pathElementsChildFile = getProjectContentKeyPathElements(underlyingFilePath)
          if (pathElementsChildFile.length === 0) {
            throw new Error('Invalid path length.')
          }
          const remainderPathChildFile = drop(1, pathElementsChildFile)

          // ProjectContentTreeRoot is a bit awkward to patch.
          const pathElementsParentFile = getProjectContentKeyPathElements(underlyingFilePathInner)
          if (pathElementsChildFile.length === 0) {
            throw new Error('Invalid path length.')
          }
          const remainderPathParentFile = drop(1, pathElementsParentFile)

          const projectContentsTreePatchParentFile: Spec<ProjectContentsTree> = remainderPathParentFile.reduceRight(
            (working: Spec<ProjectContentsTree>, pathPart: string) => {
              return {
                children: {
                  [pathPart]: working,
                },
              }
            },
            projectContentFilePatchParentFile,
          )

          const projectContentsTreePatchChildFile: Spec<ProjectContentsTree> = remainderPathChildFile.reduceRight(
            (working: Spec<ProjectContentsTree>, pathPart: string) => {
              return {
                children: {
                  [pathPart]: working,
                },
              }
            },
            projectContentFilePatchChildFile,
          )

          // Finally patch the last part of the path in.
          const projectContentTreeRootPatchChildFile: Spec<ProjectContentTreeRoot> = {
            [pathElementsChildFile[0]]: projectContentsTreePatchChildFile,
          }

          // Finally patch the last part of the path in.
          const projectContentTreeRootPatchParentFile: Spec<ProjectContentTreeRoot> = {
            [pathElementsParentFile[0]]: projectContentsTreePatchParentFile,
          }

          const editorStatePatchChildFile = {
            projectContents: projectContentTreeRootPatchChildFile,
          }
          const editorStatePatchParentFile = {
            projectContents: projectContentTreeRootPatchParentFile,
          }

          editorStatePatches = [editorStatePatchChildFile, editorStatePatchParentFile]
        },
      )
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Reparent Element ${EP.toUid(command.target)} to new parent ${EP.toUid(
      command.newParent,
    )}`,
  }
}
