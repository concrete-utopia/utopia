import { Spec } from 'immutability-helper'
import { openFileMessage } from 'utopia-vscode-common'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import { drop, last, dropLast } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { TopLevelElement, UtopiaJSXComponent } from '../../../core/shared/element-template'
import {
  ElementPath,
  Imports,
  ParseSuccess,
  RevisionsState,
} from '../../../core/shared/project-file-types'
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
    (
      successOldParent,
      underlyingElementOldParent,
      underlyingTargetOldParent,
      underlyingFilePathOldParent,
    ) => {
      forUnderlyingTargetFromEditorState(
        command.newParent,
        editorState,
        (
          successNewParent,
          underlyingElementNewParent,
          underlyingTargetNewParent,
          underlyingFilePathNewParent,
        ) => {
          if (underlyingFilePathOldParent === underlyingFilePathNewParent) {
            const components = getUtopiaJSXComponentsFromSuccess(successOldParent)
            const withElementRemoved = removeElementAtPath(command.target, components)

            const withElementInserted = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathOldParent,
              command.newParent,
              underlyingElementOldParent,
              withElementRemoved,
              null,
            )
            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successOldParent.topLevelElements,
              withElementInserted,
              successOldParent.imports,
              underlyingFilePathOldParent,
            )

            editorStatePatches = [editorStatePatchOldParentFile]
          } else {
            const componentsOldParent = getUtopiaJSXComponentsFromSuccess(successOldParent)
            const withElementRemoved = removeElementAtPath(command.target, componentsOldParent)
            const componentsNewParent = getUtopiaJSXComponentsFromSuccess(successNewParent)

            const withElementInserted = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathNewParent,
              command.newParent,
              underlyingElementNewParent,
              componentsNewParent,
              null,
            )

            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successOldParent.topLevelElements,
              withElementRemoved,
              successOldParent.imports,
              underlyingFilePathOldParent,
            )
            const editorStatePatchNewParentFile = getPatchForComponentChange(
              successNewParent.topLevelElements,
              withElementInserted,
              successNewParent.imports,
              underlyingFilePathNewParent,
            )

            editorStatePatches = [editorStatePatchOldParentFile, editorStatePatchNewParentFile]
          }
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

function getPatchForComponentChange(
  topLevelElements: Array<TopLevelElement>,
  newUtopiaComponents: Array<UtopiaJSXComponent>,
  imports: Imports,
  filePath: string,
) {
  const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
    topLevelElements,
    newUtopiaComponents,
  )
  const projectContentFilePatch: Spec<ProjectContentFile> = {
    content: {
      fileContents: {
        revisionsState: {
          $set: RevisionsState.ParsedAhead,
        },
        parsed: {
          topLevelElements: {
            $set: updatedTopLevelElements,
          },
          imports: {
            $set: imports,
          },
        },
      },
    },
  }
  // ProjectContentTreeRoot is a bit awkward to patch.
  const pathElements = getProjectContentKeyPathElements(filePath)
  if (pathElements.length === 0) {
    throw new Error('Invalid path length.')
  }
  const remainderPath = drop(1, pathElements)
  const projectContentsTreePatch: Spec<ProjectContentsTree> = remainderPath.reduceRight(
    (working: Spec<ProjectContentsTree>, pathPart: string) => {
      return {
        children: {
          [pathPart]: working,
        },
      }
    },
    projectContentFilePatch,
  )

  // Finally patch the last part of the path in.
  const projectContentTreeRootPatch: Spec<ProjectContentTreeRoot> = {
    [pathElements[0]]: projectContentsTreePatch,
  }

  return {
    projectContents: projectContentTreeRootPatch,
  }
}
