import { Spec } from 'immutability-helper'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import { drop } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath, RevisionsState } from '../../../core/shared/project-file-types'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
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
  let editorStatePatch: EditorStatePatch = {}
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const withElementRemoved = removeElementAtPath(command.target, components)
      const withElementInserted = insertElementAtPath(
        editorState.projectContents,
        editorState.canvas.openFile?.filename ?? null,
        command.newParent,
        underlyingElement,
        withElementRemoved,
        null,
      )

      const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
        success.topLevelElements,
        withElementInserted,
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
                $set: success.imports,
              },
            },
          },
        },
      }
      // ProjectContentTreeRoot is a bit awkward to patch.
      const pathElements = getProjectContentKeyPathElements(underlyingFilePath)
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

      editorStatePatch = {
        projectContents: projectContentTreeRootPatch,
      }
    },
  )

  return {
    editorStatePatch: editorStatePatch,
    commandDescription: `Reparent Element ${EP.toUid(command.target)} to new parent ${EP.toUid(
      command.newParent,
    )}`,
  }
}
