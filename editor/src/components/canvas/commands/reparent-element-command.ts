import { Spec } from 'immutability-helper'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import { drop } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { TopLevelElement, UtopiaJSXComponent } from '../../../core/shared/element-template'
import { ElementPath, Imports, RevisionsState } from '../../../core/shared/project-file-types'
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
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (successTarget, underlyingElementTarget, _underlyingTarget, underlyingFilePathTarget) => {
      forUnderlyingTargetFromEditorState(
        command.newParent,
        editorState,
        (
          successNewParent,
          _underlyingElementNewParent,
          _underlyingTargetNewParent,
          underlyingFilePathNewParent,
        ) => {
          if (underlyingFilePathTarget === underlyingFilePathNewParent) {
            const components = getUtopiaJSXComponentsFromSuccess(successTarget)
            const withElementRemoved = removeElementAtPath(command.target, components)

            const withElementInserted = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathTarget,
              command.newParent,
              underlyingElementTarget,
              withElementRemoved,
              null,
            )
            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              withElementInserted,
              successTarget.imports,
              underlyingFilePathTarget,
            )

            editorStatePatches = [editorStatePatchOldParentFile]
          } else {
            const componentsOldParent = getUtopiaJSXComponentsFromSuccess(successTarget)
            const withElementRemoved = removeElementAtPath(command.target, componentsOldParent)
            const componentsNewParent = getUtopiaJSXComponentsFromSuccess(successNewParent)

            const withElementInserted = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathNewParent,
              command.newParent,
              underlyingElementTarget,
              componentsNewParent,
              null,
            )

            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              withElementRemoved,
              successTarget.imports,
              underlyingFilePathTarget,
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
