import { Spec } from 'immutability-helper'
import { JSXElement } from '../../../core/shared/element-template'
import {
  ProjectContentFile,
  getProjectContentKeyPathElements,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../../components/assets'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingElementForOpenFile,
} from '../../../components/editor/store/editor-state'
import { drop } from '../../../core/shared/array-utils'
import { foldEither } from '../../../core/shared/either'
import { unsetJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import { ElementPath, PropertyPath, RevisionsState } from '../../../core/shared/project-file-types'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'

export interface DeleteProperties extends BaseCommand {
  type: 'DELETE_PROPERTIES'
  element: ElementPath
  properties: Array<PropertyPath>
}

export function deleteProperties(
  whenToRun: WhenToRun,
  element: ElementPath,
  properties: Array<PropertyPath>,
): DeleteProperties {
  return {
    type: 'DELETE_PROPERTIES',
    whenToRun: whenToRun,
    element: element,
    properties: properties,
  }
}

export const runDeleteProperties: CommandFunction<DeleteProperties> = (
  editorState: EditorState,
  command: DeleteProperties,
) => {
  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = deleteValuesAtPath(
    editorState,
    command.element,
    command.properties,
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Delete Properties ${command.properties
      .map(PP.toString)
      .join(',')} on ${EP.toUid(command.element)}`,
  }
}

export function deleteValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  properties: Array<PropertyPath>,
): { editorStateWithChanges: EditorState; editorStatePatch: EditorStatePatch } {
  let editorStatePatch: EditorStatePatch = {}

  const workingEditorState = modifyUnderlyingElementForOpenFile(
    target,
    editorState,
    (element: JSXElement) => {
      return foldEither(
        () => {
          return element
        },
        (updatedProps) => {
          return {
            ...element,
            props: updatedProps,
          }
        },
        unsetJSXValuesAtPaths(element.props, properties),
      )
    },
  )

  forUnderlyingTargetFromEditorState(
    target,
    workingEditorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const projectContentFilePatch: Spec<ProjectContentFile> = {
        content: {
          fileContents: {
            revisionsState: {
              $set: RevisionsState.ParsedAhead,
            },
            parsed: {
              topLevelElements: {
                $set: success.topLevelElements,
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
    editorStateWithChanges: workingEditorState,
    editorStatePatch: editorStatePatch,
  }
}
