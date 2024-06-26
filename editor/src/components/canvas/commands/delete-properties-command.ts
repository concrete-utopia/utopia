import type { JSXElement } from '../../../core/shared/element-template'
import type { EditorState, EditorStatePatch } from '../../../components/editor/store/editor-state'
import { modifyUnderlyingElementForOpenFile } from '../../../components/editor/store/editor-state'
import { foldEither } from '../../../core/shared/either'
import { unsetJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { patchParseSuccessAtElementPath } from './patch-utils'

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

  const editorStatePatch = patchParseSuccessAtElementPath(target, workingEditorState, (success) => {
    return {
      topLevelElements: {
        $set: success.topLevelElements,
      },
      imports: {
        $set: success.imports,
      },
    }
  })
  return {
    editorStateWithChanges: workingEditorState,
    editorStatePatch: editorStatePatch,
  }
}
