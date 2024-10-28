import type { EditorState } from '../../../components/editor/store/editor-state'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { deleteValuesAtPath } from './utils/property-utils'

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
