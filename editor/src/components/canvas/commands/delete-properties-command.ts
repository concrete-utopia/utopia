import type { EditorState } from '../../../components/editor/store/editor-state'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import type { BaseCommand, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { deleteValuesAtPath, maybeCssPropertyFromInlineStyle } from './utils/property-utils'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { runStyleUpdateForStrategy } from '../plugins/style-plugins'
import * as SU from '../plugins/style-plugins'

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

export const runDeleteProperties = (
  editorState: EditorState,
  command: DeleteProperties,
  interactionLifecycle: InteractionLifecycle,
) => {
  const propsToDelete = command.properties.reduce(
    (acc: { styleProps: string[]; rest: PropertyPath[] }, p) => {
      const prop = maybeCssPropertyFromInlineStyle(p)
      if (prop == null) {
        acc.rest.push(p)
      } else {
        acc.styleProps.push(prop)
      }
      return acc
    },
    { styleProps: [], rest: [] },
  )

  // Apply the update to the properties.
  const { editorStateWithChanges } = deleteValuesAtPath(
    editorState,
    command.element,
    propsToDelete.rest,
  )

  const { editorStatePatches } = runStyleUpdateForStrategy(
    interactionLifecycle,
    editorStateWithChanges,
    command.element,
    propsToDelete.styleProps.map(SU.deleteCSSProp),
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Delete Properties ${command.properties
      .map(PP.toString)
      .join(',')} on ${EP.toUid(command.element)}`,
  }
}
