import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type {
  ElementPath,
  PropertyPath,
  PropertyPathPart,
} from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { runStyleUpdateForStrategy } from '../plugins/style-plugins'
import * as SU from '../plugins/style-plugins'
import type { BaseCommand, CommandFunction, CommandFunctionResult, WhenToRun } from './commands'
import {
  applyValuesAtPath,
  deleteValuesAtPath,
  maybeCssPropertyFromInlineStyle,
} from './utils/property-utils'

type PositionProp = 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'

export interface SetProperty extends BaseCommand {
  type: 'SET_PROPERTY'
  element: ElementPath
  property: PropertyPath
  value: string | number
}
export type PropertyToUpdate = PropertyToSet | PropertyToDelete
type PropertyToSet = { type: 'SET'; path: PropertyPath; value: string | number }
type PropertyToDelete = { type: 'DELETE'; path: PropertyPath }
export function propertyToSet(path: PropertyPath, value: string | number): PropertyToUpdate {
  return {
    type: 'SET',
    path: path,
    value: value,
  }
}
export function propertyToDelete(path: PropertyPath): PropertyToUpdate {
  return {
    type: 'DELETE',
    path: path,
  }
}
export interface UpdateBulkProperties extends BaseCommand {
  type: 'UPDATE_BULK_PROPERTIES'
  element: ElementPath
  properties: PropertyToUpdate[]
}
export function setProperty<T extends PropertyPathPart>(
  whenToRun: WhenToRun,
  element: ElementPath,
  property: PropertyPath<[PropertyPathPart, T extends PositionProp ? never : T]>,
  value: string | number,
): SetProperty {
  return {
    type: 'SET_PROPERTY',
    whenToRun: whenToRun,
    element: element,
    property: property,
    value: value,
  }
}
export function updateBulkProperties(
  whenToRun: WhenToRun,
  element: ElementPath,
  properties: PropertyToUpdate[],
): UpdateBulkProperties {
  return {
    type: 'UPDATE_BULK_PROPERTIES',
    whenToRun: whenToRun,
    element: element,
    properties: properties,
  }
}
export function setPropertyOmitNullProp<T extends PropertyPathPart>(
  whenToRun: WhenToRun,
  element: ElementPath,
  property: PropertyPath<[PropertyPathPart, T extends PositionProp ? never : T]>,
  value: string | number | null,
): [SetProperty] | [] {
  if (value == null) {
    return []
  } else {
    return [setProperty(whenToRun, element, property, value)]
  }
}

function getCommandDescription(command: SetProperty) {
  return `Set Property ${PP.toString(command.property)}=${JSON.stringify(
    command.property,
    null,
    2,
  )} on ${EP.toUid(command.element)}`
}

export const runSetProperty = (
  editorState: EditorState,
  command: SetProperty,
  interactionLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  const prop = maybeCssPropertyFromInlineStyle(command.property)
  if (prop == null) {
    const { editorStatePatch } = applyValuesAtPath(editorState, command.element, [
      { path: command.property, value: jsExpressionValue(command.value, emptyComments) },
    ])
    return {
      editorStatePatches: [editorStatePatch],
      commandDescription: getCommandDescription(command),
    }
  }

  const { editorStatePatches } = runStyleUpdateForStrategy(
    interactionLifecycle,
    editorState,
    command.element,
    [SU.updateCSSProp(prop, command.value)],
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: getCommandDescription(command),
  }
}

export const runBulkUpdateProperties: CommandFunction<UpdateBulkProperties> = (
  editorState: EditorState,
  command: UpdateBulkProperties,
) => {
  // 1. Apply DELETE updates
  const propsToDelete: PropertyToDelete[] = mapDropNulls(
    (p) => (p.type === 'DELETE' ? p : null),
    command.properties,
  )
  const withDeletedProps = deleteValuesAtPath(
    editorState,
    command.element,
    propsToDelete.map((d) => d.path),
  )

  // 2. Apply SET updates
  const propsToSet: PropertyToSet[] = mapDropNulls(
    (p) => (p.type === 'SET' ? p : null),
    command.properties,
  )
  const withSetProps = applyValuesAtPath(
    withDeletedProps.editorStateWithChanges,
    command.element,
    propsToSet.map((property) => ({
      path: property.path,
      value: jsExpressionValue(property.value, emptyComments),
    })),
  )

  // Final patch
  const patch = withSetProps.editorStatePatch

  return {
    editorStatePatches: [patch],
    commandDescription: `Set Properties ${command.properties.map((property) =>
      PP.toString(property.path),
    )}=${JSON.stringify(command.properties, null, 2)} on ${EP.toUid(command.element)}`,
  }
}
