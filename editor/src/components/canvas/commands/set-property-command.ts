import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type {
  ElementPath,
  PropertyPath,
  PropertyPathPart,
} from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

type PositionProp = 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'

export interface SetProperty extends BaseCommand {
  type: 'SET_PROPERTY'
  element: ElementPath
  property: PropertyPath
  value: string | number
}

export function setProperty<T extends PropertyPathPart>(
  whenToRun: WhenToRun,
  element: ElementPath,
  property: PropertyPath,
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

export function setPropertyOmitNullProp<T extends PropertyPathPart>(
  whenToRun: WhenToRun,
  element: ElementPath,
  property: PropertyPath,
  value: string | number | null,
): [SetProperty] | [] {
  if (value == null) {
    return []
  } else {
    return [setProperty(whenToRun, element, property, value)]
  }
}

export const runSetProperty: CommandFunction<SetProperty> = (
  editorState: EditorState,
  command: SetProperty,
) => {
  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.element,
    [{ path: command.property, value: jsExpressionValue(command.value, emptyComments) }],
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Set Property ${PP.toString(command.property)}=${JSON.stringify(
      command.property,
      null,
      2,
    )} on ${EP.toUid(command.element)}`,
  }
}
