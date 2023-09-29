import { foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  modifiableAttributeIsAttributeNotFound,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { DerivedState, EditorState } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import { runShowToastCommand, showToastCommand } from './show-toast-command'

export interface AddPropIfDoesntExist extends BaseCommand {
  type: 'ADD_PROP_IF_DOESNT_EXIST'
  element: ElementPath
  property: PropertyPath
  value: string
  toastMessage: string | null
}

export function addPropIfDoesntExist(
  whenToRun: WhenToRun,
  element: ElementPath,
  property: PropertyPath,
  value: string,
  toastMessage: string | null,
): AddPropIfDoesntExist {
  return {
    type: 'ADD_PROP_IF_DOESNT_EXIST',
    whenToRun: whenToRun,
    element: element,
    property: property,
    value: value,
    toastMessage: toastMessage,
  }
}

export const runAddPropIfDoesntExist = (
  editorState: EditorState,
  _derivedState: DerivedState,
  command: AddPropIfDoesntExist,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  // check if the prop exists
  const propertyExists = withUnderlyingTargetFromEditorState(
    command.element,
    editorState,
    false,
    (_, element) => {
      if (isJSXElement(element)) {
        return foldEither(
          () => false,
          (value) => !modifiableAttributeIsAttributeNotFound(value),
          getModifiableJSXAttributeAtPath(element.props, command.property),
        )
      } else {
        return false
      }
    },
  )

  if (!propertyExists) {
    const showToastPatches =
      command.toastMessage != null
        ? runShowToastCommand(
            editorState,
            showToastCommand(command.toastMessage, 'NOTICE', 'property-added'),
            commandLifecycle,
          ).editorStatePatches
        : []

    // Apply the update to the properties.
    const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
      editorState,
      command.element,
      [{ path: command.property, value: jsExpressionValue(command.value, emptyComments) }],
    )

    return {
      editorStatePatches: [propertyUpdatePatch, ...showToastPatches],
      commandDescription: `Add Prop if doesn't Exist ${PP.toString(
        command.property,
      )}=${JSON.stringify(command.property, null, 2)} on ${EP.toUid(command.element)}`,
    }
  } else {
    // no op return to prevent overwriting an existing prop. if you want to set a prop regardless whether it exists or not, use the setPropertyCommand
    return {
      editorStatePatches: [],
      commandDescription: `Add Prop if doesn't Exist found existing prop for ${PP.toString(
        command.property,
      )}`,
    }
  }
}
