import { foldEither, isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  modifiableAttributeIsAttributeNotFound,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attribute-utils'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface UpdatePropIfExists extends BaseCommand {
  type: 'UPDATE_PROP_IF_EXISTS'
  element: ElementPath
  property: PropertyPath
  value: string
}

export function updatePropIfExists(
  whenToRun: WhenToRun,
  element: ElementPath,
  property: PropertyPath,
  value: string,
): UpdatePropIfExists {
  return {
    type: 'UPDATE_PROP_IF_EXISTS',
    whenToRun: whenToRun,
    element: element,
    property: property,
    value: value,
  }
}

export const runUpdatePropIfExists: CommandFunction<UpdatePropIfExists> = (
  editorState: EditorState,
  command: UpdatePropIfExists,
) => {
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

  if (propertyExists) {
    // Apply the update to the properties.
    const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
      editorState,
      command.element,
      [{ path: command.property, value: jsExpressionValue(command.value, emptyComments) }],
    )

    return {
      editorStatePatches: [propertyUpdatePatch],
      commandDescription: `Update Prop if Exists ${PP.toString(command.property)}=${JSON.stringify(
        command.property,
        null,
        2,
      )} on ${EP.toUid(command.element)}`,
    }
  } else {
    // no op return to prevent updating a nonexistant prop. if you want to set a prop regardless whether it exists or not, use the setPropertyCommand
    return {
      editorStatePatches: [],
      commandDescription: `Update Prop if Exists did not find existing prop for ${PP.toString(
        command.property,
      )}`,
    }
  }
}
