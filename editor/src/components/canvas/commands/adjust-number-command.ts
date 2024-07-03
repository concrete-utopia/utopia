import { foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type { JSXElement } from '../../../core/shared/element-template'
import {
  emptyComments,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  getNumberPropertyFromProps,
  setJSXValuesAtPaths,
} from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  modifyUnderlyingElementForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { patchParseSuccessAtElementPath } from './patch-utils'

export interface AdjustNumberProperty extends BaseCommand {
  type: 'ADJUST_NUMBER_PROPERTY'
  target: ElementPath
  property: PropertyPath
  value: number | AdjustNumberInequalityCondition
  createIfNonExistant: boolean
}

export function adjustNumberProperty(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  value: number | AdjustNumberInequalityCondition,
  createIfNonExistant: boolean,
): AdjustNumberProperty {
  return {
    type: 'ADJUST_NUMBER_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    property: property,
    value: value,
    createIfNonExistant: createIfNonExistant,
  }
}

export type AdjustNumberCondition = 'less-than' | 'greater-than'

export interface AdjustNumberInequalityCondition {
  property: PropertyPath
  condition: AdjustNumberCondition
}

export function adjustNumberInequalityCondition(
  property: PropertyPath,
  condition: AdjustNumberCondition,
): AdjustNumberInequalityCondition {
  return {
    property: property,
    condition: condition,
  }
}

export const runAdjustNumberProperty: CommandFunction<AdjustNumberProperty> = (
  editorState: EditorState,
  command: AdjustNumberProperty,
) => {
  // Handle updating the existing value, treating a value that can't be parsed
  // as zero.
  let newValue: number = 0

  // Identify the current value, whatever that may be.
  let targetPropertyNonExistant: boolean = false
  let inequalityValue: number | null = null
  const currentValue = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    null,
    (success, element, underlyingTarget, underlyingFilePath) => {
      if (isJSXElement(element)) {
        // Check for the inequality adjustment target while we're here.
        if (typeof command.value !== 'number') {
          inequalityValue = getNumberPropertyFromProps(element.props, command.value.property)
        }

        // Try the property we're updating.
        const fromProperty = getNumberPropertyFromProps(element.props, command.property)
        if (fromProperty == null) {
          targetPropertyNonExistant = true
        } else {
          return fromProperty
        }
      }
      return null
    },
  )

  if (targetPropertyNonExistant && !command.createIfNonExistant) {
    return {
      editorStatePatches: [],
      commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property does not exist.`,
    }
  } else {
    if (typeof command.value === 'number') {
      if (currentValue != null) {
        newValue += currentValue
      }

      // Change the value.
      newValue += command.value
    } else {
      if (currentValue != null && inequalityValue != null) {
        switch (command.value.condition) {
          case 'less-than':
            if (inequalityValue <= currentValue) {
              return {
                editorStatePatches: [],
                commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
                  command.property,
                )} not applied as value is large enough already.`,
              }
            } else {
              newValue = inequalityValue
            }
            break
          case 'greater-than':
            if (inequalityValue >= currentValue) {
              return {
                editorStatePatches: [],
                commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
                  command.property,
                )} not applied as value is small enough already.`,
              }
            } else {
              newValue = inequalityValue
            }
            break
          default:
            const _exhaustiveCheck: never = command.value.condition
            throw new Error(`Unhandled command condition of ${JSON.stringify(command.value)}`)
        }
      }
    }

    const propsToUpdate: Array<ValueAtPath> = [
      {
        path: command.property,
        value: jsExpressionValue(newValue, emptyComments),
      },
    ]

    // Apply the update to the properties.
    const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
      editorState,
      command.target,
      propsToUpdate,
    )

    return {
      editorStatePatches: [propertyUpdatePatch],
      commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} by ${command.value}`,
    }
  }
}

export function applyValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
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
        setJSXValuesAtPaths(element.props, jsxValuesAndPathsToSet, 'include-in-printing'),
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
