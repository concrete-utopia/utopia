import fastDeepEquals from 'fast-deep-equal'
import { Patch, produceWithPatches } from 'immer'
import { Spec } from 'immutability-helper'
import { drop } from '../../../core/shared/array-utils'
import { foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  isJSXElement,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import {
  getNumberPropertyFromProps,
  setJSXValuesAtPaths,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import {
  ElementPath,
  ParseSuccess,
  PropertyPath,
  RevisionsState,
  TextFile,
} from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import {
  getProjectContentKeyPathElements,
  ProjectContentDirectory,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import { ConstraintsControls } from '../controls/constraints-control'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface AdjustNumberProperty extends BaseCommand {
  type: 'ADJUST_NUMBER_PROPERTY'
  target: ElementPath
  property: PropertyPath
  value: number | AdjustNumberInequalityCondition
  createIfNonExistant: boolean
}

export function adjustNumberProperty(
  transient: TransientOrNot,
  target: ElementPath,
  property: PropertyPath,
  value: number | AdjustNumberInequalityCondition,
  createIfNonExistant: boolean,
): AdjustNumberProperty {
  return {
    type: 'ADJUST_NUMBER_PROPERTY',
    transient: transient,
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
      editorStatePatch: [],
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
                editorStatePatch: [],
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
                editorStatePatch: [],
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
        value: jsxAttributeValue(newValue, emptyComments),
      },
    ]

    // Apply the update to the properties.
    const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
      editorState,
      command.target,
      propsToUpdate,
    )

    return {
      editorStatePatch: propertyUpdatePatch,
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
): { editorStateWithChanges: EditorState; editorStatePatch: Array<Patch> } {
  let editorStatePatch: EditorStatePatch = {}

  const workingEditorState = modifyUnderlyingForOpenFile(
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
        setJSXValuesAtPaths(element.props, jsxValuesAndPathsToSet),
      )
    },
  )

  const patches: Array<Patch> = withUnderlyingTargetFromEditorState(
    target,
    workingEditorState,
    [],
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      // ProjectContentTreeRoot is a bit awkward to patch.
      const pathElements = getProjectContentKeyPathElements(underlyingFilePath)
      const pathElementsWithChildren = pathElements.flatMap((value, index, array) =>
        array.length - 1 !== index // check for the last item
          ? [value, 'children']
          : value,
      )

      if (pathElements.length === 0) {
        throw new Error('Invalid path length.')
      }
      const immerPatches: Array<Patch> = [
        {
          op: 'replace',
          path: [
            'projectContents',
            ...pathElementsWithChildren,
            'content',
            'fileContents',
            'revisionsState',
          ],
          value: RevisionsState.ParsedAhead,
        },
        {
          op: 'replace',
          path: [
            'projectContents',
            ...pathElementsWithChildren,
            'content',
            'fileContents',
            'parsed',
            'topLevelElements',
          ],
          value: success.topLevelElements,
        },
        {
          op: 'replace',
          path: [
            'projectContents',
            ...pathElementsWithChildren,
            'content',
            'fileContents',
            'parsed',
            'imports',
          ],
          value: success.imports,
        },
      ]

      return immerPatches
    },
  )

  return {
    editorStateWithChanges: workingEditorState,
    editorStatePatch: patches,
  }
}
