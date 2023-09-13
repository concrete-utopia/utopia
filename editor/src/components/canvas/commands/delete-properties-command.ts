import type { JSXElement } from '../../../core/shared/element-template'
import type {
  DerivedState,
  EditorState,
  EditorStatePatch,
} from '../../../components/editor/store/editor-state'
import {
  deriveState,
  modifyUnderlyingElementForOpenFile,
} from '../../../components/editor/store/editor-state'
import { foldEither } from '../../../core/shared/either'
import { unsetJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { patchParseSuccessAtElementPath } from './patch-utils'
import { patchedCreateRemixDerivedDataMemo } from '../../editor/store/remix-derived-data'

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
  derivedState: DerivedState,
  command: DeleteProperties,
) => {
  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = deleteValuesAtPath(
    editorState,
    derivedState,
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
  derivedState: DerivedState,
  target: ElementPath,
  properties: Array<PropertyPath>,
): { editorStateWithChanges: EditorState; editorStatePatch: EditorStatePatch } {
  const workingEditorState = modifyUnderlyingElementForOpenFile(
    target,
    editorState,
    derivedState,
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

  const workingDerivedState = deriveState(
    workingEditorState,
    derivedState,
    'patched',
    patchedCreateRemixDerivedDataMemo,
  )

  const editorStatePatch = patchParseSuccessAtElementPath(
    target,
    workingEditorState,
    workingDerivedState,
    (success) => {
      return {
        topLevelElements: {
          $set: success.topLevelElements,
        },
        imports: {
          $set: success.imports,
        },
      }
    },
  )
  return {
    editorStateWithChanges: workingEditorState,
    editorStatePatch: editorStatePatch,
  }
}
