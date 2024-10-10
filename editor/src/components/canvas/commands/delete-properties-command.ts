import {
  emptyComments,
  jsExpressionValue,
  type JSXElement,
} from '../../../core/shared/element-template'
import type {
  EditorState,
  EditorStatePatch,
  PropertiesToUnset,
} from '../../../components/editor/store/editor-state'
import { modifyUnderlyingElementForOpenFile } from '../../../components/editor/store/editor-state'
import { foldEither } from '../../../core/shared/either'
import { unsetJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { patchParseSuccessAtElementPath } from './patch-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { applyValuesAtPath } from './adjust-number-command'

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

type PropertiesToUnsetArray = Array<{
  prop: keyof PropertiesToUnset
  value: PropertiesToUnset[keyof PropertiesToUnset]
  path: PropertyPath
}>

function getUnsetProperties(properties: Array<PropertyPath>): PropertiesToUnsetArray {
  return mapDropNulls((property) => {
    if (property.propertyElements.at(0) !== 'style') {
      return null
    }

    switch (property.propertyElements.at(1)) {
      case 'gap':
        return { prop: 'gap', value: '0px', path: property }
      default:
        return null
    }
  }, properties)
}

function getPropertiesToUnset(propertiesToUnset: PropertiesToUnsetArray): PropertiesToUnset {
  let result: Partial<PropertiesToUnset> = {}
  for (const { prop, value } of propertiesToUnset) {
    result[prop] = value
  }
  return result
}

function getPropertiesToUnsetPatches(
  editorState: EditorState,
  command: DeleteProperties,
): EditorStatePatch[] {
  if (command.whenToRun === 'on-complete') {
    return []
  }

  const unsetProperties = getUnsetProperties(command.properties)
  const partialPropertiesToUnset = getPropertiesToUnset(unsetProperties)
  const unsetPropertiesPatch: EditorStatePatch = {
    canvas: { propertiesToUnset: { $set: partialPropertiesToUnset } },
  }
  const { editorStatePatch: setPropertiesToUnsetValuePatch } = applyValuesAtPath(
    editorState,
    command.element,
    unsetProperties.map(({ path, value }) => ({
      path: path,
      value: jsExpressionValue(value, emptyComments),
    })),
  )
  return [unsetPropertiesPatch, setPropertiesToUnsetValuePatch]
}

export const runDeleteProperties = (
  editorState: EditorState,
  command: DeleteProperties,
): CommandFunctionResult => {
  const { editorStatePatch: propertyUpdatePatch, editorStateWithChanges: editorStateWithChanges } =
    deleteValuesAtPath(editorState, command.element, command.properties)

  const propertiesToUnsetPatches = getPropertiesToUnsetPatches(editorStateWithChanges, command)

  return {
    editorStatePatches: [propertyUpdatePatch, ...propertiesToUnsetPatches],
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
