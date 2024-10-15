import {
  emptyComments,
  jsExpressionValue,
  type JSXElement,
} from '../../../core/shared/element-template'
import type {
  EditorState,
  EditorStatePatch,
  UnsetPropertyValues,
} from '../../../components/editor/store/editor-state'
import { modifyUnderlyingElementForOpenFile } from '../../../components/editor/store/editor-state'
import { foldEither } from '../../../core/shared/either'
import { unsetJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { patchParseSuccessAtElementPath } from './patch-utils'
import { mapDropNulls, stripNulls } from '../../../core/shared/array-utils'
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
  prop: keyof UnsetPropertyValues
  value: UnsetPropertyValues[keyof UnsetPropertyValues]
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

function getPropertiesToUnset(propertiesToUnset: PropertiesToUnsetArray): UnsetPropertyValues {
  let result: UnsetPropertyValues = {}
  for (const { prop, value } of propertiesToUnset) {
    result[prop] = value
  }
  return result
}

function getPropertiesToUnsetPatches(
  editorState: EditorState,
  command: DeleteProperties,
): EditorStatePatch[] {
  const unsetProperties = getUnsetProperties(command.properties)
  const partialPropertiesToUnset = getPropertiesToUnset(unsetProperties)
  const pathString = EP.toString(command.element)
  const unsetPropertiesPatch: EditorStatePatch = {
    canvas: {
      propertiesToUnset: {
        $set: {
          ...editorState.canvas.propertiesToUnset,
          [pathString]: {
            ...editorState.canvas.propertiesToUnset[pathString],
            ...partialPropertiesToUnset,
          },
        },
      },
    },
  }

  if (command.whenToRun === 'on-complete') {
    return [unsetPropertiesPatch]
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
  const result = deleteValuesAtPath(editorState, command.element, command.properties)

  const updatedEditorState = result == null ? editorState : result.editorStateWithChanges
  const propertiesToUnsetPatches = getPropertiesToUnsetPatches(updatedEditorState, command)

  return {
    editorStatePatches: stripNulls([result?.editorStatePatch, ...propertiesToUnsetPatches]),
    commandDescription: `Delete Properties ${command.properties
      .map(PP.toString)
      .join(',')} on ${EP.toUid(command.element)}`,
  }
}

export function deleteValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  properties: Array<PropertyPath>,
): { editorStateWithChanges: EditorState; editorStatePatch: EditorStatePatch } | null {
  try {
    return deleteValuesAtPathUnsafe(editorState, target, properties)
  } catch {
    return null
  }
}

// This function is unsafe, because it calls
// `transformJSXComponentAtElementPath` internally, and
// `transformJSXComponentAtElementPath` throws an error if it cannot find an
// element at the element path passed to it
function deleteValuesAtPathUnsafe(
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
