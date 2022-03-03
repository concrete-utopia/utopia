import { isLeft, left } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import {
  GetModifiableAttributeResult,
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { EditorState, withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { CSSNumber, parseCSSPx, printCSSNumber } from '../../inspector/common/css-utils'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, PathMappings, TransientOrNot } from './commands'

export interface AdjustPxNumberProperty extends BaseCommand {
  type: 'ADJUST_PX_NUMBER_PROPERTY'
  target: ElementPath
  property: PropertyPath
  valuePx: number
  createIfNonExistant: boolean
}

export function adjustPxNumberProperty(
  transient: TransientOrNot,
  target: ElementPath,
  property: PropertyPath,
  valuePx: number,
  createIfNonExistant: boolean,
): AdjustPxNumberProperty {
  return {
    type: 'ADJUST_PX_NUMBER_PROPERTY',
    transient: transient,
    target: target,
    property: property,
    valuePx: valuePx,
    createIfNonExistant: createIfNonExistant,
  }
}

export const runAdjustPxNumberProperty: CommandFunction<AdjustPxNumberProperty> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: AdjustPxNumberProperty,
) => {
  // Identify the current value, whatever that may be.
  const currentValue: GetModifiableAttributeResult = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    left(`no target element was found at path ${EP.toString(command.target)}`),
    (_, element) => {
      return getModifiableJSXAttributeAtPath(element.props, command.property)
    },
  )
  if (isLeft(currentValue)) {
    return {
      editorStatePatch: {},
      pathMappings: pathMappings,
      commandDescription: `Adjust Px Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as value is not writeable.`,
    }
  }
  const currentModifiableValue = currentValue.value
  const simpleValueResult = jsxSimpleAttributeToValue(currentModifiableValue)
  const valueProbablyExpression = isLeft(simpleValueResult)
  const targetPropertyNonExistant: boolean = currentModifiableValue.type === 'ATTRIBUTE_NOT_FOUND'

  if (targetPropertyNonExistant && !command.createIfNonExistant) {
    return {
      editorStatePatch: {},
      pathMappings: pathMappings,
      commandDescription: `Adjust Px Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property does not exist.`,
    }
  }

  if (valueProbablyExpression) {
    // TODO add option to override expressions!!!
    return {
      editorStatePatch: {},
      pathMappings: pathMappings,
      commandDescription: `Adjust Px Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property is an expression we did not want to override.`,
    }
  }

  const parseCssResult = parseCSSPx(simpleValueResult.value)

  if (isLeft(parseCssResult)) {
    return {
      editorStatePatch: {},
      pathMappings: pathMappings,
      commandDescription: `Adjust Px Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property is in a CSS unit we do not support. (${
        simpleValueResult.value
      })`,
    }
  }

  const currentValuePx = parseCssResult.value.value
  const newValueCssNumber: CSSNumber = {
    value: currentValuePx + command.valuePx,
    unit: parseCssResult.value.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

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
    pathMappings: pathMappings,
    commandDescription: `Adjust Px Number Prop: ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )} by ${command.valuePx}`,
  }
}
