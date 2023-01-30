import { isLeft, isRight, left } from '../../../core/shared/either'
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
import {
  CSSNumber,
  cssPixelLength,
  FlexDirection,
  parseCSSPercent,
  printCSSNumber,
} from '../../inspector/common/css-utils'
import { deleteConflictingPropsForWidthHeight } from './adjust-css-length-command'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'

type CssNumberOrKeepOriginalUnit =
  | { type: 'EXPLICIT_CSS_NUMBER'; value: CSSNumber }
  | { type: 'KEEP_ORIGINAL_UNIT'; valuePx: number; parentDimensionPx: number | undefined }

export function setExplicitCssValue(value: CSSNumber): CssNumberOrKeepOriginalUnit {
  return { type: 'EXPLICIT_CSS_NUMBER', value: value }
}

export function setValueKeepingOriginalUnit(
  valuePx: number,
  parentDimensionPx: number | undefined,
): CssNumberOrKeepOriginalUnit {
  return { type: 'KEEP_ORIGINAL_UNIT', valuePx: valuePx, parentDimensionPx: parentDimensionPx }
}

export interface SetCssLengthProperty extends BaseCommand {
  type: 'SET_CSS_LENGTH_PROPERTY'
  target: ElementPath
  property: PropertyPath
  value: CssNumberOrKeepOriginalUnit
  parentFlexDirection: FlexDirection | null
}

export function setCssLengthProperty(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  value: CssNumberOrKeepOriginalUnit,
  parentFlexDirection: FlexDirection | null,
): SetCssLengthProperty {
  return {
    type: 'SET_CSS_LENGTH_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    property: property,
    value: value,
    parentFlexDirection: parentFlexDirection,
  }
}

export const runSetCssLengthProperty: CommandFunction<SetCssLengthProperty> = (
  editorState: EditorState,
  command: SetCssLengthProperty,
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
      editorStatePatches: [],
      commandDescription: `Set Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as value is not writeable.`,
    }
  }
  const currentModifiableValue = currentValue.value
  const simpleValueResult = jsxSimpleAttributeToValue(currentModifiableValue)
  const valueProbablyExpression = isLeft(simpleValueResult)
  if (valueProbablyExpression) {
    // TODO add option to override expressions!!!
    return {
      editorStatePatches: [],
      commandDescription: `Set Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property is an expression we did not want to override.`,
    }
  }

  let newValue: CSSNumber

  const parsePercentResult = parseCSSPercent(simpleValueResult.value)
  if (
    isRight(parsePercentResult) &&
    command.value.type === 'KEEP_ORIGINAL_UNIT' &&
    command.value.parentDimensionPx != null
  ) {
    const currentValuePercent = parsePercentResult.value
    const valueInPercent = (command.value.valuePx / command.value.parentDimensionPx) * 100
    newValue = {
      value: valueInPercent,
      unit: currentValuePercent.unit,
    }
  } else {
    newValue =
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value
        : cssPixelLength(command.value.valuePx)
  }

  const propToUpdate: ValueAtPath = {
    path: command.property,
    value: jsxAttributeValue(printCSSNumber(newValue, 'px'), emptyComments),
  }

  // in case of width or height change, delete min, max and flex props
  const editorStateWithPropsDeleted = deleteConflictingPropsForWidthHeight(
    editorState,
    command.target,
    newValue,
    command.property,
    command.parentFlexDirection,
  )

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    command.target,
    [propToUpdate],
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Set Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )} by ${
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value.value
        : command.value.valuePx
    }`,
  }
}
