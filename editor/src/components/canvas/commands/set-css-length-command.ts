import { isLeft, isRight, left } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  isJSXElement,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
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
  CSSKeyword,
  CSSNumber,
  cssPixelLength,
  FlexDirection,
  parseCSSPercent,
  printCSSNumber,
  printCSSNumberOrKeyword,
} from '../../inspector/common/css-utils'
import { deleteConflictingPropsForWidthHeight } from './adjust-css-length-command'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'

type CssNumberOrKeepOriginalUnit =
  | { type: 'EXPLICIT_CSS_NUMBER'; value: CSSNumber | CSSKeyword }
  | { type: 'KEEP_ORIGINAL_UNIT'; valuePx: number; parentDimensionPx: number | undefined }

export function setExplicitCssValue(value: CSSNumber | CSSKeyword): CssNumberOrKeepOriginalUnit {
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
  // in case of width or height change, delete min, max and flex props
  const editorStateWithPropsDeleted = deleteConflictingPropsForWidthHeight(
    editorState,
    command.target,
    command.property,
    command.parentFlexDirection,
  )

  // Identify the current value, whatever that may be.
  const currentValue: GetModifiableAttributeResult = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    left(`no target element was found at path ${EP.toString(command.target)}`),
    (_, element) => {
      if (isJSXElement(element)) {
        return getModifiableJSXAttributeAtPath(element.props, command.property)
      } else {
        return left(`No JSXElement was found at path ${EP.toString(command.target)}`)
      }
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

  let propsToUpdate: Array<ValueAtPath> = []

  const parsePercentResult = parseCSSPercent(simpleValueResult.value)
  if (
    isRight(parsePercentResult) &&
    command.value.type === 'KEEP_ORIGINAL_UNIT' &&
    command.value.parentDimensionPx != null
  ) {
    const currentValuePercent = parsePercentResult.value
    const valueInPercent = (command.value.valuePx / command.value.parentDimensionPx) * 100
    const newValueCssNumber: CSSNumber = {
      value: valueInPercent,
      unit: currentValuePercent.unit,
    }
    const newValue = printCSSNumber(newValueCssNumber, null)

    propsToUpdate.push({
      path: command.property,
      value: jsxAttributeValue(newValue, emptyComments),
    })
  } else {
    const newCssValue =
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value
        : cssPixelLength(command.value.valuePx)

    const printedValue = printCSSNumberOrKeyword(newCssValue, 'px')

    propsToUpdate.push({
      path: command.property,
      value: jsxAttributeValue(printedValue, emptyComments),
    })
  }

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    command.target,
    propsToUpdate,
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
