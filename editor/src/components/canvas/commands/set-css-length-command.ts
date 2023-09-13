import { isLeft, isRight, left } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import type { GetModifiableAttributeResult, ValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attributes'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { DerivedState, EditorState } from '../../editor/store/editor-state'
import { deriveState, withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { patchedCreateRemixDerivedDataMemo } from '../../editor/store/remix-derived-data'
import type { CSSKeyword, CSSNumber, FlexDirection } from '../../inspector/common/css-utils'
import {
  cssPixelLength,
  parseCSSPercent,
  printCSSNumber,
  printCSSNumberOrKeyword,
} from '../../inspector/common/css-utils'
import type { CreateIfNotExistant } from './adjust-css-length-command'
import { deleteConflictingPropsForWidthHeight } from './adjust-css-length-command'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

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
  createIfNonExistant: CreateIfNotExistant
}

export function setCssLengthProperty(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  value: CssNumberOrKeepOriginalUnit,
  parentFlexDirection: FlexDirection | null,
  createIfNonExistant: CreateIfNotExistant = 'create-if-not-existing', // TODO remove the default value and set it explicitly everywhere
): SetCssLengthProperty {
  return {
    type: 'SET_CSS_LENGTH_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    property: property,
    value: value,
    parentFlexDirection: parentFlexDirection,
    createIfNonExistant: createIfNonExistant,
  }
}

export const runSetCssLengthProperty: CommandFunction<SetCssLengthProperty> = (
  editorState: EditorState,
  derivedState: DerivedState,
  command: SetCssLengthProperty,
) => {
  // in case of width or height change, delete min, max and flex props
  const editorStateWithPropsDeleted = deleteConflictingPropsForWidthHeight(
    editorState,
    derivedState,
    command.target,
    command.property,
    command.parentFlexDirection,
  )

  // Identify the current value, whatever that may be.
  const currentValue: GetModifiableAttributeResult = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    derivedState,
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

  const targetPropertyNonExistant: boolean = currentModifiableValue.type === 'ATTRIBUTE_NOT_FOUND'
  if (
    targetPropertyNonExistant &&
    command.createIfNonExistant === 'do-not-create-if-doesnt-exist'
  ) {
    return {
      editorStatePatches: [],
      commandDescription: `Set Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property does not currently exist.`,
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
      value: jsExpressionValue(newValue, emptyComments),
    })
  } else {
    const newCssValue =
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value
        : cssPixelLength(command.value.valuePx)

    const printedValue = printCSSNumberOrKeyword(newCssValue, 'px')

    propsToUpdate.push({
      path: command.property,
      value: jsExpressionValue(printedValue, emptyComments),
    })
  }

  const derivedStateWithPropsDeleted = deriveState(
    editorStateWithPropsDeleted,
    derivedState,
    'patched',
    patchedCreateRemixDerivedDataMemo,
  )

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    derivedStateWithPropsDeleted,
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
