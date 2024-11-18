import { notice } from '../../../components/common/notice'
import * as EP from '../../../core/shared/element-path'
import { roundTo } from '../../../core/shared/math-utils'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import type { CSSKeyword, CSSNumber, FlexDirection } from '../../inspector/common/css-utils'
import {
  cssPixelLength,
  printCSSNumber,
  printCSSNumberOrKeyword,
} from '../../inspector/common/css-utils'
import {
  deleteConflictingPropsForWidthHeight,
  type CreateIfNotExistant,
} from './adjust-css-length-command'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import { addToastPatch } from './show-toast-command'
import { getCSSNumberFromStyleInfo, maybeCssPropertyFromInlineStyle } from './utils/property-utils'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import type { StyleUpdate } from '../plugins/style-plugins'
import { getActivePlugin, runStyleUpdateForStrategy } from '../plugins/style-plugins'

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
  whenReplacingPercentageValues: 'do-not-warn' | 'warn-about-replacement'
}

export function setCssLengthProperty(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  value: CssNumberOrKeepOriginalUnit,
  parentFlexDirection: FlexDirection | null,
  createIfNonExistant: CreateIfNotExistant = 'create-if-not-existing', // TODO remove the default value and set it explicitly everywhere
  whenReplacingPercentageValues: SetCssLengthProperty['whenReplacingPercentageValues'] = 'do-not-warn',
): SetCssLengthProperty {
  return {
    type: 'SET_CSS_LENGTH_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    property: property,
    value: value,
    parentFlexDirection: parentFlexDirection,
    createIfNonExistant: createIfNonExistant,
    whenReplacingPercentageValues: whenReplacingPercentageValues,
  }
}

export const runSetCssLengthProperty = (
  editorState: EditorState,
  command: SetCssLengthProperty,
  interactionLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  // in case of width or height change, delete min, max and flex props
  const editorStateWithPropsDeleted = deleteConflictingPropsForWidthHeight(
    interactionLifecycle,
    editorState,
    command.target,
    [command.property],
    command.parentFlexDirection,
  )

  const styleInfo = getActivePlugin(editorStateWithPropsDeleted).styleInfoFactory({
    projectContents: editorStateWithPropsDeleted.projectContents,
  })(command.target)

  if (styleInfo == null) {
    return {
      editorStatePatches: [],
      commandDescription: `Set Css Length Prop: Element not found at ${EP.toUid(command.target)}`,
    }
  }

  const property = maybeCssPropertyFromInlineStyle(command.property)

  if (property == null) {
    return {
      editorStatePatches: [],
      commandDescription: `Set Css Length Prop: Element not found at ${EP.toUid(command.target)}`,
    }
  }

  const currentValue = getCSSNumberFromStyleInfo(styleInfo, property)
  if (
    currentValue.type === 'not-found' &&
    command.createIfNonExistant === 'do-not-create-if-doesnt-exist'
  ) {
    return {
      editorStatePatches: [],
      commandDescription: `Set Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property does not currently exist.`,
    }
  }

  let propsToUpdate: Array<StyleUpdate> = []

  let percentageValueWasReplaced: boolean = false
  const javascriptExpressionValueWasReplaced = currentValue.type === 'not-css-number'

  if (
    currentValue.type === 'css-number' &&
    currentValue.number.unit === '%' &&
    command.value.type === 'KEEP_ORIGINAL_UNIT' &&
    command.value.parentDimensionPx != null
  ) {
    const currentValuePercent = currentValue.number
    const valueInPercent = roundTo(
      (command.value.valuePx / command.value.parentDimensionPx) * 100,
      2,
    )
    const newValueCssNumber: CSSNumber = {
      value: valueInPercent,
      unit: currentValuePercent.unit,
    }
    const newValue = printCSSNumber(newValueCssNumber, null)

    propsToUpdate.push({
      type: 'set',
      property: property,
      value: newValue,
    })
  } else {
    const newCssValue =
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value
        : cssPixelLength(command.value.valuePx)

    if (
      command.whenReplacingPercentageValues === 'warn-about-replacement' &&
      currentValue.type === 'css-number' &&
      currentValue.number.unit === '%'
    ) {
      percentageValueWasReplaced = true
    }

    const printedValue = printCSSNumberOrKeyword(newCssValue, 'px')

    propsToUpdate.push({
      type: 'set',
      property: property,
      value: printedValue,
    })
  }

  // Apply the update to the properties.
  const { editorStatePatches } = runStyleUpdateForStrategy(
    interactionLifecycle,
    editorStateWithPropsDeleted,
    command.target,
    propsToUpdate,
  )

  // Always include the property update patch, but potentially also include a warning
  // that a percentage based property was replaced with a pixel based one.
  if (percentageValueWasReplaced) {
    editorStatePatches.push(
      addToastPatch(
        editorStateWithPropsDeleted.toasts,
        notice(
          'One or more percentage based style properties were replaced with a pixel based one.',
          'INFO',
          false,
          'percentage-pin-replaced',
        ),
      ),
    )
  }

  if (
    command.whenReplacingPercentageValues === 'warn-about-replacement' &&
    javascriptExpressionValueWasReplaced
  ) {
    editorStatePatches.push(
      addToastPatch(
        editorStateWithPropsDeleted.toasts,
        notice(
          `props.${PP.toString(command.property)} was replaced by a px value`,
          'INFO',
          false,
          'percentage-pin-replaced',
        ),
      ),
    )
  }

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Set Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )} by ${
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value.value
        : command.value.valuePx
    }`,
  }
}
