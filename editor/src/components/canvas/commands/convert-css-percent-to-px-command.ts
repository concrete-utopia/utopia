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
} from '../../../core/shared/jsx-attribute-utils'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import type { CSSNumber } from '../../inspector/common/css-utils'
import { parseCSSPercent, printCSSNumber } from '../../inspector/common/css-utils'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { applyValuesAtPath } from './utils/property-utils'

export interface ConvertCssPercentToPx extends BaseCommand {
  type: 'CONVERT_CSS_PERCENT_TO_PX'
  target: ElementPath
  property: PropertyPath
  parentDimensionPx: number
}

export function convertCssPercentToPx(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  parentDimensionPx: number,
): ConvertCssPercentToPx {
  return {
    type: 'CONVERT_CSS_PERCENT_TO_PX',
    whenToRun: whenToRun,
    target: target,
    property: property,
    parentDimensionPx: parentDimensionPx,
  }
}

export const runConvertCssPercentToPx: CommandFunction<ConvertCssPercentToPx> = (
  editorState: EditorState,
  command: ConvertCssPercentToPx,
) => {
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
      commandDescription: `Convert CSS percent: ${EP.toUid(command.target)}/${PP.toString(
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
      commandDescription: `Convert CSS percent to px: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property is an expression we did not want to override.`,
    }
  }

  let propsToUpdate: Array<ValueAtPath> = []

  const parsePercentResult = parseCSSPercent(simpleValueResult.value)
  if (isLeft(parsePercentResult)) {
    return {
      editorStatePatches: [],
      commandDescription: `Convert CSS percent to px: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property is not in percents.`,
    }
  }
  if (isRight(parsePercentResult)) {
    const currentValuePercent = parsePercentResult.value
    const currentValueInPxs = currentValuePercent.value * (command.parentDimensionPx / 100)

    const newValueCssNumber: CSSNumber = {
      value: currentValueInPxs,
      unit: null,
    }
    const newValue = printCSSNumber(newValueCssNumber, null)

    propsToUpdate.push({
      path: command.property,
      value: jsExpressionValue(newValue, emptyComments),
    })
  }

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Convert CSS percent to px: ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )}`,
  }
}
