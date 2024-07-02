import { notice } from '../../../components/common/notice'
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
import { roundTo } from '../../../core/shared/math-utils'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
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
import { addToastPatch } from './show-toast-command'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { ClassNameToAttributes } from '../../../core/third-party/tailwind-defaults'
import {
  convertPixelsToTailwindDimension,
  getTailwindPropFromPropertyPath,
} from '../../../core/tailwind/tailwind-helpers'

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

  const tailwindProp = getTailwindPropFromPropertyPath(command.property)
  let relevantClassNames: Array<string> = []
  let classNames: Array<string> = []

  // Identify the current value, whatever that may be.
  const currentValue: GetModifiableAttributeResult = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    left(`no target element was found at path ${EP.toString(command.target)}`),
    (_, element) => {
      if (isJSXElement(element)) {
        classNames = getClassNameAttribute(element).value?.split(' ') ?? []
        relevantClassNames =
          tailwindProp == null
            ? []
            : classNames.filter((c) => ClassNameToAttributes[c].includes(tailwindProp))

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

  let percentageValueWasReplaced: boolean = false
  const javascriptExpressionValueWasReplaced: boolean = isLeft(simpleValueResult) // Left for jsxSimpleAttributeToValue means "not simple" which means a javascript expression like `5 + props.hello`

  const parsePercentResult = parseCSSPercent(simpleValueResult.value)
  if (
    isRight(parsePercentResult) &&
    command.value.type === 'KEEP_ORIGINAL_UNIT' &&
    command.value.parentDimensionPx != null
  ) {
    const currentValuePercent = parsePercentResult.value
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
      path: command.property,
      value: jsExpressionValue(newValue, emptyComments),
    })
  } else {
    const newCssValue =
      command.value.type === 'EXPLICIT_CSS_NUMBER'
        ? command.value.value
        : cssPixelLength(command.value.valuePx)

    if (
      command.whenReplacingPercentageValues === 'warn-about-replacement' &&
      isRight(parsePercentResult)
    ) {
      percentageValueWasReplaced = true
    }

    const printedValue = printCSSNumberOrKeyword(newCssValue, 'px')
    if (relevantClassNames.length > 0 && tailwindProp != null && typeof printedValue === 'number') {
      propsToUpdate.push({
        path: PP.fromString('className'),
        value: jsExpressionValue(
          `${classNames
            .filter((c) => !relevantClassNames.includes(c))
            .join(' ')} ${convertPixelsToTailwindDimension(printedValue, tailwindProp, 'spacing')}`,
          emptyComments,
        ),
      })
    } else {
      propsToUpdate.push({
        path: command.property,
        value: jsExpressionValue(printedValue, emptyComments),
      })
    }
  }

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    command.target,
    propsToUpdate,
  )

  // Always include the property update patch, but potentially also include a warning
  // that a percentage based property was replaced with a pixel based one.
  let editorStatePatches: Array<EditorStatePatch> = [propertyUpdatePatch]
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
