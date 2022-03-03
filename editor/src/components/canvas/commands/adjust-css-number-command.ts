import { number } from 'prop-types'
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
  parseCSSPercent,
  parseCSSPx,
  printCSSNumber,
} from '../../inspector/common/css-utils'
import { applyValuesAtPath } from './adjust-number-command'
import {
  BaseCommand,
  CommandFunction,
  CommandFunctionResult,
  PathMappings,
  TransientOrNot,
} from './commands'

export interface AdjustPxNumberProperty extends BaseCommand {
  type: 'ADJUST_PX_NUMBER_PROPERTY'
  target: ElementPath
  property: PropertyPath
  valuePx: number
  parentDimensionPx: number | undefined
  createIfNonExistant: boolean
}

export function adjustPxNumberProperty(
  transient: TransientOrNot,
  target: ElementPath,
  property: PropertyPath,
  valuePx: number,
  parentDimensionPx: number | undefined,
  createIfNonExistant: boolean,
): AdjustPxNumberProperty {
  return {
    type: 'ADJUST_PX_NUMBER_PROPERTY',
    transient: transient,
    target: target,
    property: property,
    valuePx: valuePx,
    parentDimensionPx: parentDimensionPx,
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

  const parsePxResult = parseCSSPx(simpleValueResult.value) // TODO make type contain px
  const parsePercentResult = parseCSSPercent(simpleValueResult.value) // TODO make type contain %

  if (isRight(parsePxResult)) {
    return updatePixelValueByPixel(
      editorState,
      pathMappings,
      command.target,
      command.property,
      parsePxResult.value,
      command.valuePx,
    )
  } else if (isRight(parsePercentResult)) {
    return updatePercentageValueByPixel(
      editorState,
      pathMappings,
      command.target,
      command.property,
      command.parentDimensionPx,
      parsePercentResult.value,
      command.valuePx,
    )
  } else {
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
}

function updatePixelValueByPixel(
  editorState: EditorState,
  pathMappings: PathMappings,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  currentValue: CSSNumber,
  byValue: number,
): CommandFunctionResult {
  if (currentValue.unit != null && currentValue.unit !== 'px') {
    throw new Error('updatePixelValueByPixel called with a non-pixel cssnumber')
  }
  const currentValuePx = currentValue.value
  const newValueCssNumber: CSSNumber = {
    value: currentValuePx + byValue,
    unit: currentValue.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsxAttributeValue(newValue, emptyComments),
    },
  ]

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    targetElement,
    propsToUpdate,
  )

  return {
    editorStatePatch: propertyUpdatePatch,
    pathMappings: pathMappings,
    commandDescription: `Adjust Px Number Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${byValue}`,
  }
}

function updatePercentageValueByPixel(
  editorState: EditorState,
  pathMappings: PathMappings,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  parentDimensionPx: number | undefined,
  currentValue: CSSNumber, // TODO restrict to percentage numbers
  byValue: number,
): CommandFunctionResult {
  if (currentValue.unit == null || currentValue.unit !== '%') {
    throw new Error('updatePercentageValueByPixel called with a non-percentage cssnumber')
  }
  if (parentDimensionPx == null) {
    return {
      editorStatePatch: {},
      pathMappings: pathMappings,
      commandDescription: `Adjust Px Number Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} not applied because the parent dimensions are unknown for some reason.`,
    }
  }
  const currentValuePercent = currentValue.value
  const offsetInPercent = (byValue / parentDimensionPx) * 100
  const newValueCssNumber: CSSNumber = {
    value: currentValuePercent + offsetInPercent,
    unit: currentValue.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsxAttributeValue(newValue, emptyComments),
    },
  ]

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    targetElement,
    propsToUpdate,
  )

  return {
    editorStatePatch: propertyUpdatePatch,
    pathMappings: pathMappings,
    commandDescription: `Adjust Px Number Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${byValue}`,
  }
}
