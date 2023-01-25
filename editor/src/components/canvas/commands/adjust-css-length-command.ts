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
import { BaseCommand, CommandFunction, CommandFunctionResult, WhenToRun } from './commands'
import { deleteValuesAtPath } from './delete-properties-command'

export interface AdjustCssLengthProperty extends BaseCommand {
  type: 'ADJUST_CSS_LENGTH_PROPERTY'
  target: ElementPath
  property: PropertyPath
  valuePx: number
  parentDimensionPx: number | undefined
  createIfNonExistant: boolean
}

export function adjustCssLengthProperty(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  valuePx: number,
  parentDimensionPx: number | undefined,
  createIfNonExistant: boolean,
): AdjustCssLengthProperty {
  return {
    type: 'ADJUST_CSS_LENGTH_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    property: property,
    valuePx: valuePx,
    parentDimensionPx: parentDimensionPx,
    createIfNonExistant: createIfNonExistant,
  }
}

export const runAdjustCssLengthProperty: CommandFunction<AdjustCssLengthProperty> = (
  editorState: EditorState,
  command: AdjustCssLengthProperty,
) => {
  // in case of width or height change, delete min, max and flex props
  const editorStateWithPropsDeleted = deleteConflictingPropsForWidthHeight(
    editorState,
    command.target,
    command.property,
  )

  // Identify the current value, whatever that may be.
  const currentValue: GetModifiableAttributeResult = withUnderlyingTargetFromEditorState(
    command.target,
    editorStateWithPropsDeleted,
    left(`no target element was found at path ${EP.toString(command.target)}`),
    (_, element) => {
      return getModifiableJSXAttributeAtPath(element.props, command.property)
    },
  )
  if (isLeft(currentValue)) {
    return {
      editorStatePatches: [],
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
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
      editorStatePatches: [],
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property does not exist.`,
    }
  }

  if (valueProbablyExpression) {
    // TODO add option to override expressions!!!
    return {
      editorStatePatches: [],
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property is an expression we did not want to override.`,
    }
  }

  const parsePxResult = parseCSSPx(simpleValueResult.value) // TODO make type contain px

  if (isRight(parsePxResult)) {
    return updatePixelValueByPixel(
      editorStateWithPropsDeleted,
      command.target,
      command.property,
      parsePxResult.value,
      command.valuePx,
    )
  }

  const parsePercentResult = parseCSSPercent(simpleValueResult.value) // TODO make type contain %
  if (isRight(parsePercentResult)) {
    return updatePercentageValueByPixel(
      editorStateWithPropsDeleted,
      command.target,
      command.property,
      command.parentDimensionPx,
      parsePercentResult.value,
      command.valuePx,
    )
  }

  if (command.createIfNonExistant) {
    return setPixelValue(
      editorStateWithPropsDeleted,
      command.target,
      command.property,
      command.valuePx,
    )
  }

  // fallback return
  return {
    editorStatePatches: [],
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )} not applied as the property is in a CSS unit we do not support. (${
      simpleValueResult.value
    })`,
  }
}

function setPixelValue(
  editorState: EditorState,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  value: number,
) {
  const newValueCssNumber: CSSNumber = {
    value: value,
    unit: null,
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
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Set css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${value}`,
  }
}

function updatePixelValueByPixel(
  editorState: EditorState,
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
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${byValue}`,
  }
}

function updatePercentageValueByPixel(
  editorState: EditorState,
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
      editorStatePatches: [],
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} not applied because the parent dimensions are unknown for some reason.`,
    }
  }
  if (parentDimensionPx === 0) {
    return {
      editorStatePatches: [],
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} not applied because the parent dimension is 0.`,
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
    editorStatePatches: [propertyUpdatePatch],
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${byValue}`,
  }
}

export function deleteConflictingPropsForWidthHeight(
  editorState: EditorState,
  target: ElementPath,
  propertyPath: PropertyPath,
): EditorState {
  let propertiesToDelete: Array<PropertyPath> = []
  switch (PP.lastPart(propertyPath)) {
    case 'width':
      propertiesToDelete = [PP.create(['style', 'minWidth']), PP.create(['style', 'maxWidth'])]
      break
    case 'height':
      propertiesToDelete = [PP.create(['style', 'minHeight']), PP.create(['style', 'maxHeight'])]
      break
  }

  const { editorStateWithChanges: editorStateWithPropsDeleted } = deleteValuesAtPath(
    editorState,
    target,
    propertiesToDelete,
  )

  return editorStateWithPropsDeleted
}
