import { getLayoutProperty, getLayoutPropertyOr } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { last } from '../../../core/shared/array-utils'
import {
  Either,
  eitherToMaybe,
  flatMapEither,
  isLeft,
  isRight,
  left,
  right,
} from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  JSXAttributes,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import {
  GetModifiableAttributeResult,
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import { objectMap } from '../../../core/shared/object-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { styleStringInArray } from '../../../utils/common-constants'
import { generateUUID } from '../../../utils/utils'
import {
  EditorState,
  EditorStatePatch,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import {
  CSSNumber,
  FlexDirection,
  parseCSSLengthPercent,
  parseCSSPercent,
  parseCSSPx,
  printCSSNumber,
} from '../../inspector/common/css-utils'
import { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { applyValuesAtPath } from './adjust-number-command'
import { BaseCommand, CommandFunction, CommandFunctionResult, WhenToRun } from './commands'
import { deleteValuesAtPath } from './delete-properties-command'
import { runShowToastCommand, showToastCommand } from './show-toast-command'

type CreateIfNotExistant = 'create-if-not-existing' | 'do-not-create-if-doesnt-exist'

export interface AdjustCssLengthProperty extends BaseCommand {
  type: 'ADJUST_CSS_LENGTH_PROPERTY'
  target: ElementPath
  property: PropertyPath
  valuePx: number
  parentDimensionPx: number | undefined
  parentFlexDirection: FlexDirection | null
  createIfNonExistant: CreateIfNotExistant
}

export function adjustCssLengthProperty(
  whenToRun: WhenToRun,
  target: ElementPath,
  property: PropertyPath,
  valuePx: number,
  parentDimensionPx: number | undefined,
  parentFlexDirection: FlexDirection | null,
  createIfNonExistant: CreateIfNotExistant,
): AdjustCssLengthProperty {
  return {
    type: 'ADJUST_CSS_LENGTH_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    property: property,
    valuePx: valuePx,
    parentDimensionPx: parentDimensionPx,
    parentFlexDirection: parentFlexDirection,
    createIfNonExistant: createIfNonExistant,
  }
}

export const runAdjustCssLengthProperty = (
  editorState: EditorState,
  command: AdjustCssLengthProperty,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
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
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as value is not writeable.`,
    }
  }
  const currentModifiableValue = currentValue.value
  const simpleValueResult = jsxSimpleAttributeToValue(currentModifiableValue)
  const valueProbablyExpression = isLeft(simpleValueResult)
  const targetPropertyNonExistant: boolean = currentModifiableValue.type === 'ATTRIBUTE_NOT_FOUND'

  if (
    targetPropertyNonExistant &&
    command.createIfNonExistant === 'do-not-create-if-doesnt-exist'
  ) {
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
      commandLifecycle,
      editorState,
      command.target,
      command.property,
      command.parentFlexDirection,
      parsePxResult.value,
      command.valuePx,
    )
  }

  const parsePercentResult = parseCSSPercent(simpleValueResult.value) // TODO make type contain %
  if (isRight(parsePercentResult)) {
    return updatePercentageValueByPixel(
      commandLifecycle,
      editorState,
      command.target,
      command.property,
      command.parentDimensionPx,
      command.parentFlexDirection,
      parsePercentResult.value,
      command.valuePx,
    )
  }

  if (command.createIfNonExistant === 'create-if-not-existing') {
    return setPixelValue(
      commandLifecycle,
      editorState,
      command.target,
      command.property,
      command.parentFlexDirection,
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
  commandLifecycle: InteractionLifecycle,
  editorState: EditorState,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  parentFlexDirection: FlexDirection | null,
  value: number,
) {
  const newValueCssNumber: CSSNumber = {
    value: value,
    unit: 'px',
  }

  const [editorStateWithPropsDeleted, toastPatch] = deleteConflictingPropsForWidthHeight(
    commandLifecycle,
    editorState,
    targetElement,
    newValueCssNumber,
    targetProperty,
    parentFlexDirection,
  )

  const newValue = printCSSNumber(newValueCssNumber, 'px')

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsxAttributeValue(newValue, emptyComments),
    },
  ]

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    targetElement,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch, ...toastPatch],
    commandDescription: `Set css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${value}`,
  }
}

function updatePixelValueByPixel(
  commandLifecycle: InteractionLifecycle,
  editorState: EditorState,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  parentFlexDirection: FlexDirection | null,
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

  const [editorStateWithPropsDeleted, toastPatch] = deleteConflictingPropsForWidthHeight(
    commandLifecycle,
    editorState,
    targetElement,
    newValueCssNumber,
    targetProperty,
    parentFlexDirection,
  )

  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsxAttributeValue(newValue, emptyComments),
    },
  ]

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    targetElement,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch, ...toastPatch],
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${byValue}`,
  }
}

function updatePercentageValueByPixel(
  commandLifecycle: InteractionLifecycle,
  editorState: EditorState,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  parentDimensionPx: number | undefined,
  parentFlexDirection: FlexDirection | null,
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

  const [editorStateWithPropsDeleted, toastPatch] = deleteConflictingPropsForWidthHeight(
    commandLifecycle,
    editorState,
    targetElement,
    newValueCssNumber,
    targetProperty,
    parentFlexDirection,
  )

  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsxAttributeValue(newValue, emptyComments),
    },
  ]

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorStateWithPropsDeleted,
    targetElement,
    propsToUpdate,
  )

  return {
    editorStatePatches: [propertyUpdatePatch, ...toastPatch],
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
      targetProperty,
    )} by ${byValue}`,
  }
}

const FlexSizeProperties: Array<PropertyPath> = [
  PP.create('style', 'flex'),
  PP.create('style', 'flexGrow'),
  PP.create('style', 'flexShrink'),
  PP.create('style', 'flexBasis'),
]

export function deleteConflictingPropsForWidthHeight(
  commandLifecycle: InteractionLifecycle,
  editorState: EditorState,
  target: ElementPath,
  newValue: CSSNumber,
  propertyPath: PropertyPath,
  parentFlexDirection: FlexDirection | null,
): [editorState: EditorState, toastPatch: Array<EditorStatePatch>] {
  let propertiesToDelete: Array<PropertyPath> = []

  const currentProps: Either<any, JSXAttributes> = withUnderlyingTargetFromEditorState(
    target,
    editorState,
    left({}),
    (_, element) => right(element.props),
  )

  const parentFlexDimension =
    parentFlexDirection == null
      ? null
      : MetadataUtils.flexDirectionToSimpleFlexDirection(parentFlexDirection).direction

  switch (PP.lastPart(propertyPath)) {
    case 'width':
      const minWidth = eitherToMaybe(
        getLayoutProperty('minWidth', currentProps, styleStringInArray),
      )

      const maxWidth = eitherToMaybe(
        getLayoutProperty('maxWidth', currentProps, styleStringInArray),
      )
      if (
        (minWidth?.unit === newValue.unit && minWidth.value > newValue.value) ||
        (maxWidth?.unit === newValue.unit && maxWidth.value < newValue.value)
      ) {
        if (minWidth != null) {
          propertiesToDelete.push(PP.create('style', 'minWidth'))
        }
        if (maxWidth != null) {
          propertiesToDelete.push(PP.create('style', 'maxWidth'))
        }
      }

      if (parentFlexDimension === 'horizontal') {
        FlexSizeProperties.forEach((prop) => {
          const propExists =
            eitherToMaybe(
              getLayoutProperty(
                last(prop.propertyElements) as any,
                currentProps,
                styleStringInArray,
              ),
            ) != null
          if (propExists) {
            propertiesToDelete.push(prop)
          }
        })
      }
      break
    case 'height':
      const minHeight = eitherToMaybe(
        getLayoutProperty('minHeight', currentProps, styleStringInArray),
      )
      const maxHeight = eitherToMaybe(
        getLayoutProperty('maxHeight', currentProps, styleStringInArray),
      )

      if (
        (minHeight?.unit === newValue.unit && minHeight.value > newValue.value) ||
        (maxHeight?.unit === newValue.unit && maxHeight.value < newValue.value)
      ) {
        if (minHeight != null) {
          propertiesToDelete.push(PP.create('style', 'minHeight'))
        }
        if (maxHeight != null) {
          propertiesToDelete.push(PP.create('style', 'maxHeight'))
        }
      }

      if (parentFlexDimension === 'vertical') {
        FlexSizeProperties.forEach((prop) => {
          const propExists =
            eitherToMaybe(
              getLayoutProperty(
                last(prop.propertyElements) as any,
                currentProps,
                styleStringInArray,
              ),
            ) != null
          if (propExists) {
            propertiesToDelete.push(prop)
          }
        })
      }
      break
  }

  const { editorStateWithChanges: editorStateWithPropsDeleted } = deleteValuesAtPath(
    editorState,
    target,
    propertiesToDelete,
  )

  const toastPatch =
    propertiesToDelete.length === 0
      ? []
      : runShowToastCommand(
          editorStateWithPropsDeleted,
          showToastCommand(
            `Deleted properties: ${propertiesToDelete
              .map((p) => last(p.propertyElements))
              .join(', ')}`,
            'WARNING',
            generateUUID(),
          ),
          commandLifecycle,
        ).editorStatePatches

  return [editorStateWithPropsDeleted, toastPatch]
}
