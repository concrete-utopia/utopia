import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { Either } from '../../../core/shared/either'
import { foldEither, isLeft, isRight, left, mapEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type { JSXAttributes, JSXElement } from '../../../core/shared/element-template'
import {
  emptyComments,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import type { ValueAtPath } from '../../../core/shared/jsx-attributes'
import { setJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attribute-utils'
import { roundTo, roundToNearestWhole } from '../../../core/shared/math-utils'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import { modifyUnderlyingForOpenFile } from '../../editor/store/editor-state'
import type { CSSNumber, FlexDirection } from '../../inspector/common/css-utils'
import { parseCSSPercent, parseCSSPx, printCSSNumber } from '../../inspector/common/css-utils'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import { patchParseSuccessAtElementPath } from './patch-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { maybeCssPropertyFromInlineStyle } from './utils/property-utils'
import { runStyleUpdateForStrategy } from '../plugins/style-plugins'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'

export type CreateIfNotExistant = 'create-if-not-existing' | 'do-not-create-if-doesnt-exist'

export interface LengthPropertyToAdjust {
  property: PropertyPath
  valuePx: number
  parentDimensionPx: number | undefined
  createIfNonExistant: CreateIfNotExistant
}

export function lengthPropertyToAdjust(
  property: PropertyPath,
  valuePx: number,
  parentDimensionPx: number | undefined,
  createIfNonExistant: CreateIfNotExistant,
): LengthPropertyToAdjust {
  return {
    property: property,
    valuePx: valuePx,
    parentDimensionPx: parentDimensionPx,
    createIfNonExistant: createIfNonExistant,
  }
}

export interface AdjustCssLengthProperties extends BaseCommand {
  type: 'ADJUST_CSS_LENGTH_PROPERTY'
  target: ElementPath
  parentFlexDirection: FlexDirection | null
  properties: Array<LengthPropertyToAdjust>
}

export function adjustCssLengthProperties(
  whenToRun: WhenToRun,
  target: ElementPath,
  parentFlexDirection: FlexDirection | null,
  properties: Array<LengthPropertyToAdjust>,
): AdjustCssLengthProperties {
  return {
    type: 'ADJUST_CSS_LENGTH_PROPERTY',
    whenToRun: whenToRun,
    target: target,
    parentFlexDirection: parentFlexDirection,
    properties: properties,
  }
}

interface UpdatedPropsAndCommandDescription {
  updatedProps: JSXAttributes
  commandDescription: string
}

export const runAdjustCssLengthProperties = (
  editorState: EditorState,
  command: AdjustCssLengthProperties,
  interactionLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  const withConflictingPropertiesRemoved = deleteConflictingPropsForWidthHeight(
    interactionLifecycle,
    editorState,
    command.target,
    command.properties.map((p) => p.property),
    command.parentFlexDirection,
  )

  let commandDescriptions: Array<string> = []
  const updatedEditorState: EditorState = modifyUnderlyingForOpenFile(
    command.target,
    withConflictingPropertiesRemoved,
    (element) => {
      if (isJSXElement(element)) {
        return command.properties.reduce((workingElement, property) => {
          // Get the current value of the property...
          const currentValue = getModifiableJSXAttributeAtPath(element.props, property.property)
          // ...If the value is not writeable then escape out.
          if (isLeft(currentValue)) {
            commandDescriptions.push(
              `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
                property.property,
              )} not applied as value is not writeable.`,
            )
            return workingElement
          }

          // ...Determine some other facts about the current value.
          const currentModifiableValue = currentValue.value
          const simpleValueResult = jsxSimpleAttributeToValue(currentModifiableValue)
          const valueProbablyExpression = isLeft(simpleValueResult)
          const targetPropertyNonExistant: boolean =
            currentModifiableValue.type === 'ATTRIBUTE_NOT_FOUND'

          // ...If the current value does not exist and we shouldn't create it if it doesn't exist
          // then exit early from handling this property.
          if (
            targetPropertyNonExistant &&
            property.createIfNonExistant === 'do-not-create-if-doesnt-exist'
          ) {
            commandDescriptions.push(
              `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
                property.property,
              )} not applied as the property does not exist.`,
            )
            return workingElement
          }

          // ...If the value is an expression then we can't update it.
          if (valueProbablyExpression) {
            // TODO add option to override expressions!!!
            commandDescriptions.push(
              `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
                property.property,
              )} not applied as the property is an expression we did not want to override.`,
            )
            return workingElement
          }

          // Commonly used function for handling the updates.
          function handleUpdateResult(
            result: Either<string, UpdatedPropsAndCommandDescription>,
          ): JSXElement {
            return foldEither(
              (error) => {
                commandDescriptions.push(error)
                return workingElement
              },
              (updatedProps) => {
                commandDescriptions.push(updatedProps.commandDescription)
                return {
                  ...workingElement,
                  props: updatedProps.updatedProps,
                }
              },
              result,
            )
          }

          // Parse the current value as a pixel value...
          const parsePxResult = parseCSSPx(simpleValueResult.value) // TODO make type contain px
          // ...If the value can be parsed as a pixel value then update it.
          if (isRight(parsePxResult)) {
            return handleUpdateResult(
              updatePixelValueByPixel(
                element.props,
                command.target,
                property.property,
                parsePxResult.value,
                property.valuePx,
              ),
            )
          }

          // Parse the current value as a percentage value...
          const parsePercentResult = parseCSSPercent(simpleValueResult.value) // TODO make type contain %
          // ...If the value can be parsed as a percentage value then update it.
          if (isRight(parsePercentResult)) {
            return handleUpdateResult(
              updatePercentageValueByPixel(
                element.props,
                command.target,
                property.property,
                property.parentDimensionPx,
                parsePercentResult.value,
                property.valuePx,
              ),
            )
          }

          // Otherwise if it is permitted to create it if it doesn't exist, then do so.
          if (property.createIfNonExistant === 'create-if-not-existing') {
            return handleUpdateResult(
              setPixelValue(element.props, command.target, property.property, property.valuePx),
            )
          }

          // Updating the props fallback.
          commandDescriptions.push(
            `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
              property.property,
            )} not applied as the property is in a CSS unit we do not support. (${
              simpleValueResult.value
            })`,
          )
          return workingElement
        }, element)
      }

      // Final fallback.
      return element
    },
  )

  if (commandDescriptions.length === 0) {
    // Cater for no updates at all happened.
    return {
      editorStatePatches: [],
      commandDescription: `No JSXElement was found at path ${EP.toString(command.target)}.`,
    }
  } else {
    if (updatedEditorState === editorState) {
      // As the `EditorState` never changed, return an empty patch.
      return {
        editorStatePatches: [],
        commandDescription: commandDescriptions.join('\n'),
      }
    } else {
      // Build the patch for the changes.
      const editorStatePatch = patchParseSuccessAtElementPath(
        command.target,
        updatedEditorState,
        (success) => {
          return {
            topLevelElements: {
              $set: success.topLevelElements,
            },
            imports: {
              $set: success.imports,
            },
          }
        },
      )
      return {
        editorStatePatches: [editorStatePatch],
        commandDescription: commandDescriptions.join('\n'),
      }
    }
  }
}

function setPixelValue(
  properties: JSXAttributes,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  value: number,
): Either<string, UpdatedPropsAndCommandDescription> {
  const newValueCssNumber: CSSNumber = {
    value: value,
    unit: null,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsExpressionValue(newValue, emptyComments),
    },
  ]

  const updatePropsResult = setJSXValuesAtPaths(properties, propsToUpdate)

  return mapEither((updatedProps) => {
    return {
      updatedProps: updatedProps,
      commandDescription: `Set css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} to ${value}.`,
    }
  }, updatePropsResult)
}

function updatePixelValueByPixel(
  properties: JSXAttributes,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  currentValue: CSSNumber,
  byValue: number,
): Either<string, UpdatedPropsAndCommandDescription> {
  if (currentValue.unit != null && currentValue.unit !== 'px') {
    throw new Error('updatePixelValueByPixel called with a non-pixel cssnumber')
  }
  const currentValuePx = currentValue.value
  const newValueCssNumber: CSSNumber = {
    value: roundToNearestWhole(currentValuePx + byValue),
    unit: currentValue.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsExpressionValue(newValue, emptyComments),
    },
  ]
  const updatePropsResult = setJSXValuesAtPaths(properties, propsToUpdate)

  return mapEither((updatedProps) => {
    return {
      updatedProps: updatedProps,
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} by ${byValue}`,
    }
  }, updatePropsResult)
}

function updatePercentageValueByPixel(
  properties: JSXAttributes,
  targetElement: ElementPath,
  targetProperty: PropertyPath,
  parentDimensionPx: number | undefined,
  currentValue: CSSNumber, // TODO restrict to percentage numbers
  byValue: number,
): Either<string, UpdatedPropsAndCommandDescription> {
  if (currentValue.unit == null || currentValue.unit !== '%') {
    throw new Error('updatePercentageValueByPixel called with a non-percentage cssnumber')
  }
  if (parentDimensionPx == null) {
    return left(
      `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} not applied because the parent dimensions are unknown for some reason.`,
    )
  }
  if (parentDimensionPx === 0) {
    return left(
      `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} not applied because the parent dimension is 0.`,
    )
  }
  const currentValuePercent = currentValue.value
  const offsetInPercent = (byValue / parentDimensionPx) * 100
  const newValueCssNumber: CSSNumber = {
    value: roundTo(currentValuePercent + offsetInPercent, 2),
    unit: currentValue.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: targetProperty,
      value: jsExpressionValue(newValue, emptyComments),
    },
  ]

  const updatePropsResult = setJSXValuesAtPaths(properties, propsToUpdate)

  return mapEither((updatedProps) => {
    return {
      updatedProps: updatedProps,
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(targetElement)}/${PP.toString(
        targetProperty,
      )} by ${byValue}`,
    }
  }, updatePropsResult)
}

const FlexSizeProperties: Array<PropertyPath> = [
  PP.create('style', 'flex'),
  PP.create('style', 'flexGrow'),
  PP.create('style', 'flexShrink'),
  PP.create('style', 'flexBasis'),
]

function getConflictingPropertiesToDelete(
  parentFlexDirection: FlexDirection | null,
  propertyPath: PropertyPath,
): Array<string> {
  let propertiesToDelete: Array<PropertyPath> = []

  const parentFlexDimension =
    parentFlexDirection == null
      ? null
      : MetadataUtils.flexDirectionToSimpleFlexDirection(parentFlexDirection).direction

  switch (PP.lastPart(propertyPath)) {
    case 'width':
      propertiesToDelete = [PP.create('style', 'minWidth'), PP.create('style', 'maxWidth')]
      if (parentFlexDimension === 'horizontal') {
        propertiesToDelete.push(...FlexSizeProperties)
      }
      break
    case 'height':
      propertiesToDelete = [PP.create('style', 'minHeight'), PP.create('style', 'maxHeight')]
      if (parentFlexDimension === 'vertical') {
        propertiesToDelete.push(...FlexSizeProperties)
      }
      break
  }
  return mapDropNulls(maybeCssPropertyFromInlineStyle, propertiesToDelete)
}

export function deleteConflictingPropsForWidthHeight(
  interactionLifecycle: InteractionLifecycle,
  editorState: EditorState,
  elementPath: ElementPath,
  propertyPaths: PropertyPath[],
  parentFlexDirection: FlexDirection | null,
): EditorState {
  return propertyPaths.reduce((editor, propertyPath) => {
    const propertiesToDelete = getConflictingPropertiesToDelete(parentFlexDirection, propertyPath)
    return runStyleUpdateForStrategy(
      interactionLifecycle,
      editor,
      elementPath,
      propertiesToDelete.map((p) => ({ type: 'delete', property: p })),
    ).editorStateWithChanges
  }, editorState)
}
