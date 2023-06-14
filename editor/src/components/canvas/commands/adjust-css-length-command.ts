import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Either, foldEither, isLeft, isRight, left, mapEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  isJSXElement,
  jsExpressionValue,
  JSXAttributes,
  JSXElement,
  JSXElementChild,
} from '../../../core/shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { EditorState, modifyUnderlyingForOpenFile } from '../../editor/store/editor-state'
import {
  CSSNumber,
  FlexDirection,
  parseCSSPercent,
  parseCSSPx,
  printCSSNumber,
} from '../../inspector/common/css-utils'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { deleteValuesAtPath } from './delete-properties-command'
import { patchParseSuccessAtElementPath } from './patch-utils'

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

export const runAdjustCssLengthProperty: CommandFunction<AdjustCssLengthProperties> = (
  editorState: EditorState,
  command: AdjustCssLengthProperties,
) => {
  let commandDescriptions: Array<string> = []
  const updatedEditorState: EditorState = modifyUnderlyingForOpenFile(
    command.target,
    editorState,
    (element) => {
      if (isJSXElement(element)) {
        const originalElement: JSXElement = element
        let workingElement: JSXElement = element
        for (const property of command.properties) {
          const attributesWithConflictingPropsDeleted =
            deleteConflictingPropsForWidthHeightFromAttributes(
              workingElement.props,
              property.property,
              command.parentFlexDirection,
            )
          if (isLeft(attributesWithConflictingPropsDeleted)) {
            commandDescriptions.push(attributesWithConflictingPropsDeleted.value)
            return element
          }
          const currentValue = getModifiableJSXAttributeAtPath(
            attributesWithConflictingPropsDeleted.value,
            property.property,
          )

          if (isLeft(currentValue)) {
            commandDescriptions.push(
              `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
                property.property,
              )} not applied as value is not writeable.`,
            )
            return element
          }
          const currentModifiableValue = currentValue.value
          const simpleValueResult = jsxSimpleAttributeToValue(currentModifiableValue)
          const valueProbablyExpression = isLeft(simpleValueResult)
          const targetPropertyNonExistant: boolean =
            currentModifiableValue.type === 'ATTRIBUTE_NOT_FOUND'

          if (
            targetPropertyNonExistant &&
            property.createIfNonExistant === 'do-not-create-if-doesnt-exist'
          ) {
            commandDescriptions.push(
              `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
                property.property,
              )} not applied as the property does not exist.`,
            )
            return element
          }

          if (valueProbablyExpression) {
            // TODO add option to override expressions!!!
            commandDescriptions.push(
              `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
                property.property,
              )} not applied as the property is an expression we did not want to override.`,
            )
            return element
          }

          const parsePxResult = parseCSSPx(simpleValueResult.value) // TODO make type contain px

          function handleUpdateResult(
            result: Either<string, UpdatedPropsAndCommandDescription>,
          ): void {
            foldEither(
              (error) => {
                commandDescriptions.push(error)
                workingElement = originalElement
              },
              (updatedProps) => {
                commandDescriptions.push(updatedProps.commandDescription)
                workingElement = {
                  ...workingElement,
                  props: updatedProps.updatedProps,
                }
              },
              result,
            )
          }

          if (isRight(parsePxResult)) {
            handleUpdateResult(
              updatePixelValueByPixel(
                attributesWithConflictingPropsDeleted.value,
                command.target,
                property.property,
                parsePxResult.value,
                property.valuePx,
              ),
            )
            continue
          }

          const parsePercentResult = parseCSSPercent(simpleValueResult.value) // TODO make type contain %
          if (isRight(parsePercentResult)) {
            handleUpdateResult(
              updatePercentageValueByPixel(
                attributesWithConflictingPropsDeleted.value,
                command.target,
                property.property,
                property.parentDimensionPx,
                parsePercentResult.value,
                property.valuePx,
              ),
            )
            continue
          }

          if (property.createIfNonExistant === 'create-if-not-existing') {
            handleUpdateResult(
              setPixelValue(
                attributesWithConflictingPropsDeleted.value,
                command.target,
                property.property,
                property.valuePx,
              ),
            )
            continue
          }

          // Updating the props fallback.
          commandDescriptions.push(
            `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
              property.property,
            )} not applied as the property is in a CSS unit we do not support. (${
              simpleValueResult.value
            })`,
          )
          return element
        }
        return workingElement
      }

      // Final fallback.
      return element
    },
  )
  if (commandDescriptions == null) {
    return {
      editorStatePatches: [],
      commandDescription: `No JSXElement was found at path ${EP.toString(command.target)}.`,
    }
  } else {
    if (updatedEditorState === editorState) {
      return {
        editorStatePatches: [],
        commandDescription: commandDescriptions.join('\n'),
      }
    } else {
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
    value: currentValuePx + byValue,
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
    value: currentValuePercent + offsetInPercent,
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
) {
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
  return propertiesToDelete
}

export function deleteConflictingPropsForWidthHeightFromAttributes(
  attributes: JSXAttributes,
  propertyPath: PropertyPath,
  parentFlexDirection: FlexDirection | null,
): Either<string, JSXAttributes> {
  const propertiesToDelete: Array<PropertyPath> = getConflictingPropertiesToDelete(
    parentFlexDirection,
    propertyPath,
  )
  return unsetJSXValuesAtPaths(attributes, propertiesToDelete)
}

export function deleteConflictingPropsForWidthHeight(
  editorState: EditorState,
  target: ElementPath,
  propertyPath: PropertyPath,
  parentFlexDirection: FlexDirection | null,
): EditorState {
  const propertiesToDelete: Array<PropertyPath> = getConflictingPropertiesToDelete(
    parentFlexDirection,
    propertyPath,
  )

  const { editorStateWithChanges: editorStateWithPropsDeleted } = deleteValuesAtPath(
    editorState,
    target,
    propertiesToDelete,
  )

  return editorStateWithPropsDeleted
}
