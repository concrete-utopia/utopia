import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { roundTo, roundToNearestWhole } from '../../../core/shared/math-utils'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState } from '../../editor/store/editor-state'
import type { CSSNumber, FlexDirection } from '../../inspector/common/css-utils'
import { printCSSNumber } from '../../inspector/common/css-utils'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { getCSSNumberFromStyleInfo, maybeCssPropertyFromInlineStyle } from './utils/property-utils'
import type { StyleUpdate } from '../plugins/style-plugins'
import { getActivePlugin, runStyleUpdateForStrategy } from '../plugins/style-plugins'
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

  const styleInfoReader = getActivePlugin(withConflictingPropertiesRemoved).styleInfoFactory({
    projectContents: withConflictingPropertiesRemoved.projectContents,
    metadata: withConflictingPropertiesRemoved.jsxMetadata,
    elementPathTree: withConflictingPropertiesRemoved.elementPathTree,
  })

  const styleInfo = styleInfoReader(command.target)
  if (styleInfo == null) {
    return {
      editorStatePatches: [],
      commandDescription: `Adjust CSS Length Properties: Element at ${EP.toString(
        command.target,
      )} not found`,
    }
  }

  let commandDescriptions: Array<string> = []

  const propsToUpdate: StyleUpdate[] = mapDropNulls((propertyUpdate) => {
    const property = maybeCssPropertyFromInlineStyle(propertyUpdate.property)
    if (property == null) {
      commandDescriptions.push(
        `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
          propertyUpdate.property,
        )} not found.`,
      )
      return null
    }

    const currentValue = getCSSNumberFromStyleInfo(styleInfo, property)
    if (currentValue.type === 'not-css-number') {
      commandDescriptions.push(
        `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
          propertyUpdate.property,
        )} not applied as value is not writeable.`,
      )
      return null
    }

    if (
      currentValue.type === 'not-found' &&
      propertyUpdate.createIfNonExistant === 'do-not-create-if-doesnt-exist'
    ) {
      commandDescriptions.push(
        `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
          propertyUpdate.property,
        )} not applied as the property does not exist.`,
      )
      return null
    }

    if (
      currentValue.type === 'css-number' &&
      (currentValue.number.unit == null || currentValue.number.unit === 'px')
    ) {
      const { commandDescription, styleUpdate } = updatePixelValueByPixel(
        command.target,
        property,
        currentValue.number,
        propertyUpdate.valuePx,
      )
      commandDescriptions.push(commandDescription)
      return styleUpdate
    }

    if (currentValue.type === 'css-number' && currentValue.number.unit === '%') {
      const { commandDescription, styleUpdate } = updatePercentageValueByPixel(
        command.target,
        property,
        propertyUpdate.parentDimensionPx,
        currentValue.number,
        propertyUpdate.valuePx,
      )
      commandDescriptions.push(commandDescription)
      return styleUpdate ?? null
    }

    if (
      currentValue.type === 'not-found' &&
      propertyUpdate.createIfNonExistant === 'create-if-not-existing'
    ) {
      const { commandDescription, styleUpdate } = setPixelValue(
        command.target,
        property,
        propertyUpdate.valuePx,
      )
      commandDescriptions.push(commandDescription)
      return styleUpdate
    }

    commandDescriptions.push(
      `Adjust Css Length Prop: ${EP.toUid(command.target)}/${PP.toString(
        propertyUpdate.property,
      )} not applied as the property is in a CSS unit we do not support.`,
    )
    return null
  }, command.properties)

  if (propsToUpdate.length === 0) {
    return { editorStatePatches: [], commandDescription: 'No props to update' }
  }

  const { editorStatePatch } = runStyleUpdateForStrategy(
    interactionLifecycle,
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: commandDescriptions.join('\n'),
  }
}

function setPixelValue(
  targetElement: ElementPath,
  targetProperty: string,
  value: number,
): { commandDescription: string; styleUpdate: StyleUpdate } {
  const newValueCssNumber: CSSNumber = {
    value: value,
    unit: null,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  return {
    styleUpdate: { type: 'set', property: targetProperty, value: newValue },
    commandDescription: `Set css Length Prop: ${EP.toUid(
      targetElement,
    )}/${targetProperty} to ${value}.`,
  }
}

function updatePixelValueByPixel(
  targetElement: ElementPath,
  targetProperty: string,
  currentValue: CSSNumber,
  byValue: number,
): { commandDescription: string; styleUpdate: StyleUpdate } {
  if (currentValue.unit != null && currentValue.unit !== 'px') {
    throw new Error('updatePixelValueByPixel called with a non-pixel cssnumber')
  }
  const currentValuePx = currentValue.value
  const newValueCssNumber: CSSNumber = {
    value: roundToNearestWhole(currentValuePx + byValue),
    unit: currentValue.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  return {
    styleUpdate: { type: 'set', property: targetProperty, value: newValue },
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(
      targetElement,
    )}/${targetProperty} by ${byValue}`,
  }
}

function updatePercentageValueByPixel(
  targetElement: ElementPath,
  targetProperty: string,
  parentDimensionPx: number | undefined,
  currentValue: CSSNumber, // TODO restrict to percentage numbers
  byValue: number,
): { commandDescription: string; styleUpdate?: StyleUpdate } {
  if (currentValue.unit == null || currentValue.unit !== '%') {
    throw new Error('updatePercentageValueByPixel called with a non-percentage cssnumber')
  }
  if (parentDimensionPx == null) {
    return {
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(
        targetElement,
      )}/${targetProperty} not applied because the parent dimensions are unknown for some reason.`,
    }
  }
  if (parentDimensionPx === 0) {
    return {
      commandDescription: `Adjust Css Length Prop: ${EP.toUid(
        targetElement,
      )}/${targetProperty} not applied because the parent dimension is 0.`,
    }
  }
  const currentValuePercent = currentValue.value
  const offsetInPercent = (byValue / parentDimensionPx) * 100
  const newValueCssNumber: CSSNumber = {
    value: roundTo(currentValuePercent + offsetInPercent, 2),
    unit: currentValue.unit,
  }
  const newValue = printCSSNumber(newValueCssNumber, null)

  return {
    styleUpdate: { type: 'set', property: targetProperty, value: newValue },
    commandDescription: `Adjust Css Length Prop: ${EP.toUid(
      targetElement,
    )}/${targetProperty} by ${byValue}`,
  }
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
    if (propertiesToDelete.length === 0) {
      return editor
    }

    return runStyleUpdateForStrategy(
      interactionLifecycle,
      editor,
      elementPath,
      propertiesToDelete.map((p) => ({ type: 'delete', property: p })),
    ).editorStateWithChanges
  }, editorState)
}
