import { styleStringInArray } from '../../utils/common-constants'
import { applyParserToValue } from '../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { defaultEither, isLeft, right } from '../../core/shared/either'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { isJSXElement } from '../../core/shared/element-template'
import type { CanvasVector, Size } from '../../core/shared/math-utils'
import { numberIsZero, roundTo, zeroRectIfNullOrInfinity } from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import type { ElementPath, ProjectContents } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import type { CSSNumber, CSSNumberUnit, CSSPadding } from '../inspector/common/css-utils'
import { printCSSNumber } from '../inspector/common/css-utils'
import type { EdgePiece } from './canvas-types'
import type {
  AdjustPrecision,
  CSSNumberWithRenderedValue,
} from './controls/select-mode/controls-common'
import {
  cssNumberWithRenderedValue,
  fallbackEmptyValue,
  offsetMeasurementByDelta,
  unitlessCSSNumberWithRenderedValue,
} from './controls/select-mode/controls-common'
import type { Modifiers } from '../../utils/modifiers'
import type {
  AdjustCssLengthProperties,
  LengthPropertyToAdjust,
} from './commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from './commands/adjust-css-length-command'
import { detectFillHugFixedState } from '../inspector/inspector-common'
import { stylePropPathMappingFn } from '../inspector/common/property-path-hooks'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { ProjectContentTreeRoot } from '../assets'
import { getElementFromProjectContents } from '../editor/store/editor-state'
import { getClassNameAttribute, getClassNameMapping } from '../../core/tailwind/tailwind-options'

export const EdgePieces: Array<EdgePiece> = ['top', 'bottom', 'left', 'right']

export type CSSPaddingKey = keyof CSSPadding
export type CSSPaddingMappedValues<T> = { [key in CSSPaddingKey]: T }
export type CSSPaddingMeasurements = CSSPaddingMappedValues<CSSNumberWithRenderedValue>

export function paddingFromSpecialSizeMeasurements(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSPaddingMappedValues<number> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const paddingFromMeasurements = element?.specialSizeMeasurements.padding

  const paddingMappedMeasurements: CSSPaddingMappedValues<number> = {
    paddingTop: paddingFromMeasurements?.top ?? 0,
    paddingBottom: paddingFromMeasurements?.bottom ?? 0,
    paddingLeft: paddingFromMeasurements?.left ?? 0,
    paddingRight: paddingFromMeasurements?.right ?? 0,
  }

  return paddingMappedMeasurements
}

export function simplePaddingFromMetadata(
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const defaults: CSSPaddingMappedValues<CSSNumber | undefined> = {
    paddingTop: undefined,
    paddingRight: undefined,
    paddingBottom: undefined,
    paddingLeft: undefined,
  }

  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return {
      paddingTop: undefined,
      paddingRight: undefined,
      paddingBottom: undefined,
      paddingLeft: undefined,
    }
  }

  const paddingNumbers = paddingFromSpecialSizeMeasurements(metadata, elementPath)

  const classList = getClassNameAttribute(
    getElementFromProjectContents(elementPath, projectContents),
  )?.value

  const classes = typeof classList === 'string' ? classList : ''
  const mapping = getClassNameMapping(classes)

  const padding: CSSPadding | undefined = defaultEither(
    undefined,
    applyParserToValue('padding', mapping['p']),
  )

  const paddingLonghands: CSSPaddingMappedValues<CSSNumber | undefined> = {
    paddingTop: defaultEither(undefined, applyParserToValue('paddingTop', mapping['pt'])),
    paddingBottom: defaultEither(undefined, applyParserToValue('paddingBottom', mapping['pb'])),
    paddingLeft: defaultEither(undefined, applyParserToValue('paddingLeft', mapping['pl'])),
    paddingRight: defaultEither(undefined, applyParserToValue('paddingRight', mapping['pr'])),
  }

  const make = (prop: CSSPaddingKey): CSSNumberWithRenderedValue | undefined => {
    return (
      optionalMap(
        (p) => cssNumberWithRenderedValue(p, paddingNumbers[prop]),
        paddingLonghands[prop],
      ) ??
      optionalMap(
        (p) => cssNumberWithRenderedValue(p, paddingNumbers[prop]),
        padding?.[prop] ?? defaults[prop],
      ) ??
      undefined
    )
  }

  return {
    paddingTop: make('paddingTop'),
    paddingRight: make('paddingRight'),
    paddingBottom: make('paddingBottom'),
    paddingLeft: make('paddingLeft'),
  }
}

export function combinePaddings(
  paddingNumber: CSSPaddingMappedValues<number>,
  paddingRenderedValues: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue> {
  return {
    paddingTop:
      paddingRenderedValues.paddingTop ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingTop),
    paddingRight:
      paddingRenderedValues.paddingRight ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingRight),
    paddingBottom:
      paddingRenderedValues.paddingBottom ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingBottom),
    paddingLeft:
      paddingRenderedValues.paddingLeft ??
      unitlessCSSNumberWithRenderedValue(paddingNumber.paddingLeft),
  }
}

export function maybeFullPadding(
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue> | undefined {
  const { paddingTop, paddingRight, paddingBottom, paddingLeft } = padding
  if (paddingTop == null || paddingRight == null || paddingBottom == null || paddingLeft == null) {
    return undefined
  }
  return { paddingTop, paddingRight, paddingBottom, paddingLeft }
}

export function paddingPropForEdge(edgePiece: EdgePiece): CSSPaddingKey {
  switch (edgePiece) {
    case 'top':
      return 'paddingTop'
    case 'bottom':
      return 'paddingBottom'
    case 'right':
      return 'paddingRight'
    case 'left':
      return 'paddingLeft'
    default:
      assertNever(edgePiece)
  }
}

export function paddingForEdge(edgePiece: EdgePiece, padding: CSSPaddingMeasurements): number {
  return padding[paddingPropForEdge(edgePiece)].renderedValuePx
}

export function paddingForEdgeSimplePadding(
  edgePiece: EdgePiece,
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): number {
  return padding[paddingPropForEdge(edgePiece)]?.renderedValuePx ?? 0
}

export function offsetPaddingByEdge(
  prop: CSSPaddingKey,
  delta: number,
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
  precision: AdjustPrecision,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined> {
  return {
    ...padding,
    [prop]: offsetMeasurementByDelta(
      padding[prop] ?? unitlessCSSNumberWithRenderedValue(0),
      delta,
      precision,
    ),
  }
}

export function paddingToPaddingString(padding: CSSPaddingMeasurements): string {
  return [
    printCssNumberWithDefaultUnit(fallbackEmptyValue(padding.paddingTop), 'px'),
    printCssNumberWithDefaultUnit(fallbackEmptyValue(padding.paddingRight), 'px'),
    printCssNumberWithDefaultUnit(fallbackEmptyValue(padding.paddingBottom), 'px'),
    printCssNumberWithDefaultUnit(fallbackEmptyValue(padding.paddingLeft), 'px'),
  ].join(' ')
}

export function deltaFromEdge(delta: CanvasVector, edgePiece: EdgePiece): number {
  switch (edgePiece) {
    case 'top':
      return delta.y
    case 'bottom':
      return -delta.y
    case 'left':
      return delta.x
    case 'right':
      return -delta.x
    default:
      assertNever(edgePiece)
  }
}

export const PaddingIndictorOffset = (scale: number): number => 10 / scale

export function printCssNumberWithDefaultUnit(
  value: CSSNumber,
  defaultUnit?: CSSNumberUnit,
): string | number {
  if (defaultUnit != null && value.unit == null) {
    return printCSSNumber({ value: value.value, unit: defaultUnit }, null)
  }
  return printCSSNumber(value, null)
}

export type PaddingAdjustMode = 'individual' | 'cross-axis' | 'all'
export function paddingAdjustMode(modifiers: Modifiers): PaddingAdjustMode {
  if (modifiers.shift === true && modifiers.alt === true) {
    return 'all'
  }
  if (modifiers.alt === true) {
    return 'cross-axis'
  }
  return 'individual'
}

export function pixelPaddingFromPadding(
  padding: CSSNumber,
  parentSizeInDimension: number,
): number | null {
  switch (padding.unit) {
    case 'px':
    case null:
      return padding.value
    case '%':
      return (padding.value / 100) * parentSizeInDimension
    default:
      return null // TODO Should we support other units here?
  }
}

export function getSizeUpdateCommandsForNewPadding(
  combinedXPadding: number | null,
  combinedYPadding: number | null,
  startingSize: Size,
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
): AdjustCssLengthProperties {
  const selectedElement = selectedElements[0]
  const targetFrame = MetadataUtils.getFrameOrZeroRect(selectedElement, metadata)

  const allChildPaths = MetadataUtils.getChildrenPathsOrdered(pathTrees, selectedElement)

  const nonAbsoluteChildrenPaths = allChildPaths.filter((childPath) =>
    MetadataUtils.targetParticipatesInAutoLayout(metadata, childPath),
  )

  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, selectedElement)
  const elementParentBounds = elementMetadata?.specialSizeMeasurements.immediateParentBounds
  const elementParentFlexDirection = elementMetadata?.specialSizeMeasurements.parentFlexDirection

  const adjustSizeCommandForDimension = (
    dimension: 'horizontal' | 'vertical',
  ): LengthPropertyToAdjust | null => {
    const isHorizontal = dimension === 'horizontal'
    const combinedPaddingInDimension = isHorizontal ? combinedXPadding : combinedYPadding

    if (combinedPaddingInDimension == null) {
      return null
    }

    const dimensionKey = isHorizontal ? 'width' : 'height'

    const fixedSizeChildrenPaths = nonAbsoluteChildrenPaths.filter(
      (childPath) =>
        detectFillHugFixedState(dimension, metadata, childPath).fixedHugFill?.type === 'fixed',
    )
    const childrenBoundingFrameMaybeInfinite = MetadataUtils.getBoundingRectangleInCanvasCoords(
      fixedSizeChildrenPaths,
      metadata,
    )
    const childrenBoundingFrame = zeroRectIfNullOrInfinity(childrenBoundingFrameMaybeInfinite)

    const combinedContentSizeInDimension =
      combinedPaddingInDimension + childrenBoundingFrame[dimensionKey]

    // TODO We need a way to call the correct resizing strategy here, but they are all assuming
    // the drag originates from a given edge, whereas we want to pass in the desired delta to a
    // dimension and receive the required commands to resize the element
    const sizeDelta = combinedContentSizeInDimension - targetFrame[dimensionKey]

    // clamp the delta so that the resultant frame will never be smaller than the starting frame
    // when scrubbing
    const clampedSizeDelta = Math.max(
      roundTo(sizeDelta, 0),
      startingSize[dimensionKey] - targetFrame[dimensionKey],
    )

    return numberIsZero(clampedSizeDelta)
      ? null
      : lengthPropertyToAdjust(
          stylePropPathMappingFn(dimensionKey, styleStringInArray),
          clampedSizeDelta,
          elementParentBounds?.[dimensionKey],
          'do-not-create-if-doesnt-exist',
        )
  }

  const horizontalSizeAdjustment = adjustSizeCommandForDimension('horizontal')
  const verticalSizeAdjustment = adjustSizeCommandForDimension('vertical')

  let lengthPropertiesToAdjust: Array<LengthPropertyToAdjust> = []
  if (horizontalSizeAdjustment != null) {
    lengthPropertiesToAdjust.push(horizontalSizeAdjustment)
  }

  if (verticalSizeAdjustment != null) {
    lengthPropertiesToAdjust.push(verticalSizeAdjustment)
  }

  return adjustCssLengthProperties(
    'always',
    selectedElement,
    elementParentFlexDirection ?? null,
    lengthPropertiesToAdjust,
  )
}
