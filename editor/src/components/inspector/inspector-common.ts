import * as PP from '../../core/shared/property-path'
import * as EP from '../../core/shared/element-path'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  allElemsEqual,
  mapDropNulls,
  safeIndex,
  strictEvery,
  stripNulls,
  uniq,
} from '../../core/shared/array-utils'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXAttributes,
} from '../../core/shared/element-template'
import {
  isJSXElement,
  jsxElementName,
  jsxElementNameEquals,
} from '../../core/shared/element-template'
import type { ElementPath, PropertyPath } from '../../core/shared/project-file-types'
import type { CSSNumber, FlexDirection } from './common/css-utils'
import {
  cssNumber,
  cssPixelLength,
  parseCSSLengthPercent,
  parseCSSNumber,
} from './common/css-utils'
import { assertNever, fastForEach } from '../../core/shared/utils'
import { defaultEither, foldEither, isLeft, isRight, right } from '../../core/shared/either'
import { elementOnlyHasTextChildren } from '../../core/model/element-template-utils'
import { forceNotNull, optionalMap } from '../../core/shared/optional-utils'
import type { CSSProperties } from 'react'
import type { CanvasCommand } from '../canvas/commands/commands'
import { deleteProperties } from '../canvas/commands/delete-properties-command'
import { setProperty } from '../canvas/commands/set-property-command'
import { addContainLayoutIfNeeded } from '../canvas/commands/add-contain-layout-if-needed-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../canvas/commands/set-css-length-command'
import {
  setPropFillStrategies,
  setPropHugStrategies,
} from './inspector-strategies/inspector-strategies'
import { commandsForFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import {
  CanvasRectangle,
  roundUpToNearestHalf,
  SimpleRectangle,
} from '../../core/shared/math-utils'
import {
  canvasRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  zeroRectIfNullOrInfinity,
} from '../../core/shared/math-utils'
import { inlineHtmlElements } from '../../utils/html-elements'
import { intersection } from '../../core/shared/set-utils'
import { showToastCommand } from '../canvas/commands/show-toast-command'
import { parseFlex } from '../../printer-parsers/css/css-parser-flex'
import { LayoutPinnedProps } from '../../core/layout/layout-helpers-new'
import { getLayoutLengthValueOrKeyword } from '../../core/layout/getLayoutProperty'
import type { Frame } from 'utopia-api/core'
import { getPinsToDelete } from './common/layout-property-path-hooks'
import type { ControlStatus } from '../../uuiui-deps'
import { getFallbackControlStatusForProperty } from './common/control-status'
import type { AllElementProps, ElementProps } from '../editor/store/editor-state'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import { treatElementAsGroupLike } from '../canvas/canvas-strategies/strategies/group-helpers'
import {
  convertGroupToFrameCommands,
  groupConversionCommands,
} from '../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { fixedSizeDimensionHandlingText } from '../text-editor/text-handling'

export type StartCenterEnd = 'flex-start' | 'center' | 'flex-end'

export type FlexJustifyContent = StartCenterEnd | 'space-around' | 'space-between' | 'space-evenly'

export function getFlexJustifyContent(value: string | null): FlexJustifyContent | null {
  switch (value) {
    case 'flex-start':
      return 'flex-start'
    case 'center':
      return 'center'
    case 'flex-end':
      return 'flex-end'
    case 'space-around':
      return 'space-around'
    case 'space-between':
      return 'space-between'
    case 'space-evenly':
      return 'space-evenly'
    default:
      return null
  }
}

export type FlexAlignment = StartCenterEnd | 'auto' | 'stretch'

export function getFlexAlignment(value: string | null): FlexAlignment | null {
  switch (value) {
    case 'auto':
      return 'auto'
    case 'flex-start':
      return 'flex-start'
    case 'center':
      return 'center'
    case 'flex-end':
      return 'flex-end'
    case 'stretch':
      return 'stretch'
    default:
      return null
  }
}

function stringToFlexDirection(str: string | null): FlexDirection | null {
  switch (str) {
    case 'row':
      return 'row'
    case 'row-reverse':
      return 'row-reverse'
    case 'column':
      return 'column'
    case 'column-reverse':
      return 'column-reverse'
    default:
      return null
  }
}

export interface JustifyContentFlexAlignemt {
  justifyContent: FlexJustifyContent
  alignItems: FlexAlignment
}

type Detect<T> = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
) => T | null

export const DefaultFlexDirection: FlexDirection = 'row'

export function detectParentFlexDirection(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexDirection | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return element.specialSizeMeasurements.parentFlexDirection
}

export function detectFlexDirectionOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexDirection | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || !MetadataUtils.isFlexLayoutedContainer(element)) {
    return null
  }

  return (
    stringToFlexDirection(element.specialSizeMeasurements?.flexDirection ?? null) ??
    DefaultFlexDirection
  )
}

export const detectFlexDirection = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): FlexDirection => {
  const allDetectedMeasurements = elementPaths.map((path) => detectFlexDirectionOne(metadata, path))
  return allElemsEqual(allDetectedMeasurements, (l, r) => l === r)
    ? allDetectedMeasurements.at(0) ?? 'row'
    : 'row'
}

function detectFlexAlignJustifyContentOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): JustifyContentFlexAlignemt | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || !MetadataUtils.isFlexLayoutedContainer(element)) {
    return null
  }

  const justifyContent: FlexJustifyContent | null = getFlexJustifyContent(
    element.specialSizeMeasurements.justifyContent ?? null,
  )
  const alignItems: FlexAlignment | null = getFlexAlignment(
    element.specialSizeMeasurements.alignItems ?? null,
  )

  if (justifyContent == null || alignItems == null) {
    return null
  }

  return { justifyContent, alignItems }
}

export const detectFlexAlignJustifyContent: Detect<JustifyContentFlexAlignemt> = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
) => {
  const allDetectedMeasurements = mapDropNulls(
    (path) => detectFlexAlignJustifyContentOne(metadata, path),
    elementPaths,
  )

  if (allDetectedMeasurements.length !== elementPaths.length) {
    return null
  }

  return allElemsEqual(
    allDetectedMeasurements,
    (l, r) => l.alignItems === r.alignItems && l.justifyContent === r.justifyContent,
  )
    ? allDetectedMeasurements[0]
    : null
}

export function filterKeepFlexContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): ElementPath[] {
  return elementPaths.filter((e: ElementPath | null) =>
    MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, e)),
  )
}

export function numberOfFlexContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): number {
  return filterKeepFlexContainers(metadata, elementPaths).length
}

export function detectAreElementsFlexContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): boolean {
  return strictEvery(elementPaths, (path) =>
    MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, path)),
  )
}

export const isFlexColumn = (flexDirection: FlexDirection): boolean =>
  flexDirection.startsWith('column')

export const hugContentsApplicableForContainer = (
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean => {
  return (
    mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ).filter(
      (element) =>
        !(
          MetadataUtils.isPositionFixed(element) ||
          MetadataUtils.isPositionSticky(element) ||
          MetadataUtils.isPositionAbsolute(element)
        ),
    ).length > 0
  )
}

export const hugContentsApplicableForText = (
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean => {
  const element = MetadataUtils.getJSXElementFromMetadata(metadata, elementPath)
  return optionalMap(elementOnlyHasTextChildren, element) === true
}

export const fillContainerApplicable = (
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean => {
  return !EP.isStoryboardChild(elementPath) && !treatElementAsGroupLike(metadata, elementPath)
}

export function justifyContentAlignItemsEquals(
  flexDirection: FlexDirection,
  one: JustifyContentFlexAlignemt,
  other: JustifyContentFlexAlignemt,
): boolean {
  const { justifyContent, alignItems } = one
  return isFlexColumn(flexDirection)
    ? alignItems === other.justifyContent && justifyContent === other.alignItems
    : alignItems === other.alignItems && justifyContent === other.justifyContent
}

export type Axis = 'horizontal' | 'vertical'

export function invert(axis: Axis): Axis {
  switch (axis) {
    case 'horizontal':
      return 'vertical'
    case 'vertical':
      return 'horizontal'
    default:
      assertNever(axis)
  }
}

export function widthHeightFromAxis(axis: Axis): 'width' | 'height' {
  switch (axis) {
    case 'horizontal':
      return 'width'
    case 'vertical':
      return 'height'
    default:
      assertNever(axis)
  }
}

export function childIs100PercentSizedInEitherDirection(
  metadataMap: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [childHorizontal100: boolean, childVertical100: boolean] {
  return [
    childIs100PercentSizedInDirection(metadataMap, elementPath, 'row'),
    childIs100PercentSizedInDirection(metadataMap, elementPath, 'column'),
  ]
}

function childIs100PercentSizedInDirection(
  metadataMap: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  parentFlexDirection: FlexDirection,
): boolean {
  const element = MetadataUtils.getJSXElementFromMetadata(metadataMap, elementPath)
  const metadata = MetadataUtils.findElementByElementPath(metadataMap, elementPath)
  if (element == null || metadata == null) {
    return false
  }
  const prop = parentFlexDirection.startsWith('row') ? 'width' : 'height'

  const matches =
    defaultEither(
      null,
      getSimpleAttributeAtPath(right(element.props), PP.create('style', prop)),
    ) === '100%'

  return matches
}

export function convertWidthToFlexGrowOptionally(
  metadataMap: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  parentFlexDirection: FlexDirection,
): Array<CanvasCommand> {
  if (!childIs100PercentSizedInDirection(metadataMap, elementPath, parentFlexDirection)) {
    return []
  }

  const prop = parentFlexDirection.startsWith('row') ? 'width' : 'height'

  return [
    deleteProperties('always', elementPath, [PP.create('style', prop)]),
    setProperty('always', elementPath, PP.create('style', 'flexGrow'), 1),
  ]
}

export function nullOrNonEmpty<T>(ts: Array<T>): Array<T> | null {
  return ts.length === 0 ? null : ts
}

export const styleP = <K extends keyof CSSProperties>(prop: K): PropertyPath<['style', K]> =>
  PP.create('style', prop)

export const flexContainerProps = [
  styleP('flexDirection'),
  styleP('flexWrap'),
  styleP('gap'),
  styleP('display'),
  styleP('alignItems'),
  styleP('justifyContent'),
]

export const flexChildProps = [
  styleP('flex'),
  styleP('flexGrow'),
  styleP('flexShrink'),
  styleP('flexBasis'),
]

export function pruneFlexPropsCommands(
  props: PropertyPath[],
  elementPath: ElementPath,
): Array<CanvasCommand> {
  return [deleteProperties('always', elementPath, props)]
}

export function isElementDisplayInline(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean {
  return (
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements
      ?.display === 'inline'
  )
}

export function isIntrinsicallyInlineElement(element: ElementInstanceMetadata | null): boolean {
  if (element == null) {
    return false
  }

  const jsxElementOfElement = defaultEither(null, element.element)
  return (
    jsxElementOfElement?.type === 'JSX_ELEMENT' &&
    inlineHtmlElements.some((e) =>
      jsxElementNameEquals(jsxElementName(e, []), jsxElementOfElement.name),
    )
  )
}

export function sizeToVisualDimensions(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const globalFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (globalFrame == null || isInfinityRectangle(globalFrame)) {
    return []
  }

  const width = fixedSizeDimensionHandlingText(metadata, pathTrees, elementPath, globalFrame.width)
  const height = globalFrame.height

  return [
    ...pruneFlexPropsCommands(flexChildProps, elementPath),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('width'),
      setExplicitCssValue(cssPixelLength(width)),
      element.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('height'),
      setExplicitCssValue(cssPixelLength(height)),
      element.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
  ]
}

export const sizeToVisualDimensionsAlongAxisInstance =
  (axis: Axis, instance: ElementInstanceMetadata) =>
  (elementPath: ElementPath): Array<CanvasCommand> => {
    const globalFrame = instance.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return []
    }

    const dimension = widthHeightFromAxis(axis)

    const value = globalFrame[dimension]

    return [
      ...pruneFlexPropsCommands(flexChildProps, elementPath),
      setCssLengthProperty(
        'always',
        elementPath,
        styleP(dimension),
        setExplicitCssValue(cssPixelLength(value)),
        instance.specialSizeMeasurements.parentFlexDirection ?? null,
      ),
    ]
  }

export const nukeSizingPropsForAxisCommand = (axis: Axis, path: ElementPath): CanvasCommand => {
  switch (axis) {
    case 'horizontal':
      return deleteProperties('always', path, [
        PP.create('style', 'width'),
        PP.create('style', 'minWidth'),
        PP.create('style', 'maxWidth'),
      ])
    case 'vertical':
      return deleteProperties('always', path, [
        PP.create('style', 'height'),
        PP.create('style', 'minHeight'),
        PP.create('style', 'maxHeight'),
      ])
    default:
      assertNever(axis)
  }
}

export const nukePositioningPropsForAxisCommand = (
  axis: Axis,
  path: ElementPath,
): CanvasCommand => {
  switch (axis) {
    case 'horizontal':
      return deleteProperties('always', path, [
        PP.create('style', 'left'),
        PP.create('style', 'right'),
      ])
    case 'vertical':
      return deleteProperties('always', path, [
        PP.create('style', 'top'),
        PP.create('style', 'bottom'),
      ])
    default:
      assertNever(axis)
  }
}

export const nukeAllAbsolutePositioningPropsCommands = (
  path: ElementPath,
): Array<CanvasCommand> => {
  return [
    addContainLayoutIfNeeded('always', path),
    deleteProperties('always', path, [
      PP.create('style', 'position'),
      PP.create('style', 'left'),
      PP.create('style', 'right'),
      PP.create('style', 'top'),
      PP.create('style', 'bottom'),
    ]),
  ]
}

export type FixedHugFill =
  | { type: 'fixed'; value: CSSNumber }
  | { type: 'fill'; value: CSSNumber }
  | { type: 'hug' }
  | { type: 'hug-group'; value: CSSNumber } // hug-group has a Fixed value but shows us Hug on the UI to explain Group behavior
  | { type: 'computed'; value: CSSNumber }
  | { type: 'detected'; value: CSSNumber }

export type FixedHugFillMode = FixedHugFill['type']

export function detectFillHugFixedState(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): { fixedHugFill: FixedHugFill | null; controlStatus: ControlStatus } {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return { fixedHugFill: null, controlStatus: 'off' }
  }

  const flexGrowLonghand = foldEither(
    () => null,
    (value) => defaultEither(null, parseCSSNumber(value, 'Unitless')),
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', 'flexGrow')),
  )

  const flexGrow =
    flexGrowLonghand ??
    foldEither(
      () => null,
      (value) => {
        return foldEither(
          () => null,
          (parsedFlexProp) => cssNumber(parsedFlexProp.flexGrow),
          parseFlex(value),
        )
      },
      getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', 'flex')),
    )

  const flexGrowStatus = getFallbackControlStatusForProperty(
    'flexGrow',
    element.element.value.props,
    element.attributeMetadatada,
  )

  if (flexGrow != null || flexGrowStatus !== 'detected') {
    // instead of the fallback detected flexgrow the control shows computed frame values
    const flexDirection = optionalMap(
      (e) => detectFlexDirectionOne(metadata, EP.parentPath(e)), // TODO fix flex parent, the parent may not be found at parentpath
      elementPath,
    )

    const isFlexDirectionHorizontal = flexDirection === 'row' || flexDirection === 'row-reverse'
    const isFlexDirectionVertical = flexDirection === 'column' || flexDirection === 'column-reverse'

    const flexGrowDetectedValue = defaultEither(
      null,
      parseCSSNumber(element.computedStyle?.flexGrow, 'Unitless'),
    )

    const isAxisMatchingFlexDirection =
      (axis === 'horizontal' && isFlexDirectionHorizontal) ||
      (axis === 'vertical' && isFlexDirectionVertical)

    if (isAxisMatchingFlexDirection) {
      if (flexGrow != null) {
        const valueWithType = { type: 'fill' as const, value: flexGrow }
        return { fixedHugFill: valueWithType, controlStatus: 'simple' }
      }
      if (flexGrowDetectedValue != null) {
        const valueWithType = { type: 'fill' as const, value: flexGrowDetectedValue }
        return { fixedHugFill: valueWithType, controlStatus: flexGrowStatus }
      }
    }
  }

  const property = widthHeightFromAxis(axis)

  const simpleAttribute = defaultEither(
    null,
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', property)),
  )

  if (isHugFromStyleAttribute(element.element.value.props, property)) {
    const valueWithType = { type: 'hug' as const }
    return { fixedHugFill: valueWithType, controlStatus: 'simple' }
  }

  const isGroupLike = treatElementAsGroupLike(metadata, elementPath)

  const parsed = defaultEither(null, parseCSSLengthPercent(simpleAttribute))
  if (parsed != null && parsed.unit === '%') {
    const valueWithType: FixedHugFill = { type: isGroupLike ? 'hug-group' : 'fill', value: parsed }
    return { fixedHugFill: valueWithType, controlStatus: 'simple' }
  }

  if (parsed != null) {
    const valueWithType: FixedHugFill = { type: isGroupLike ? 'hug-group' : 'fixed', value: parsed }
    return { fixedHugFill: valueWithType, controlStatus: 'simple' }
  }

  const frame = element.globalFrame
  if (frame != null && isFiniteRectangle(frame)) {
    const dimension = widthHeightFromAxis(axis)

    const controlStatus = getFallbackControlStatusForProperty(
      property,
      element.element.value.props,
      element.attributeMetadatada,
    )

    const valueWithType: FixedHugFill = {
      type: isGroupLike ? 'hug-group' : controlStatus === 'controlled' ? 'computed' : 'detected',
      value: cssNumber(frame[dimension]),
    }
    return { fixedHugFill: valueWithType, controlStatus: controlStatus }
  }

  return { fixedHugFill: null, controlStatus: 'unset' }
}

export function isFixedHugFillModeApplied(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
  mode: FixedHugFillMode,
): boolean {
  return (
    detectFillHugFixedState('horizontal', metadata, element).fixedHugFill?.type === mode &&
    detectFillHugFixedState('vertical', metadata, element).fixedHugFill?.type === mode
  )
}

export function isFixedHugFillModeAppliedOnAnySide(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
  mode: FixedHugFillMode,
): boolean {
  return (
    detectFillHugFixedState('horizontal', metadata, element).fixedHugFill?.type === mode ||
    detectFillHugFixedState('vertical', metadata, element).fixedHugFill?.type === mode
  )
}

export function setToFixedSizeCommands(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: ElementProps,
  targetElements: Array<ElementPath>,
): Array<CanvasCommand> {
  return targetElements.flatMap((targetElement) => {
    const parentPath = EP.parentPath(targetElement)
    const isChildOfGroup = treatElementAsGroupLike(metadata, parentPath)
    const isGroup = treatElementAsGroupLike(metadata, targetElement)
    // Only convert the group to a frame if it is not itself a child of a group.
    if (isGroup && !isChildOfGroup) {
      return convertGroupToFrameCommands(metadata, elementPathTree, allElementProps, targetElement)
    } else {
      return sizeToVisualDimensions(metadata, elementPathTree, targetElement)
    }
  })
}

export function isHugFromStyleAttribute(
  props: JSXAttributes,
  property: 'width' | 'height',
): boolean {
  const simpleAttribute = defaultEither(
    null,
    getSimpleAttributeAtPath(right(props), PP.create('style', property)),
  )

  return simpleAttribute === MaxContent
}

export function isHugFromStyleAttributeOrNull(
  props: JSXAttributes | null,
  property: 'width' | 'height',
): boolean {
  if (props == null) {
    return false
  }
  return isHugFromStyleAttribute(props, property)
}

export function detectFillHugFixedStateMultiselect(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): { fixedHugFill: FixedHugFill | null; controlStatus: ControlStatus } {
  if (elementPaths.length === 1) {
    return detectFillHugFixedState(axis, metadata, elementPaths[0])
  } else {
    const results = elementPaths.map((path) => detectFillHugFixedState(axis, metadata, path))
    let controlStatus: ControlStatus = results[0]?.controlStatus ?? 'off'
    let value: FixedHugFill | null = results[0]?.fixedHugFill

    fastForEach(results, (result) => {
      if (!isFixedHugFillEqual(result, results[0])) {
        controlStatus = 'multiselect-mixed-simple-or-unset'
      }
    })

    const allControlStatus = uniq(results.map((result) => result.controlStatus))
    if (allControlStatus.includes('unoverwritable')) {
      controlStatus = 'multiselect-unoverwritable'
    } else if (allControlStatus.includes('controlled')) {
      controlStatus = 'multiselect-controlled'
    }

    return { fixedHugFill: value, controlStatus: controlStatus }
  }
}

export const MaxContent = 'max-content' as const

export type PackedSpaced = 'packed' | 'spaced'

function detectPackedSpacedSettingInner(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): PackedSpaced {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  return element?.specialSizeMeasurements.justifyContent === 'space-between' ? 'spaced' : 'packed'
}

export function detectPackedSpacedSetting(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): PackedSpaced | null {
  if (elementPaths.length === 0) {
    return null
  }
  const detectedPackedSpacedSettings = elementPaths.map((path) =>
    detectPackedSpacedSettingInner(metadata, path),
  )
  return allElemsEqual(detectedPackedSpacedSettings)
    ? detectedPackedSpacedSettings.at(0) ?? null
    : null
}

export function resizeToFitCommands(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): Array<CanvasCommand> {
  const commands = [
    ...(commandsForFirstApplicableStrategy(
      metadata,
      selectedViews,
      elementPathTree,
      allElementProps,
      setPropHugStrategies('horizontal'),
    ) ?? []),
    ...(commandsForFirstApplicableStrategy(
      metadata,
      selectedViews,
      elementPathTree,
      allElementProps,
      setPropHugStrategies('vertical'),
    ) ?? []),
  ]
  return commands
}

export function resizeToFillCommands(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): Array<CanvasCommand> {
  const commands = [
    ...(commandsForFirstApplicableStrategy(
      metadata,
      selectedViews,
      elementPathTree,
      allElementProps,
      setPropFillStrategies('horizontal', 'default', false),
    ) ?? []),
    ...(commandsForFirstApplicableStrategy(
      metadata,
      selectedViews,
      elementPathTree,
      allElementProps,
      setPropFillStrategies('vertical', 'default', false),
    ) ?? []),
  ]
  return commands
}

export function addPositionAbsoluteTopLeft(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const left = element.specialSizeMeasurements.offset.x
  const top = element.specialSizeMeasurements.offset.y

  const parentFlexDirection = element.specialSizeMeasurements.parentFlexDirection

  return [
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('left'),
      setExplicitCssValue(cssPixelLength(left)),
      parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('top'),
      setExplicitCssValue(cssPixelLength(top)),
      parentFlexDirection,
    ),
    setProperty('always', elementPath, styleP('position'), 'absolute'),
  ]
}

export function setElementTopLeft(
  instance: ElementInstanceMetadata,
  { top, left }: { top: number; left: number },
): Array<CanvasCommand> {
  return [
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(top, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(left, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
  ]
}

export function toggleResizeToFitSetToFixed(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): Array<CanvasCommand> {
  const firstElementPath = safeIndex(elementPaths, 0)
  if (firstElementPath == null || elementPaths.length < 1) {
    return []
  }

  // Note: This is checking the first but the changes apply to everything...
  const isSetToHug = isFixedHugFillModeApplied(metadata, firstElementPath, 'hug')

  return isSetToHug
    ? setToFixedSizeCommands(metadata, elementPathTree, allElementProps, elementPaths)
    : resizeToFitCommands(metadata, elementPaths, elementPathTree, allElementProps)
}

function fixedOrHugForGroup(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): 'fixed' | 'hug-group' {
  return treatElementAsGroupLike(metadata, path) ? 'hug-group' : 'fixed'
}

export function getFixedFillHugOptionsForElement(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  selectedView: ElementPath,
): Set<FixedHugFillMode> {
  return new Set(
    stripNulls([
      fixedOrHugForGroup(metadata, selectedView),
      hugContentsApplicableForText(metadata, selectedView) ||
      hugContentsApplicableForContainer(metadata, pathTrees, selectedView)
        ? 'hug'
        : null,
      fillContainerApplicable(metadata, selectedView) ? 'fill' : null,
    ]),
  )
}

export function getFillFixedHugOptions(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  selectedViews: Array<ElementPath>,
): Array<FixedHugFillMode> {
  return [
    ...intersection(
      selectedViews.map((selectedView) =>
        getFixedFillHugOptionsForElement(metadata, pathTrees, selectedView),
      ),
    ),
  ]
}

export function setParentToFixedIfHugCommands(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const parentInstance = MetadataUtils.findElementByElementPath(metadata, parentPath)
  if (parentInstance == null) {
    return []
  }

  const isHug = detectFillHugFixedState(axis, metadata, parentPath).fixedHugFill?.type === 'hug'
  if (!isHug) {
    return []
  }

  const globalFrame = MetadataUtils.getFrame(parentPath, metadata)
  if (globalFrame == null || isInfinityRectangle(globalFrame)) {
    return []
  }
  const prop = widthHeightFromAxis(axis)
  const dimension = globalFrame[prop]

  return [
    showToastCommand(
      `Parent set to fixed size along the ${axis} axis`,
      'INFO',
      `setParentToFixedIfHugCommands-${EP.toString(parentPath)}-${axis}`,
    ),
    setCssLengthProperty(
      'always',
      parentPath,
      styleP(prop),
      setExplicitCssValue(cssPixelLength(dimension)),
      parentInstance.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
  ]
}

export function getFramePointsFromMetadata(elementMetadata: ElementInstanceMetadata): Frame {
  if (isRight(elementMetadata.element) && isJSXElement(elementMetadata.element.value)) {
    const jsxElement = elementMetadata.element.value
    return LayoutPinnedProps.reduce<Frame>((working, point) => {
      const value = getLayoutLengthValueOrKeyword(point, right(jsxElement.props), ['style'])
      if (isLeft(value)) {
        return working
      } else {
        return {
          ...working,
          [point]: value.value,
        }
      }
    }, {})
  } else {
    return {}
  }
}

export function removeExtraPinsWhenSettingSize(
  axis: Axis,
  elementMetadata: ElementInstanceMetadata | null,
): Array<CanvasCommand> {
  if (elementMetadata == null) {
    return []
  }
  const framePinValues = getFramePointsFromMetadata(elementMetadata)
  const newFrameProp = axis === 'horizontal' ? 'width' : 'height'
  const pinsToDelete = getPinsToDelete(newFrameProp, framePinValues, null, null)

  return pinsToDelete.map((frameProp) =>
    deleteProperties('always', elementMetadata.elementPath, [styleP(frameProp)]),
  )
}

export function isFixedHugFillEqual(
  a: { fixedHugFill: FixedHugFill | null; controlStatus: ControlStatus },
  b: { fixedHugFill: FixedHugFill | null; controlStatus: ControlStatus },
): boolean {
  if (a.fixedHugFill == null && b.fixedHugFill == null && a.controlStatus === b.controlStatus) {
    return true
  }

  if (a.controlStatus !== b.controlStatus) {
    return false
  }

  if (a.fixedHugFill == null || b.fixedHugFill == null) {
    return false
  }

  switch (a.fixedHugFill.type) {
    case 'hug':
      return b.fixedHugFill.type === 'hug'
    case 'fill':
    case 'fixed':
    case 'computed':
    case 'detected':
    case 'hug-group':
      return (
        a.fixedHugFill.type === b.fixedHugFill.type &&
        a.fixedHugFill.value.value === b.fixedHugFill.value.value &&
        a.fixedHugFill.value.unit === b.fixedHugFill.value.unit
      )
    default:
      const _exhaustiveCheck: never = a.fixedHugFill
      throw new Error(`Unknown type in FixedHugFill ${JSON.stringify(a.fixedHugFill)}`)
  }
}

export function toggleAbsolutePositioningCommands(
  jsxMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
): Array<CanvasCommand> {
  const commands = selectedViews.flatMap((elementPath) => {
    const maybeGroupConversionCommands = groupConversionCommands(
      jsxMetadata,
      allElementProps,
      elementPathTree,
      elementPath,
    )

    if (maybeGroupConversionCommands != null) {
      return maybeGroupConversionCommands
    }

    const element = MetadataUtils.findElementByElementPath(jsxMetadata, elementPath)
    if (element == null) {
      return []
    }

    if (
      MetadataUtils.isFragmentFromMetadata(element) ||
      MetadataUtils.isConditionalFromMetadata(element)
    ) {
      return []
    }

    if (MetadataUtils.isPositionAbsolute(element)) {
      return [
        ...nukeAllAbsolutePositioningPropsCommands(elementPath),
        ...(isIntrinsicallyInlineElement(element)
          ? [
              ...sizeToVisualDimensions(jsxMetadata, elementPathTree, elementPath),
              setProperty('always', elementPath, PP.create('style', 'display'), 'inline-block'),
            ]
          : []),
      ]
    } else {
      return [
        ...sizeToVisualDimensions(jsxMetadata, elementPathTree, elementPath),
        ...addPositionAbsoluteTopLeft(jsxMetadata, elementPath),
      ]
    }
  })

  return commands
}
