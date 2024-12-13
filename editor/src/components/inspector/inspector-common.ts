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
import { assertNever } from '../../core/shared/utils'
import { defaultEither, foldEither, isLeft, isRight, right } from '../../core/shared/either'
import { elementOnlyHasTextChildren } from '../../core/model/element-template-utils'
import { optionalMap } from '../../core/shared/optional-utils'
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
  setPropHugAbsoluteStrategies,
} from './inspector-strategies/inspector-strategies'
import { commandsForFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { CanvasVector, Size } from '../../core/shared/math-utils'
import {
  isFiniteRectangle,
  isInfinityRectangle,
  roundRectangleToNearestWhole,
} from '../../core/shared/math-utils'
import type { LocalRectangle, MaybeInfinityCanvasRectangle } from '../../core/shared/math-utils'
import { inlineHtmlElements } from '../../utils/html-elements'
import { intersection } from '../../core/shared/set-utils'
import { showToastCommand } from '../canvas/commands/show-toast-command'
import { parseFlex } from '../../printer-parsers/css/css-parser-flex'
import type { LayoutPinnedProp } from '../../core/layout/layout-helpers-new'
import { isLayoutPinnedProp, LayoutPinnedProps } from '../../core/layout/layout-helpers-new'
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
import { convertToAbsolute } from '../canvas/commands/convert-to-absolute-command'
import { hugPropertiesFromStyleMap } from '../../core/shared/dom-utils'
import { setHugContentForAxis } from './inspector-strategies/hug-contents-strategy'

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

export type AlignContent =
  | 'normal'
  | 'start'
  | 'center'
  | 'end'
  | 'flex-start'
  | 'flex-end'
  | 'baseline'
  | 'first baseline'
  | 'last baseline'
  | 'space-between'
  | 'space-around'
  | 'space-evenly'
  | 'stretch'
  | 'safe center'
  | 'unsafe center'
  | 'inherit'
  | 'initial'
  | 'revert'
  | 'revert-layer'
  | 'unset'

export function getAlignContent(value: string | null): AlignContent | null {
  switch (value) {
    case 'normal':
      return 'normal'
    case 'start':
      return 'start'
    case 'center':
      return 'center'
    case 'end':
      return 'end'
    case 'flex-start':
      return 'flex-start'
    case 'flex-end':
      return 'flex-end'
    case 'baseline':
      return 'baseline'
    case 'first baseline':
      return 'first baseline'
    case 'last baseline':
      return 'last baseline'
    case 'space-between':
      return 'space-between'
    case 'space-around':
      return 'space-around'
    case 'space-evenly':
      return 'space-evenly'
    case 'stretch':
      return 'stretch'
    case 'safe center':
      return 'safe center'
    case 'unsafe center':
      return 'unsafe center'
    case 'inherit':
      return 'inherit'
    case 'initial':
      return 'initial'
    case 'revert':
      return 'revert'
    case 'revert-layer':
      return 'revert-layer'
    case 'unset':
      return 'unset'
    default:
      return null
  }
}

export type FlexAlignment = StartCenterEnd | 'auto' | 'stretch'
export type SelfAlignment = FlexAlignment | 'end' | 'start'

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

export function getSelfAlignment(value: string | null): SelfAlignment | null {
  switch (value) {
    case 'end':
      return 'end'
    case 'start':
      return 'start'
    default:
      return getFlexAlignment(value)
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

export function filterKeepGridContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): ElementPath[] {
  return elementPaths.filter((e: ElementPath | null) =>
    MetadataUtils.isGridLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, e)),
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

export function detectAreElementsGridContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): boolean {
  return strictEvery(elementPaths, (path) =>
    MetadataUtils.isGridLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, path)),
  )
}

export const isFlexColumn = (flexDirection: FlexDirection): boolean =>
  flexDirection.startsWith('column')

export const basicHugContentsApplicableForContainer = (
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean => {
  const isNonFixStickOrAbsolute =
    mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      MetadataUtils.getChildrenPathsOrdered(pathTrees, elementPath),
    ).filter(
      (element) =>
        !(
          MetadataUtils.isPositionFixed(element) ||
          MetadataUtils.isPositionSticky(element) ||
          MetadataUtils.isPositionAbsolute(element)
        ),
    ).length > 0

  const isGrid = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(metadata, elementPath),
  )

  return isNonFixStickOrAbsolute && !isGrid
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
): { childWidth100Percent: boolean; childHeight100Percent: boolean } {
  return {
    childWidth100Percent: childIs100PercentSizedInDirection(metadataMap, elementPath, 'row'),
    childHeight100Percent: childIs100PercentSizedInDirection(metadataMap, elementPath, 'column'),
  }
}

export function onlyChildIsSpan(
  metadataMap: ElementInstanceMetadataMap,
  childrenPaths: ElementPath[],
): boolean {
  if (childrenPaths.length !== 1) {
    return false
  }
  return (
    optionalMap(
      (i) => MetadataUtils.isSpan(i),
      MetadataUtils.findElementByElementPath(metadataMap, childrenPaths[0]),
    ) ?? false
  )
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

export const gridContainerProps = [
  styleP('gap'),
  styleP('gridGap'),
  styleP('display'),
  styleP('gridTemplateRows'),
  styleP('gridTemplateColumns'),
  styleP('gridAutoColumns'),
  styleP('gridAutoRows'),
  styleP('rowGap'),
  styleP('columnGap'),
]

export const gridElementProps = [
  styleP('gridColumn'),
  styleP('gridColumnStart'),
  styleP('gridColumnEnd'),
  styleP('gridRow'),
  styleP('gridRowStart'),
  styleP('gridRowEnd'),
]

export const flexChildProps = [
  styleP('flex'),
  styleP('flexGrow'),
  styleP('flexShrink'),
  styleP('flexBasis'),
]

export const flexChildAndBottomRightProps = [...flexChildProps, styleP('bottom'), styleP('right')]

export function prunePropsCommands(
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
  const globalFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (globalFrame == null || isInfinityRectangle(globalFrame)) {
    return []
  }

  return sizeToDimensionsFromFrame(metadata, pathTrees, elementPath, globalFrame)
}

export function sizeToDimensionsFromFrame(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
  frame: { width: number; height: number },
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const width = fixedSizeDimensionHandlingText(metadata, pathTrees, elementPath, frame.width)
  const height = frame.height

  return [
    ...prunePropsCommands(flexChildAndBottomRightProps, elementPath),
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
      ...prunePropsCommands(flexChildProps, elementPath),
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

export const nukeGridCellPositioningPropsCommands = (path: ElementPath): Array<CanvasCommand> => {
  return [
    deleteProperties('always', path, [
      PP.create('style', 'gridColumn'),
      PP.create('style', 'gridColumnStart'),
      PP.create('style', 'gridColumnEnd'),
      PP.create('style', 'gridRow'),
      PP.create('style', 'gridRowStart'),
      PP.create('style', 'gridRowEnd'),
    ]),
  ]
}

export type FixedHugFill =
  | { type: 'fixed'; value: CSSNumber }
  | { type: 'fill'; value: CSSNumber }
  | { type: 'hug' }
  | { type: 'squeeze' }
  | { type: 'collapsed' }
  | { type: 'hug-group'; value: CSSNumber } // hug-group has a Fixed value but shows us Hug on the UI to explain Group behavior
  | { type: 'computed'; value: CSSNumber }
  | { type: 'detected'; value: CSSNumber }
  | { type: 'scaled'; value: CSSNumber }
  | { type: 'stretch' }

export type FixedHugFillMode = FixedHugFill['type']

export function isHuggingFixedHugFill(fixedHugFillMode: FixedHugFillMode | null | undefined) {
  return (
    fixedHugFillMode === 'hug' || fixedHugFillMode === 'squeeze' || fixedHugFillMode === 'collapsed'
  )
}

export interface DetectedFillHugFixedState {
  fixedHugFill: FixedHugFill | null
  controlStatus: ControlStatus
}

export function detectFillHugFixedState(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): DetectedFillHugFixedState {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return { fixedHugFill: null, controlStatus: 'off' }
  }

  const width = foldEither(
    () => null,
    (value) => defaultEither(null, parseCSSNumber(value, 'Unitless')),
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', 'width')),
  )
  const height = foldEither(
    () => null,
    (value) => defaultEither(null, parseCSSNumber(value, 'Unitless')),
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', 'height')),
  )

  if (MetadataUtils.isGridItem(metadata, elementPath)) {
    const isStretchingExplicitly =
      (element.specialSizeMeasurements.alignSelf === 'stretch' &&
        axis === 'horizontal' &&
        width == null) ||
      (element.specialSizeMeasurements.justifySelf === 'stretch' &&
        axis === 'vertical' &&
        height == null)

    const isStretchingImplicitly =
      width == null &&
      height == null &&
      (element.specialSizeMeasurements.alignSelf == null ||
        element.specialSizeMeasurements.alignSelf === 'auto') &&
      (element.specialSizeMeasurements.justifySelf == null ||
        element.specialSizeMeasurements.justifySelf === 'auto')

    if (isStretchingExplicitly || isStretchingImplicitly) {
      return { fixedHugFill: { type: 'stretch' }, controlStatus: 'detected' }
    }
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
    element.attributeMetadata,
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

  const detectedHugType = element.specialSizeMeasurements.computedHugProperty[property]
  if (detectedHugType != null) {
    const hugTypeFromStyleProps = hugTypeFromStyleAttribute(
      element.element.value.props,
      property,
      element.specialSizeMeasurements.display,
      element.globalFrame,
    )
    const controlStatus: ControlStatus = (() => {
      if (detectedHugType !== hugTypeFromStyleProps) {
        return 'detected-fromcss'
      }
      if (detectedHugType === 'squeeze' || detectedHugType === 'collapsed') {
        return hugTypeFromStyleProps != null ? 'simple' : 'detected-fromcss'
      }
      return 'simple'
    })()

    const valueWithType = { type: detectedHugType }
    return { fixedHugFill: valueWithType, controlStatus: controlStatus }
  }

  const isGroupLike = treatElementAsGroupLike(metadata, elementPath)

  const parsed = defaultEither(null, parseCSSLengthPercent(simpleAttribute))
  if (parsed != null && parsed.unit === '%') {
    const type = (() => {
      if (isGroupLike) {
        return 'hug-group'
      }
      if (parsed.value === 100) {
        return 'fill'
      }
      return 'scaled'
    })()

    const valueWithType: FixedHugFill = {
      type: type,
      value: parsed,
    }
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
      element.attributeMetadata,
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

export function isFillOrStretchModeApplied(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
): boolean {
  return (
    isFixedHugFillModeApplied(metadata, element, 'fill') ||
    isFixedHugFillModeApplied(metadata, element, 'stretch')
  )
}

export function isFillOrStretchModeAppliedOnAnySide(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
): boolean {
  return (
    isFixedHugFillModeAppliedOnAnySide(metadata, element, 'fill') ||
    isFixedHugFillModeAppliedOnAnySide(metadata, element, 'stretch')
  )
}

export function isFillOrStretchModeAppliedOnSpecificSide(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
  side: 'horizontal' | 'vertical',
): boolean {
  return (
    detectFillHugFixedState(side, metadata, element).fixedHugFill?.type === 'fill' ||
    detectFillHugFixedState(side, metadata, element).fixedHugFill?.type === 'stretch'
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

const HuggingWidthHeightValues = ['max-content', 'min-content', 'fit-content', 'auto']

export function isHugFromStyleAttribute(
  props: JSXAttributes,
  property: 'width' | 'height',
  includeAllHugs: 'include-all-hugs' | 'only-max-content',
): boolean {
  const simpleAttribute = defaultEither(
    null,
    getSimpleAttributeAtPath(right(props), PP.create('style', property)),
  )

  if (includeAllHugs === 'only-max-content') {
    return simpleAttribute === MaxContent
  }

  // TODO simpleAttribute == null is not good enough here, see https://github.com/concrete-utopia/utopia/pull/4389#discussion_r1363594423
  return simpleAttribute == null || HuggingWidthHeightValues.includes(simpleAttribute)
}

export function hugTypeFromStyleAttribute(
  props: JSXAttributes,
  property: 'width' | 'height',
  display: string,
  globalFrame: MaybeInfinityCanvasRectangle | null,
): 'hug' | 'squeeze' | 'collapsed' | null {
  const getStyleValue = (prop: string) =>
    prop === 'display'
      ? display
      : defaultEither(null, getSimpleAttributeAtPath(right(props), PP.create('style', prop)))

  const hugProperties = hugPropertiesFromStyleMap(getStyleValue, globalFrame)

  return hugProperties[property]
}

export function isHugFromStyleAttributeOrNull(
  props: JSXAttributes | null,
  property: 'width' | 'height',
  includeAllHugs: 'include-all-hugs' | 'only-max-content',
): boolean {
  if (props == null) {
    return includeAllHugs === 'include-all-hugs' // null size means implicit hug!
  }
  return isHugFromStyleAttribute(props, property, includeAllHugs)
}

export function detectFillHugFixedStateMultiselect(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): { fixedHugFill: FixedHugFill | null; controlStatus: ControlStatus } {
  if (elementPaths.length === 1) {
    return detectFillHugFixedState(axis, metadata, elementPaths[0])
  } else {
    function fixedHugFillWithControlStatus(
      fixedHugFill: FixedHugFill | null,
      controlStatus: ControlStatus,
    ) {
      return {
        fixedHugFill: fixedHugFill,
        controlStatus: controlStatus,
      }
    }

    const results = elementPaths.map((path) => detectFillHugFixedState(axis, metadata, path))
    const value: FixedHugFill | null = results[0]?.fixedHugFill

    const isMixed = results.some((result) => {
      return !isFixedHugFillEqual(result, results[0])
    })
    if (isMixed) {
      return fixedHugFillWithControlStatus(value, 'multiselect-mixed-simple-or-unset')
    }

    const allControlStatus = uniq(results.map((result) => result.controlStatus))
    if (allControlStatus.includes('unoverwritable')) {
      return fixedHugFillWithControlStatus(value, 'multiselect-unoverwritable')
    } else if (allControlStatus.includes('controlled')) {
      return fixedHugFillWithControlStatus(value, 'multiselect-controlled')
    } else {
      return fixedHugFillWithControlStatus(value, results[0]?.controlStatus ?? 'off')
    }
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
      setPropHugStrategies(metadata, selectedViews, elementPathTree, 'horizontal'),
    ) ?? []),
    ...(commandsForFirstApplicableStrategy(
      setPropHugStrategies(metadata, selectedViews, elementPathTree, 'vertical'),
    ) ?? []),
  ]

  if (commands.length == 0) {
    return (
      commandsForFirstApplicableStrategy(
        setPropHugAbsoluteStrategies(metadata, selectedViews, elementPathTree, allElementProps),
      ) ?? []
    )
  }
  return commands
}

export function resizeToFillCommands(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
): Array<CanvasCommand> {
  const commands = [
    ...(commandsForFirstApplicableStrategy(
      setPropFillStrategies(metadata, selectedViews, 'horizontal', 'default', false),
    ) ?? []),
    ...(commandsForFirstApplicableStrategy(
      setPropFillStrategies(metadata, selectedViews, 'vertical', 'default', false),
    ) ?? []),
  ]
  return commands
}

function addPositionAbsoluteTopLeft(
  elementPath: ElementPath,
  localFrame: LocalRectangle,
  parentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  return [
    convertToAbsolute('always', elementPath),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('left'),
      setExplicitCssValue(cssPixelLength(localFrame.x)),
      parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('top'),
      setExplicitCssValue(cssPixelLength(localFrame.y)),
      parentFlexDirection,
    ),
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

export function getFixedFillHugOptionsForElement(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  selectedView: ElementPath,
): Set<FixedHugFillMode> {
  const isGroup = treatElementAsGroupLike(metadata, selectedView)
  return new Set(
    stripNulls([
      isGroup ? 'hug-group' : null,
      'fixed',
      hugContentsApplicableForText(metadata, selectedView) ||
      (!isGroup && basicHugContentsApplicableForContainer(metadata, pathTrees, selectedView))
        ? 'hug'
        : null,
      fillContainerApplicable(metadata, selectedView)
        ? MetadataUtils.isGridItem(metadata, selectedView)
          ? 'stretch'
          : 'fill'
        : null,
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

  const isHug = isHuggingFixedHugFill(
    detectFillHugFixedState(axis, metadata, parentPath).fixedHugFill?.type,
  )
  if (!isHug) {
    return []
  }

  const globalFrame = MetadataUtils.getLocalFrame(parentPath, metadata, null)
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

type GetFramePointsFromMetadataResult = {
  left?: CSSNumber | 'max-content' | undefined
  right?: CSSNumber | 'max-content' | undefined
  centerX?: CSSNumber | 'max-content' | undefined
  width?: CSSNumber | 'max-content' | undefined
  top?: CSSNumber | 'max-content' | undefined
  bottom?: CSSNumber | 'max-content' | undefined
  centerY?: CSSNumber | 'max-content' | undefined
  height?: CSSNumber | 'max-content' | undefined
}

export function getFramePointsFromMetadataTypeFixed(
  elementMetadata: ElementInstanceMetadata,
): GetFramePointsFromMetadataResult {
  if (isRight(elementMetadata.element) && isJSXElement(elementMetadata.element.value)) {
    const jsxElement = elementMetadata.element.value
    return LayoutPinnedProps.reduce<GetFramePointsFromMetadataResult>((working, point) => {
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

export function removeAlignJustifySelf(
  axis: Axis,
  elementMetadata: ElementInstanceMetadata | null,
): Array<CanvasCommand> {
  if (elementMetadata == null) {
    return []
  }
  return [
    deleteProperties('always', elementMetadata.elementPath, [
      styleP(axis === 'horizontal' ? 'alignSelf' : 'justifySelf'),
    ]),
  ]
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
    case 'squeeze':
    case 'collapsed':
      return a.fixedHugFill.type === b.fixedHugFill.type
    case 'fill':
    case 'fixed':
    case 'scaled':
    case 'computed':
    case 'detected':
    case 'hug-group':
      return (
        a.fixedHugFill.type === b.fixedHugFill.type &&
        a.fixedHugFill.value.value === b.fixedHugFill.value.value &&
        a.fixedHugFill.value.unit === b.fixedHugFill.value.unit
      )
    case 'stretch':
      return a.fixedHugFill.type === b.fixedHugFill.type
    default:
      assertNever(a.fixedHugFill)
  }
}

export function toggleAbsolutePositioningCommands(
  jsxMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  canvasContext: { scale: number; offset: CanvasVector },
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
      // First check if the parent is a group and prevent the removal of the position property in this case.
      const isGroupChild = treatElementAsGroupLike(jsxMetadata, EP.parentPath(elementPath))
      if (isGroupChild) {
        return [
          showToastCommand(
            'Cannot remove absolute position for group children.',
            'WARNING',
            'cannot-remove-group-child-absolute-position',
          ),
        ]
      } else {
        return [
          ...nukeAllAbsolutePositioningPropsCommands(elementPath),
          ...(isIntrinsicallyInlineElement(element)
            ? [
                ...sizeToVisualDimensions(jsxMetadata, elementPathTree, elementPath),
                setProperty('always', elementPath, PP.create('style', 'display'), 'inline-block'),
              ]
            : []),
        ]
      }
    } else {
      return getConvertIndividualElementToAbsoluteCommandsFromMetadata(
        elementPath,
        jsxMetadata,
        elementPathTree,
      )
    }
  })

  return commands
}

export function getConvertIndividualElementToAbsoluteCommandsFromMetadata(
  target: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): Array<CanvasCommand> {
  const localFrame = MetadataUtils.getLocalFrame(target, jsxMetadata, null)
  if (localFrame == null || isInfinityRectangle(localFrame)) {
    return []
  }

  const element = MetadataUtils.findElementByElementPath(jsxMetadata, target)
  if (element == null) {
    return []
  }

  const parentFlexDirection = element.specialSizeMeasurements.parentFlexDirection

  return getConvertIndividualElementToAbsoluteCommands(
    target,
    jsxMetadata,
    elementPathTree,
    localFrame,
    parentFlexDirection,
  )
}

export function getConvertIndividualElementToAbsoluteCommands(
  target: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  frame: LocalRectangle,
  parentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  // First round the frame so that we don't end up with half pixel values
  const roundedFrame = roundRectangleToNearestWhole(frame)
  return [
    ...nukeGridCellPositioningPropsCommands(target),
    ...sizeToDimensionsFromFrame(jsxMetadata, elementPathTree, target, roundedFrame),
    ...addPositionAbsoluteTopLeft(target, roundedFrame, parentFlexDirection),
  ]
}

export function setAutoWidthCommands(
  elementPath: ElementPath,
  parentFlexDirection: FlexDirection | null,
  computedHeight: number,
): CanvasCommand[] {
  return [
    setHugContentForAxis('horizontal', elementPath, parentFlexDirection),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('height'),
      setExplicitCssValue(cssPixelLength(computedHeight)),
      parentFlexDirection,
    ),
  ]
}

export function setAutoHeightCommands(
  elementPath: ElementPath,
  parentFlexDirection: FlexDirection | null,
  computedWidth: number,
): CanvasCommand[] {
  return [
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('width'),
      setExplicitCssValue(cssPixelLength(computedWidth)),
      parentFlexDirection,
    ),
    setHugContentForAxis('vertical', elementPath, parentFlexDirection),
  ]
}

export function setFixedSizeCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
  frame: Size,
) {
  return sizeToDimensionsFromFrame(metadata, pathTrees, elementPath, frame)
}

export function getSafeGroupChildConstraintsArray(
  allElementProps: AllElementProps,
  path: ElementPath,
): LayoutPinnedProp[] {
  const value = allElementProps[EP.toString(path)]?.['data-constraints'] ?? []
  if (!Array.isArray(value)) {
    return []
  }
  return value.filter((v) => typeof v === 'string' && isLayoutPinnedProp(v))
}

export function getConstraintsIncludingImplicitForElement(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  element: ElementPath,
  includeImplicitConstraints: 'include-max-content' | 'only-explicit-constraints',
) {
  let constraints: Set<LayoutPinnedProp> = new Set(
    getSafeGroupChildConstraintsArray(allElementProps, element),
  )

  if (includeImplicitConstraints === 'only-explicit-constraints') {
    return Array.from(constraints)
  }

  // collect implicit constraints
  const jsxElement = MetadataUtils.getJSXElementFromMetadata(metadata, element)
  if (jsxElement != null) {
    if (isHugFromStyleAttribute(jsxElement.props, 'width', 'only-max-content')) {
      constraints.add('width')
    }
    if (isHugFromStyleAttribute(jsxElement.props, 'height', 'only-max-content')) {
      constraints.add('height')
    }
  }

  return Array.from(constraints)
}

export function isHuggingParent(element: ElementInstanceMetadata, property: 'width' | 'height') {
  return element.specialSizeMeasurements.computedHugProperty[property] != null
}
