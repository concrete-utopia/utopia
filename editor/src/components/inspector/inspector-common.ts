import * as PP from '../../core/shared/property-path'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { isStoryboardChild, parentPath } from '../../core/shared/element-path'
import { mapDropNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { ElementPath, PropertyPath } from '../../core/shared/project-file-types'
import {
  CSSNumber,
  cssNumber,
  cssPixelLength,
  FlexDirection,
  parseCSSLengthPercent,
  parseCSSNumber,
} from './common/css-utils'
import { assertNever } from '../../core/shared/utils'
import { defaultEither, foldEither, isLeft, right } from '../../core/shared/either'
import { elementOnlyHasTextChildren } from '../../core/model/element-template-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { CSSProperties } from 'react'
import { CanvasCommand } from '../canvas/commands/commands'
import { deleteProperties } from '../canvas/commands/delete-properties-command'
import { setProperty } from '../canvas/commands/set-property-command'
import { addContainLayoutIfNeeded } from '../canvas/commands/add-contain-layout-if-needed-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../canvas/commands/set-css-length-command'
import { setPropHugStrategies } from './inspector-strategies/inspector-strategies'
import { commandsForFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'

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
  return elementPaths.every((path) =>
    MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, path)),
  )
}

export const isFlexColumn = (flexDirection: FlexDirection): boolean =>
  flexDirection.startsWith('column')

export const hugContentsApplicableForContainer = (
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean => {
  return (
    mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      MetadataUtils.getChildrenPaths(metadata, elementPath),
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

export const fillContainerApplicable = (elementPath: ElementPath): boolean =>
  !isStoryboardChild(elementPath)

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

function allElemsEqual<T>(objects: T[], areEqual: (a: T, b: T) => boolean): boolean {
  if (objects.length === 0) {
    return false
  }

  return objects.slice(1).every((obj) => areEqual(objects[0], obj))
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

export function sizeToVisualDimensions(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const width = element.specialSizeMeasurements.clientWidth
  const height = element.specialSizeMeasurements.clientHeight

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

export function detectFillHugFixedState(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath | null,
): FixedHugFill | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const flexGrow = foldEither(
    () => null,
    (value) => defaultEither(null, parseCSSNumber(value, 'Unitless')),
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', 'flexGrow')),
  )

  if (flexGrow != null) {
    const flexDirection = optionalMap(
      (e) => detectFlexDirectionOne(metadata, parentPath(e)),
      elementPath,
    )

    const isFlexDirectionHorizontal = flexDirection === 'row' || flexDirection === 'row-reverse'
    if (axis === 'horizontal' && isFlexDirectionHorizontal) {
      return { type: 'fill', value: flexGrow }
    }

    const isFlexDirectionVertical = flexDirection === 'column' || flexDirection === 'column-reverse'
    if (axis === 'vertical' && isFlexDirectionVertical) {
      return { type: 'fill', value: flexGrow }
    }
  }

  const property = widthHeightFromAxis(axis)

  const prop = defaultEither(
    null,
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', property)),
  )

  if (prop === MaxContent) {
    return { type: 'hug' }
  }

  const parsed = defaultEither(null, parseCSSLengthPercent(prop))

  if (parsed != null && parsed.unit === '%') {
    return { type: 'fill', value: parsed }
  }

  if (parsed != null) {
    return { type: 'fixed', value: parsed }
  }

  return null
}

export const MaxContent = 'max-content' as const

export function resizeToFitCommands(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
): Array<CanvasCommand> {
  const commands = [
    ...(commandsForFirstApplicableStrategy(
      metadata,
      selectedViews,
      setPropHugStrategies('horizontal'),
    ) ?? []),
    ...(commandsForFirstApplicableStrategy(
      metadata,
      selectedViews,
      setPropHugStrategies('vertical'),
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

export function notFixedSizeOnEitherAxis(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): boolean {
  return elementPaths.every((elementPath) => {
    const horizontalState = detectFillHugFixedState('horizontal', metadata, elementPath)?.type
    const verticalState = detectFillHugFixedState('vertical', metadata, elementPath)?.type
    return horizontalState !== 'fixed' && verticalState !== 'fixed'
  })
}

export function toggleResizeToFitSetToFixed(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): Array<CanvasCommand> {
  if (elementPaths.length === 0) {
    return []
  }

  return notFixedSizeOnEitherAxis(metadata, elementPaths)
    ? elementPaths.flatMap((e) => sizeToVisualDimensions(metadata, e))
    : resizeToFitCommands(metadata, elementPaths)
}
