import * as PP from '../../core/shared/property-path'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { isStoryboardChild } from '../../core/shared/element-path'
import { mapDropNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath, PropertyPath } from '../../core/shared/project-file-types'
import { FlexDirection } from './common/css-utils'
import { assertNever } from '../../core/shared/utils'
import { defaultEither, right } from '../../core/shared/either'
import { elementOnlyHasTextChildren } from '../../core/model/element-template-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { CSSProperties } from 'react'
import { CanvasCommand } from '../canvas/commands/commands'
import { deleteProperties } from '../canvas/commands/delete-properties-command'
import { setProperty } from '../canvas/commands/set-property-command'

export type StartCenterEnd = 'flex-start' | 'center' | 'flex-end'

export type FlexJustifyContent = StartCenterEnd | 'space-around' | 'space-between' | 'space-evenly'

function getFlexJustifyContent(value: string | null): FlexJustifyContent | null {
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

function getFlexAlignment(value: string | null): FlexAlignment | null {
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
    stringToFlexDirection(element.computedStyle?.['flexDirection'] ?? null) ?? DefaultFlexDirection
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
    element.computedStyle?.['justifyContent'] ?? null,
  )
  const alignItems: FlexAlignment | null = getFlexAlignment(
    element.computedStyle?.['alignItems'] ?? null,
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

export function convertWidthToFlexGrow(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  parentPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.getJSXElementFromMetadata(metadata, elementPath)
  if (element == null) {
    return []
  }

  const parentFlexDirection =
    MetadataUtils.findElementByElementPath(metadata, parentPath)?.computedStyle?.[
      'flexDirection'
    ] ?? 'row'
  const prop = parentFlexDirection.startsWith('row') ? 'width' : 'height'

  const matches =
    defaultEither(
      null,
      getSimpleAttributeAtPath(right(element.props), PP.create(['style', prop])),
    ) === '100%'

  if (!matches) {
    return []
  }
  return [
    deleteProperties('always', elementPath, [PP.create(['style', prop])]),
    setProperty('always', elementPath, PP.create(['style', 'flexGrow']), 1),
  ]
}

export function nullOrNonEmpty<T>(ts: Array<T>): Array<T> | null {
  return ts.length === 0 ? null : ts
}

export const styleP = (prop: keyof CSSProperties): PropertyPath => PP.create(['style', prop])

export const flexContainerProps = [
  styleP('flexDirection'),
  styleP('flexWrap'),
  styleP('gap'),
  styleP('display'),
  styleP('alignItems'),
  styleP('justifyContent'),
]

export const flexChildProps = [styleP('flex'), styleP('flexGrow'), styleP('flexShrink')]

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
    setProperty('always', elementPath, styleP('width'), width),
    setProperty('always', elementPath, styleP('height'), height),
  ]
}
