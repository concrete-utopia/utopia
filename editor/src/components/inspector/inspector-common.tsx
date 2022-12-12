import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { FlexDirection } from './common/css-utils'

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

type Detect<T> = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
) => T | null

export const DefaultFlexDirection: FlexDirection = 'row'

function detectFlexDirectionOne(
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

export const detectFlexDirection: Detect<FlexDirection> = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
) => {
  const allDetectedMeasurements = elementPaths.map((path) => detectFlexDirectionOne(metadata, path))
  return allElemsEqual(allDetectedMeasurements, (l, r) => l === r)
    ? allDetectedMeasurements[0]
    : null
}

function detectFlexAlignJustifyContentOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [FlexJustifyContent, FlexAlignment] | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || !MetadataUtils.isFlexLayoutedContainer(element)) {
    return null
  }

  const justifyContent: FlexJustifyContent | null = getFlexJustifyContent(
    element.computedStyle?.['justifyContent'] ?? null,
  )
  const flexAlignment: FlexAlignment | null = getFlexAlignment(
    element.computedStyle?.['alignItems'] ?? null,
  )

  if (justifyContent == null || flexAlignment == null) {
    return null
  }

  return [justifyContent, flexAlignment]
}

export const detectFlexAlignJustifyContent: Detect<[FlexJustifyContent, FlexAlignment]> = (
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

  return allElemsEqual(allDetectedMeasurements, (l, r) => l[0] === r[0] && l[1] === r[1])
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

export function detectAreElementsInFlexLayout(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): boolean {
  return elementPaths.every((path) =>
    MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, path)),
  )
}

export const isFlexColumn = (flexDirection: FlexDirection): boolean =>
  flexDirection.startsWith('column')

export function justifyContentAlignItemsEquals(
  flexDirection: FlexDirection,
  left: [FlexJustifyContent, FlexAlignment],
  right: [FlexJustifyContent, FlexAlignment],
): boolean {
  const [justifyContent, alignItems] = left
  return isFlexColumn(flexDirection)
    ? alignItems === right[0] && justifyContent === right[1]
    : alignItems === right[1] && justifyContent === right[0]
}

function allElemsEqual<T>(objects: T[], areEqual: (a: T, b: T) => boolean): boolean {
  if (objects.length === 0) {
    return false
  }

  return objects.slice(1).every((obj) => areEqual(objects[0], obj))
}
