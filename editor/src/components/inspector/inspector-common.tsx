import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { EditorState, EditorStorePatched } from '../editor/store/editor-state'
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

const DefaultJustifyContentFlexAlign: [FlexJustifyContent, FlexAlignment] = [
  'flex-start',
  'flex-start',
]

function detectFlexAlignJustifyContentOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [FlexJustifyContent, FlexAlignment] {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || !MetadataUtils.isFlexLayoutedContainer(element)) {
    return DefaultJustifyContentFlexAlign
  }

  const justifyContent: FlexJustifyContent =
    getFlexJustifyContent(element.computedStyle?.['justifyContent'] ?? null) ??
    DefaultJustifyContentFlexAlign[0]
  const flexAlignment: FlexAlignment =
    getFlexAlignment(element.computedStyle?.['alignItems'] ?? null) ??
    DefaultJustifyContentFlexAlign[1]

  return [justifyContent, flexAlignment]
}

export function detectFlexAlignJustifyContent(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): [FlexJustifyContent, FlexAlignment] {
  const allDetectedMeasurements = elementPaths.map((path) =>
    detectFlexAlignJustifyContentOne(metadata, path),
  )
  return allElemsEqual(allDetectedMeasurements, (l, r) => l[0] === r[0] && l[1] === r[1])
    ? allDetectedMeasurements[0]
    : DefaultJustifyContentFlexAlign
}

export function filterKeepFlexContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): ElementPath[] {
  return elementPaths.filter((e: ElementPath | null) =>
    MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, e)),
  )
}

const DefaultFlexDirection: FlexDirection = 'row'

function detectFlexDirectionOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexDirection {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || !MetadataUtils.isFlexLayoutedContainer(element)) {
    return DefaultFlexDirection
  }

  return (
    stringToFlexDirection(element.computedStyle?.['flexDirection'] ?? null) ?? DefaultFlexDirection
  )
}

export function detectFlexDirection(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): FlexDirection {
  const allDetectedMeasurement = elementPaths.map((path) => detectFlexDirectionOne(metadata, path))
  return allElemsEqual(allDetectedMeasurement, (l, r) => l === r)
    ? allDetectedMeasurement[0]
    : DefaultFlexDirection
}

export const isFlexColumn = (flexDirection: FlexDirection): boolean =>
  flexDirection.startsWith('column')

function allElemsEqual<T>(objects: T[], areEqual: (a: T, b: T) => boolean): boolean {
  if (objects.length === 0) {
    return false
  }

  return objects.slice(1).every((obj) => areEqual(objects[0], obj))
}
