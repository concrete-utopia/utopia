import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isStoryboardChild } from '../../core/shared/element-path'
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

export function detectFlexAlignJustifyContent(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [FlexJustifyContent, FlexAlignment] {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return ['flex-start', 'flex-start']
  }

  const justifyContent: FlexJustifyContent =
    getFlexJustifyContent(element.computedStyle?.['justifyContent'] ?? null) ?? 'flex-start'
  const flexAlignment: FlexAlignment =
    getFlexAlignment(element.computedStyle?.['alignItems'] ?? null) ?? 'flex-start'

  return [justifyContent, flexAlignment]
}

export function filterKeepFlexContainers(
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): ElementPath[] {
  return elementPaths.filter((e: ElementPath | null) =>
    MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, e)),
  )
}

export function detectFlexDirection(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexDirection {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return 'row'
  }

  return stringToFlexDirection(element.computedStyle?.['flexDirection'] ?? null) ?? 'row'
}

export const isFlexColumn = (flexDirection: FlexDirection): boolean =>
  flexDirection.startsWith('column')

export const hugContentsApplicable = (
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean => MetadataUtils.getChildrenPaths(metadata, elementPath).length > 0

export const fillContainerApplicable = (elementPath: ElementPath): boolean =>
  !isStoryboardChild(elementPath)
