import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'

export type FlexJustifyContent =
  | 'flex-start'
  | 'center'
  | 'flex-end'
  | 'space-around'
  | 'space-between'
  | 'space-evenly'

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

export type FlexAlignment = 'auto' | 'flex-start' | 'center' | 'flex-end' | 'stretch'

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

export function detectFlexAlignJustifyContent(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [FlexJustifyContent, FlexAlignment] {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return ['flex-start', 'flex-start']
  }

  const justifyContent: FlexJustifyContent =
    getFlexJustifyContent(element.computedStyle?.['alignItems'] ?? null) ?? 'flex-start'
  const flexAlignment: FlexAlignment =
    getFlexAlignment(element.computedStyle?.['justifyContent'] ?? null) ?? 'flex-start'

  return [justifyContent, flexAlignment]
}
