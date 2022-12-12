import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isStoryboardChild } from '../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'

const fitChildrenApplicable = (
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean => MetadataUtils.getChildrenPaths(metadata, elementPath).length > 0

const fillContainerApplicable = (elementPath: ElementPath): boolean =>
  isStoryboardChild(elementPath)
