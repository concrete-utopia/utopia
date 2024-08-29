import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type {
  JSXElementName,
  ElementInstanceMetadataMap,
  ElementInstanceMetadata,
} from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type { IcnProps } from '../../../uuiui'
import { createComponentOrElementIconProps } from '../../navigator/layout-element-icons'
import type { AllElementProps } from '../../editor/store/editor-state'
import type { EditorStorePatched } from '../../editor/store/editor-state'
import React from 'react'
import { objectValues } from '../../../core/shared/object-utils'
import { eitherToMaybe } from '../../../core/shared/either'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { FilePathMappings } from '../../../core/model/project-file-utils'
import type { PropertyControlsInfo } from '../../custom-code/code-file'
import type { ProjectContentTreeRoot } from '../../assets'

export interface NameAndIconResult {
  path: ElementPath
  name: JSXElementName | null
  label: string
  iconProps: IcnProps
}

export function useMetadata(): ElementInstanceMetadataMap {
  return useEditorState(Substores.metadata, (store) => store.editor.jsxMetadata, 'useMetadata')
}

export function getNamesAndIconsAllPaths(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  autoFocusedPaths: Array<ElementPath>,
  filePathMappings: FilePathMappings,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): NameAndIconResult[] {
  return objectValues(metadata).map((elementInstanceMetadata) => {
    return getNameAndIconResult(
      metadata,
      allElementProps,
      elementInstanceMetadata,
      elementPathTree,
      autoFocusedPaths,
      filePathMappings,
      propertyControlsInfo,
      projectContents,
    )
  })
}

function getNameAndIconResult(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementInstanceMetadata: ElementInstanceMetadata,
  pathTrees: ElementPathTrees,
  autoFocusedPaths: Array<ElementPath>,
  filePathMappings: FilePathMappings,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): NameAndIconResult {
  const elementName = MetadataUtils.getJSXElementName(
    eitherToMaybe(elementInstanceMetadata.element),
  )
  return {
    path: elementInstanceMetadata.elementPath,
    name: elementName,
    label: MetadataUtils.getElementLabelFromMetadata(
      metadata,
      allElementProps,
      pathTrees,
      elementInstanceMetadata,
    ),
    iconProps: createComponentOrElementIconProps(
      elementInstanceMetadata.elementPath,
      metadata,
      pathTrees,
      autoFocusedPaths,
      null,
      allElementProps,
      filePathMappings,
      propertyControlsInfo,
      projectContents,
    ),
  }
}
