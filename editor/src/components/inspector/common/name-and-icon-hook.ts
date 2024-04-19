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
import {
  NameAndIconResultArrayKeepDeepEquality,
  NameAndIconResultKeepDeepEquality,
} from '../../../utils/deep-equality-instances'
import { createSelector } from 'reselect'
import type { AllElementProps } from '../../editor/store/editor-state'
import type { EditorStorePatched } from '../../editor/store/editor-state'
import React from 'react'
import { objectValues } from '../../../core/shared/object-utils'
import { eitherToMaybe } from '../../../core/shared/either'
import type {
  CanvasSubstate,
  MetadataSubstate,
  ProjectContentSubstate,
} from '../../editor/store/store-hook-substore-types'
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

const namesAndIconsAllPathsResultSelector = createSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (store: EditorStorePatched) => store.derived.autoFocusedPaths,
  (store: EditorStorePatched) => store.derived.filePathMappings,
  (store: EditorStorePatched) => store.editor.propertyControlsInfo,
  (store: ProjectContentSubstate) => store.editor.projectContents,
  (
    metadata,
    allElementProps,
    pathTrees,
    autoFocusedPaths,
    filePathMappings,
    propertyControlsInfo,
    projectContents,
  ) => {
    let result: Array<NameAndIconResult> = []
    for (const metadataElement of objectValues(metadata)) {
      const nameAndIconResult = getNameAndIconResult(
        metadata,
        allElementProps,
        metadataElement,
        pathTrees,
        autoFocusedPaths,
        filePathMappings,
        propertyControlsInfo,
        projectContents,
      )
      result.push(nameAndIconResult)
    }
    return result
  },
)

export function useNamesAndIconsAllPaths(): NameAndIconResult[] {
  const selector = React.useMemo(() => namesAndIconsAllPathsResultSelector, [])
  return useEditorState(
    Substores.fullStore,
    selector,
    'useNamesAndIconsAllPaths',
    (oldResult, newResult) => {
      return NameAndIconResultArrayKeepDeepEquality(oldResult, newResult).areEqual
    },
  )
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
