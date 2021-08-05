import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { JSXElementName, ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import { IcnProps } from '../../../uuiui'
import { createComponentOrElementIconProps } from '../../navigator/layout-element-icons'
import {
  NameAndIconResultArrayKeepDeepEquality,
  NameAndIconResultKeepDeepEquality,
} from '../../../utils/deep-equality-instances'
import { createSelector } from 'reselect'
import { EditorStore } from '../../editor/store/editor-state'
import * as React from 'react'

export interface NameAndIconResult {
  path: ElementPath
  name: JSXElementName | null
  label: string
  iconProps: IcnProps
}

export function useMetadata(): ElementInstanceMetadataMap {
  return useEditorState((store) => store.editor.jsxMetadata, 'useMetadata')
}

const nameAndIconResultSelector = (path: ElementPath) => {
  return createSelector(
    (store: EditorStore) => store.editor.jsxMetadata,
    (metadata) => getNameAndIconResult(path, metadata),
  )
}

const namesAndIconsAllPathsResultSelector = createSelector(
  (store: EditorStore) => store.editor.jsxMetadata,
  (metadata) => {
    return MetadataUtils.getAllPaths(metadata).map((path) => {
      return getNameAndIconResult(path, metadata)
    })
  },
)

export function useNameAndIcon(path: ElementPath): NameAndIconResult {
  const selector = React.useMemo(() => nameAndIconResultSelector(path), [path])
  return useEditorState(selector, 'useNameAndIcon', (oldResult, newResult) => {
    return NameAndIconResultKeepDeepEquality(oldResult, newResult).areEqual
  })
}

export function useNamesAndIconsAllPaths(): NameAndIconResult[] {
  const selector = React.useMemo(() => namesAndIconsAllPathsResultSelector, [])
  return useEditorState(selector, 'useNamesAndIconsAllPaths', (oldResult, newResult) => {
    return NameAndIconResultArrayKeepDeepEquality(oldResult, newResult).areEqual
  })
}

function getNameAndIconResult(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): NameAndIconResult {
  const elementName = MetadataUtils.getJSXElementNameFromMetadata(metadata, path)
  return {
    path: path,
    name: elementName,
    label: MetadataUtils.getElementLabel(path, metadata),
    iconProps: createComponentOrElementIconProps(path, metadata),
  }
}
