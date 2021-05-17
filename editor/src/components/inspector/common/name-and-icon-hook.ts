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

export interface NameAndIconResult {
  path: ElementPath
  name: JSXElementName | null
  label: string
  iconProps: IcnProps
}

export function useNameAndIcon(path: ElementPath): NameAndIconResult {
  return useEditorState(
    (store) => {
      const metadata = store.editor.jsxMetadata
      return getNameAndIconResult(path, metadata)
    },
    'useNameAndIcon',
    (oldResult, newResult) => {
      return NameAndIconResultKeepDeepEquality(oldResult, newResult).areEqual
    },
  )
}

export function useNamesAndIconsAllPaths(): NameAndIconResult[] {
  return useEditorState(
    (store) => {
      const metadata = store.editor.jsxMetadata
      return MetadataUtils.getAllPaths(metadata).map((path) => {
        return getNameAndIconResult(path, metadata)
      })
    },
    'useNamesAndIconsAllPaths',
    (oldResult, newResult) => {
      return NameAndIconResultArrayKeepDeepEquality(oldResult, newResult).areEqual
    },
  )
}

function getNameAndIconResult(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): NameAndIconResult {
  const elementName = MetadataUtils.getJSXElementFromMetadata(metadata, path)
  return {
    path: path,
    name: elementName,
    label: MetadataUtils.getElementLabel(path, metadata),
    iconProps: createComponentOrElementIconProps(path, metadata),
  }
}
