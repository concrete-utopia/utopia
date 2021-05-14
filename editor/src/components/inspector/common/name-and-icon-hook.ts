import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  JSXElementName,
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { Imports, ElementPath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import { IcnProps } from '../../../uuiui'
import { createComponentOrElementIconProps } from '../../navigator/layout-element-icons'
import { getJSXComponentsAndImportsForPathFromState } from '../../editor/store/editor-state'
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
      const { components, imports } = getJSXComponentsAndImportsForPathFromState(
        path,
        store.editor,
        store.derived,
      )
      return getNameAndIconResult(path, components, metadata, imports)
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
      return MetadataUtils.getAllPaths(store.editor.jsxMetadata).map((path) => {
        const { components, imports } = getJSXComponentsAndImportsForPathFromState(
          path,
          store.editor,
          store.derived,
        )
        return getNameAndIconResult(path, components, store.editor.jsxMetadata, imports)
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
  components: UtopiaJSXComponent[],
  metadata: ElementInstanceMetadataMap,
  imports: Imports,
): NameAndIconResult {
  const elementName = MetadataUtils.getJSXElementName(path, components)
  return {
    path: path,
    name: elementName,
    label: MetadataUtils.getElementLabel(path, metadata),
    iconProps: createComponentOrElementIconProps(path, components, metadata, imports),
  }
}
