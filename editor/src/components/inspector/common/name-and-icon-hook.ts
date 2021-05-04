import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  JSXElementName,
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { Imports, ElementPath } from '../../../core/shared/project-file-types'
import { useEditorState } from '../../editor/store/store-hook'
import { IcnProps } from '../../../uuiui'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { createComponentOrElementIconProps } from '../../navigator/layout-element-icons'
import { getJSXComponentsAndImportsForPathFromState } from '../../editor/store/editor-state'

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
      const pathEquals = EP.pathsEqual(oldResult.path, newResult.path)
      const labelEquals = oldResult.label === newResult.label
      const iconPropsEqual = shallowEqual(oldResult.iconProps, newResult.iconProps)
      const oldNamePath = oldResult.name?.propertyPath != null ? oldResult.name?.propertyPath : null
      const newNamePath = newResult.name?.propertyPath != null ? newResult.name?.propertyPath : null
      const namePathEquals = PP.pathsEqual(oldNamePath, newNamePath)
      const nameVariableEquals = oldResult.name?.baseVariable === newResult.name?.baseVariable
      return pathEquals && labelEquals && iconPropsEqual && namePathEquals && nameVariableEquals
    },
  )
}

export function useNamesAndIconsAllPaths(): NameAndIconResult[] {
  const metadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'useNamesAndIconsAllPaths metadata',
  )
  const editor = useEditorState((store) => store.editor, 'useNamesAndIconsAllPaths editor')
  const derived = useEditorState((store) => store.derived, 'useNamesAndIconsAllPaths derived')

  return MetadataUtils.getAllPaths(metadata).map((path) => {
    const { components, imports } = getJSXComponentsAndImportsForPathFromState(
      path,
      editor,
      derived,
    )
    return getNameAndIconResult(path, components, metadata, imports)
  })
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
