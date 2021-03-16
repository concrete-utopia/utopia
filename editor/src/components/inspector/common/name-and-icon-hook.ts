import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  JSXElementName,
  JSXMetadata,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { Imports, TemplatePath } from '../../../core/shared/project-file-types'
import {
  getOpenImportsFromState,
  getOpenUtopiaJSXComponentsFromState,
} from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { IcnProps } from '../../../uuiui'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { createComponentOrElementIconProps } from '../../navigator/layout-element-icons'

export interface NameAndIconResult {
  path: TemplatePath
  name: JSXElementName | null
  label: string
  iconProps: IcnProps
}

function nameAndIconResultEquals(a: NameAndIconResult, b: NameAndIconResult): boolean {
  const pathEquals = TP.pathsEqual(a.path, b.path)
  const labelEquals = a.label === b.label
  const iconPropsEqual = shallowEqual(a.iconProps, b.iconProps)
  const oldNamePath = a.name?.propertyPath != null ? a.name?.propertyPath : null
  const newNamePath = b.name?.propertyPath != null ? b.name?.propertyPath : null
  const namePathEquals = PP.pathsEqual(oldNamePath, newNamePath)
  const nameVariableEquals = a.name?.baseVariable === b.name?.baseVariable
  return pathEquals || labelEquals || iconPropsEqual || namePathEquals || nameVariableEquals
}

export function useNameAndIcon(path: TemplatePath): NameAndIconResult {
  return useEditorState(
    (store) => {
      const metadata = store.editor.jsxMetadataKILLME
      const components = getOpenUtopiaJSXComponentsFromState(store.editor)
      const imports = getOpenImportsFromState(store.editor)
      return getNameAndIconResult(path, components, metadata, imports)
    },
    'useNameAndIcon',
    nameAndIconResultEquals,
  )
}

export function useNamesAndIconsSelectedViews(): NameAndIconResult[] {
  return useEditorState(
    (store) => {
      const metadata = store.editor.jsxMetadataKILLME
      const components = getOpenUtopiaJSXComponentsFromState(store.editor)
      const imports = getOpenImportsFromState(store.editor)
      const selectedViews = store.editor.selectedViews
      return selectedViews.map((path) => getNameAndIconResult(path, components, metadata, imports))
    },
    'useNameAndIconSelectedViews',
    (oldResult, newResult) => {
      return arrayEquals(oldResult, newResult, nameAndIconResultEquals)
    },
  )
}

export function useNamesAndIconsAllPaths(): NameAndIconResult[] {
  const metadata = useEditorState(
    (store) => store.editor.jsxMetadataKILLME,
    'useNamesAndIconsAllPaths metadata',
  )
  const components = useEditorState(
    (store) => getOpenUtopiaJSXComponentsFromState(store.editor),
    'useNamesAndIconsAllPaths components',
  )
  const imports = useEditorState(
    (store) => getOpenImportsFromState(store.editor),
    'useNamesAndIconsAllPaths imports',
  )

  return MetadataUtils.getAllPaths(metadata).map((path) =>
    getNameAndIconResult(path, components, metadata, imports),
  )
}

function getNameAndIconResult(
  path: TemplatePath,
  components: UtopiaJSXComponent[],
  metadata: JSXMetadata,
  imports: Imports,
): NameAndIconResult {
  const elementName = MetadataUtils.getJSXElementName(path, components, metadata.components)
  return {
    path: path,
    name: elementName,
    label: MetadataUtils.getElementLabel(path, metadata),
    iconProps: createComponentOrElementIconProps(path, components, metadata, imports),
  }
}
