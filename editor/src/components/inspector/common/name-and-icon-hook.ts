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
import { maybeEitherToMaybe } from '../../../core/shared/either'

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

  return MetadataUtils.getAllPaths(metadata).map((path) => {
    return getNameAndIconResult(path, metadata)
  }) // TODO memoize?
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
