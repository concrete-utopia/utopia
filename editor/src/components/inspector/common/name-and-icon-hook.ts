import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  isAnimatedElementAgainstImports,
  isImportedComponent,
} from '../../../core/model/project-file-utils'
import {
  isIntrinsicHTMLElement,
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

export interface NameAndIconResult {
  path: TemplatePath
  name: JSXElementName | null
  label: string
  iconProps: IcnProps
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
    (oldResult, newResult) => {
      const pathEquals = TP.pathsEqual(oldResult.path, newResult.path)
      const labelEquals = oldResult.label === newResult.label
      const iconPropsEqual = shallowEqual(oldResult.iconProps, newResult.iconProps)
      const oldNamePath = oldResult.name?.propertyPath != null ? oldResult.name?.propertyPath : null
      const newNamePath = newResult.name?.propertyPath != null ? newResult.name?.propertyPath : null
      const namePathEquals = PP.pathsEqual(oldNamePath, newNamePath)
      const nameVariableEquals = oldResult.name?.baseVariable === newResult.name?.baseVariable
      return pathEquals || labelEquals || iconPropsEqual || namePathEquals || nameVariableEquals
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
    iconProps: createIconProps(path, metadata, components, imports, elementName),
  }
}

function createIconProps(
  path: TemplatePath,
  metadata: JSXMetadata,
  components: UtopiaJSXComponent[],
  imports: Imports,
  elementName: JSXElementName | null,
): IcnProps {
  let elementIcon = {
    category: 'element',
    type: 'div',
    width: 18,
    height: 18,
  }

  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null

  const isComponent = elementName != null && !isIntrinsicHTMLElement(elementName)
  if (isComponent) {
    elementIcon = {
      ...elementIcon,
      category: 'component',
      type: 'default',
    }
  }
  const isImported = elementName != null && isImportedComponent(elementName, imports)
  if (isImported) {
    elementIcon = {
      ...elementIcon,
      category: 'component',
      type: 'npm',
    }
  }
  const isAnimatedComponent =
    elementName != null && isAnimatedElementAgainstImports(elementName, imports)
  if (isAnimatedComponent) {
    elementIcon = {
      ...elementIcon,
      category: 'component',
      type: 'animated',
    }
  }
  const isStyledComponent = element?.isEmotionComponent
  if (isStyledComponent) {
    elementIcon = {
      ...elementIcon,
      category: 'component',
      type: 'styled',
    }
  }
  if (TP.isScenePath(path)) {
    elementIcon = {
      ...elementIcon,
      category: 'component',
      type: 'scene',
    }
  }

  return elementIcon
}
