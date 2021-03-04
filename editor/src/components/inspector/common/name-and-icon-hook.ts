import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  isAnimatedElementAgainstImports,
  isEllipseAgainstImports,
  isHTMLComponent,
  isImg,
  isRectangleAgainstImports,
  isTextAgainstImports,
  isViewAgainstImports,
} from '../../../core/model/project-file-utils'
import {
  JSXElementName,
  JSXMetadata,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import * as TP from '../../../core/shared/template-path'
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
      if (!TP.pathsEqual(oldResult.path, newResult.path)) {
        return false
      }
      if (oldResult.label !== newResult.label) {
        return false
      }
      if (!shallowEqual(oldResult.iconProps, newResult.iconProps)) {
        return false
      }
      if (!shallowEqual(oldResult.name, newResult.name)) {
        return false
      }
      return true
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
  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null
  const isFlexLayoutedContainer = MetadataUtils.isFlexLayoutedContainer(element)
  const flexDirection = MetadataUtils.getYogaDirection(element)
  const flexWrap = MetadataUtils.getYogaWrap(element)
  const componentInstance = MetadataUtils.isComponentInstance(path, components, metadata, imports)
  const isAutosizingView = MetadataUtils.isAutoSizingView(element)

  return {
    category: 'element',
    type: getIconTypeForElement(
      imports,
      elementName,
      isFlexLayoutedContainer,
      flexDirection,
      flexWrap,
      'open',
      componentInstance,
      isAutosizingView,
    ),
    width: 18,
    height: 18,
  }
}

export function getIconTypeForElement(
  imports: Imports,
  elementName: JSXElementName | null,
  isFlexLayoutedContainer: boolean,
  originalFlexDirection: 'row' | 'row-reverse' | 'column' | 'column-reverse',
  flexWrap: 'wrap' | 'wrap-reverse' | 'nowrap',
  openStatus: 'closed' | 'open',
  componentInstance: boolean,
  isGroup: boolean,
): string {
  let role: string = 'default'
  const flexDirection: 'column' | 'row' =
    originalFlexDirection === 'column' || originalFlexDirection === 'column-reverse'
      ? 'column'
      : 'row'
  const flexWrapped: boolean = flexWrap === 'wrap' || flexWrap === 'wrap-reverse'

  if (elementName == null) {
    role = 'scene'
  } else {
    if (isViewAgainstImports(elementName, imports)) {
      if (isGroup) {
        role = 'group'
      } else {
        role = 'view'
      }
    } else if (isEllipseAgainstImports(elementName, imports)) {
      role = 'ellipse'
    } else if (isRectangleAgainstImports(elementName, imports)) {
      role = 'rectangle'
    } else if (isImg(elementName)) {
      role = 'image'
    } else if (isTextAgainstImports(elementName, imports)) {
      role = 'text'
    } else if (isAnimatedElementAgainstImports(elementName, imports)) {
      role = 'animated'
    } else if (componentInstance) {
      role = 'componentinstance'
    } else if (isHTMLComponent(elementName, imports)) {
      role = 'div'
    }
  }

  let specifierPath: string
  if (isFlexLayoutedContainer && role === 'view') {
    specifierPath = `-flex-${flexWrapped ? 'wrap' : 'nowrap'}-${flexDirection}`
  } else {
    if (role === 'group') {
      specifierPath = `-${openStatus}`
    } else {
      specifierPath = ''
    }
  }
  return `${role}${specifierPath}`
}
