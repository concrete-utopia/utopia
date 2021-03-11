import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  isAnimatedElementAgainstImports,
  isImg,
  isImportedComponent,
} from '../../core/model/project-file-utils'
import {
  isIntrinsicHTMLElement,
  isJSXElement,
  JSXMetadata,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import * as TP from '../../core/shared/template-path'
import { Imports, TemplatePath } from '../../core/shared/project-file-types'
import {
  getOpenImportsFromState,
  getOpenUtopiaJSXComponentsFromState,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { isRight } from '../../core/shared/either'
import { IcnProps } from '../../uuiui'

interface LayoutIconResult {
  iconProps: IcnProps
  hasWidthOrHeight: boolean
}

export function useLayoutOrElementIcon(path: TemplatePath): LayoutIconResult {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadataKILLME
    const components = getOpenUtopiaJSXComponentsFromState(store.editor)
    return createLayoutOrElementIconResult(path, components, metadata)
  }, 'useLayoutOrElementIcon')
}

export function useComponentIcon(path: TemplatePath): IcnProps | null {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadataKILLME
    const components = getOpenUtopiaJSXComponentsFromState(store.editor)
    const imports = getOpenImportsFromState(store.editor)
    return createComponentIconProps(path, components, metadata, imports)
  }, 'useComponentIcon')
}

export function createComponentOrElementIconProps(
  path: TemplatePath,
  components: UtopiaJSXComponent[],
  metadata: JSXMetadata,
  imports: Imports,
): IcnProps {
  return (
    createComponentIconProps(path, components, metadata, imports) ??
    createElementIconProps(path, components, metadata)
  )
}

export function createLayoutOrElementIconResult(
  path: TemplatePath,
  components: UtopiaJSXComponent[],
  metadata: JSXMetadata,
): LayoutIconResult {
  let hasWidthOrHeight: boolean = false

  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null

  if (element != null && element.props != null && element.props.style != null) {
    hasWidthOrHeight = element.props.style['width'] != null || element.props.style['height'] != null
  }

  const layoutIcon = createLayoutIconProps(path, metadata)
  if (TP.isScenePath(path)) {
    return {
      iconProps: {
        category: 'component',
        type: 'scene',
        width: 18,
        height: 18,
      },
      hasWidthOrHeight: hasWidthOrHeight,
    }
  } else if (layoutIcon != null) {
    return {
      iconProps: layoutIcon,
      hasWidthOrHeight: hasWidthOrHeight,
    }
  } else {
    return {
      iconProps: createElementIconProps(path, components, metadata),
      hasWidthOrHeight: hasWidthOrHeight,
    }
  }
}

function createLayoutIconProps(path: TemplatePath, metadata: JSXMetadata): IcnProps | null {
  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null

  const isFlexLayoutedContainer = MetadataUtils.isFlexLayoutedContainer(element)
  if (isFlexLayoutedContainer) {
    const flexDirection = MetadataUtils.getFlexDirection(element)
    if (flexDirection === 'row' || flexDirection === 'row-reverse') {
      return {
        category: 'layout/systems',
        type: 'flex-row',
        width: 18,
        height: 18,
      }
    } else {
      return {
        category: 'layout/systems',
        type: 'flex-column',
        width: 18,
        height: 18,
      }
    }
  }

  const isGridLayoutedContainer = MetadataUtils.isGridLayoutedContainer(element)
  if (isGridLayoutedContainer) {
    return {
      category: 'layout/systems',
      type: 'grid',
      width: 18,
      height: 18,
    }
  }

  return null
}

export function createElementIconProps(
  path: TemplatePath,
  components: UtopiaJSXComponent[],
  metadata: JSXMetadata,
): IcnProps {
  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null
  const isButton = MetadataUtils.isButton(path, components, metadata)
  if (isButton) {
    return {
      category: 'element',
      type: 'button',
      width: 18,
      height: 18,
    }
  }
  const elementName = MetadataUtils.getJSXElementName(path, components, metadata.components)
  if (elementName != null && isImg(elementName)) {
    return {
      category: 'element',
      type: 'image',
      width: 18,
      height: 18,
    }
  }

  const isDisplayInline = element?.specialSizeMeasurements.display.includes('inline')
  if (
    isDisplayInline &&
    element != null &&
    isRight(element.element) &&
    isJSXElement(element.element.value)
  ) {
    const hasTextChild = element.element.value.children.some(
      (child) => child.type === 'JSX_TEXT_BLOCK',
    )
    if (hasTextChild) {
      return {
        category: 'element',
        type: 'text',
        width: 18,
        height: 18,
      }
    }
  }

  return {
    category: 'element',
    type: 'div',
    width: 18,
    height: 18,
  }
}

function createComponentIconProps(
  path: TemplatePath,
  components: UtopiaJSXComponent[],
  metadata: JSXMetadata,
  imports: Imports,
): IcnProps | null {
  const elementName = MetadataUtils.getJSXElementName(path, components, metadata.components)
  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null
  const isStyledComponent = element?.isEmotionComponent
  if (isStyledComponent) {
    return {
      category: 'component',
      type: 'styled',
      width: 18,
      height: 18,
    }
  }
  const isAnimatedComponent =
    elementName != null && isAnimatedElementAgainstImports(elementName, imports)
  if (isAnimatedComponent) {
    return {
      category: 'component',
      type: 'animated',
      width: 18,
      height: 18,
    }
  }
  const isImported = elementName != null && isImportedComponent(elementName, imports)
  if (isImported) {
    return {
      category: 'component',
      type: 'npm',
      width: 18,
      height: 18,
    }
  }
  const isComponent = elementName != null && !isIntrinsicHTMLElement(elementName)
  if (isComponent) {
    return {
      category: 'component',
      type: 'default',
      width: 18,
      height: 18,
    }
  }

  return null
}
