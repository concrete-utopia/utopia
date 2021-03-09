import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isImg } from '../../core/model/project-file-utils'
import { isJSXElement, JSXMetadata, UtopiaJSXComponent } from '../../core/shared/element-template'
import * as TP from '../../core/shared/template-path'
import { TemplatePath } from '../../core/shared/project-file-types'
import { getOpenUtopiaJSXComponentsFromState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { isRight } from '../../core/shared/either'

interface LayoutIconResult {
  iconProps: {
    category: string
    type: string
    width: number
    height: number
  }
  hasWidthOrHeight: boolean
}

export function useLayoutIcon(path: TemplatePath): LayoutIconResult {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadataKILLME
    const components = getOpenUtopiaJSXComponentsFromState(store.editor)
    return createLayoutIconResult(path, components, metadata)
  }, 'useLayoutIcon')
}

export function createLayoutIconResult(
  path: TemplatePath,
  components: UtopiaJSXComponent[],
  metadata: JSXMetadata,
): LayoutIconResult {
  let layoutIconProps = {
    category: 'element',
    type: 'div',
    width: 18,
    height: 18,
  }
  let hasWidthOrHeight: boolean = false

  const element = TP.isInstancePath(path)
    ? MetadataUtils.getElementByInstancePathMaybe(metadata.elements, path)
    : null

  if (element != null && element.props != null && element.props.style != null) {
    hasWidthOrHeight = element.props.style['width'] != null || element.props.style['height'] != null
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
      layoutIconProps = {
        category: 'element',
        type: 'text',
        width: 18,
        height: 18,
      }
    }
  }

  const isGridLayoutedContainer = MetadataUtils.isGridLayoutedContainer(element)
  if (isGridLayoutedContainer) {
    layoutIconProps = {
      category: 'layout/systems',
      type: 'grid',
      width: 18,
      height: 18,
    }
  }
  const isFlexLayoutedContainer = MetadataUtils.isFlexLayoutedContainer(element)
  if (isFlexLayoutedContainer) {
    const flexDirection = MetadataUtils.getFlexDirection(element)
    if (flexDirection === 'row' || flexDirection === 'row-reverse') {
      layoutIconProps = {
        category: 'layout/systems',
        type: 'flex-row',
        width: 18,
        height: 18,
      }
    } else {
      layoutIconProps = {
        category: 'layout/systems',
        type: 'flex-column',
        width: 18,
        height: 18,
      }
    }
  }

  const elementName = MetadataUtils.getJSXElementName(path, components, metadata.components)
  if (elementName != null && isImg(elementName)) {
    layoutIconProps = {
      category: 'element',
      type: 'image',
      width: 18,
      height: 18,
    }
  }

  const isButton = MetadataUtils.isButton(path, components, metadata)
  if (isButton) {
    layoutIconProps = {
      category: 'element',
      type: 'button',
      width: 18,
      height: 18,
    }
  }

  return {
    iconProps: layoutIconProps,
    hasWidthOrHeight: hasWidthOrHeight,
  }
}
