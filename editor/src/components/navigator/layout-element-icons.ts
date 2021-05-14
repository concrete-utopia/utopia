import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isAnimatedElement, isImg, isImportedComponent } from '../../core/model/project-file-utils'
import {
  isIntrinsicHTMLElement,
  isJSXElement,
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import * as EP from '../../core/shared/element-path'
import { Imports, ElementPath } from '../../core/shared/project-file-types'
import { useEditorState } from '../editor/store/store-hook'
import { isRight, maybeEitherToMaybe } from '../../core/shared/either'
import { IcnPropsBase } from '../../uuiui'
import { shallowEqual } from '../../core/shared/equality-utils'
import { isProbablySceneFromMetadata } from './navigator-item/navigator-item'

interface LayoutIconResult {
  iconProps: IcnPropsBase
  hasWidthOrHeight: boolean
}

export function useLayoutOrElementIcon(path: ElementPath): LayoutIconResult {
  return useEditorState(
    (store) => {
      const metadata = store.editor.jsxMetadata
      return createLayoutOrElementIconResult(path, metadata)
    },
    'useLayoutOrElementIcon',
    (oldResult: LayoutIconResult, newResult: LayoutIconResult) => {
      return (
        oldResult.hasWidthOrHeight === newResult.hasWidthOrHeight ||
        shallowEqual(oldResult.iconProps, newResult.iconProps)
      )
    },
  )
}

export function useComponentIcon(path: ElementPath): IcnPropsBase | null {
  return useEditorState((store) => {
    const metadata = store.editor.jsxMetadata
    return createComponentIconProps(path, metadata)
  }, 'useComponentIcon') // TODO Memoize Icon Result
}

export function createComponentOrElementIconProps(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): IcnPropsBase {
  return createComponentIconProps(path, metadata) ?? createElementIconProps(path, metadata)
}

export function createLayoutOrElementIconResult(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): LayoutIconResult {
  let hasWidthOrHeight: boolean = false

  const element = MetadataUtils.findElementByElementPath(metadata, path)

  if (element != null && element.props != null && element.props.style != null) {
    hasWidthOrHeight = element.props.style['width'] != null || element.props.style['height'] != null
  }

  const layoutIcon = createLayoutIconProps(path, metadata)
  if (isProbablySceneFromMetadata(metadata, path)) {
    return {
      iconProps: {
        category: 'component',
        type: 'scene',
        width: 18,
        height: 18,
      },
      hasWidthOrHeight: false,
    }
  } else if (layoutIcon != null) {
    return {
      iconProps: layoutIcon,
      hasWidthOrHeight: hasWidthOrHeight,
    }
  } else {
    return {
      iconProps: createElementIconProps(path, metadata),
      hasWidthOrHeight: hasWidthOrHeight,
    }
  }
}

function createLayoutIconProps(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): IcnPropsBase | null {
  const element = MetadataUtils.findElementByElementPath(metadata, path)

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
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): IcnPropsBase {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  const isButton = MetadataUtils.isButton(path, metadata)
  if (isButton) {
    return {
      category: 'element',
      type: 'button',
      width: 18,
      height: 18,
    }
  }
  const elementName = MetadataUtils.getJSXElementName(maybeEitherToMaybe(element?.element))
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
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): IcnPropsBase | null {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (isProbablySceneFromMetadata(metadata, path)) {
    return null
  }
  if (element?.isEmotionOrStyledComponent) {
    return {
      category: 'component',
      type: 'styled',
      width: 18,
      height: 18,
    }
  }
  const isAnimatedComponent = isAnimatedElement(element)
  if (isAnimatedComponent) {
    return {
      category: 'component',
      type: 'animated',
      width: 18,
      height: 18,
    }
  }
  const isImported = isImportedComponent(element)
  if (isImported) {
    return {
      category: 'component',
      type: 'npm',
      width: 18,
      height: 18,
    }
  }
  const isComponent = MetadataUtils.isFocusableComponent(path, metadata)
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
