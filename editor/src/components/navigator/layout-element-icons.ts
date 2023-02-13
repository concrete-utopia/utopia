import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isAnimatedElement, isImg, isImportedComponent } from '../../core/model/project-file-utils'
import {
  isIntrinsicHTMLElement,
  isJSXElement,
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
  ElementInstanceMetadata,
  isJSXFragment,
} from '../../core/shared/element-template'
import * as EP from '../../core/shared/element-path'
import { Imports, ElementPath } from '../../core/shared/project-file-types'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { isRight, maybeEitherToMaybe } from '../../core/shared/either'
import { IcnPropsBase } from '../../uuiui'
import { shallowEqual } from '../../core/shared/equality-utils'
import { AllElementProps } from '../editor/store/editor-state'
import { isSpawnedActor } from 'xstate/lib/Actor'

interface LayoutIconResult {
  iconProps: IcnPropsBase
  isPositionAbsolute: boolean
}

export function useLayoutOrElementIcon(path: ElementPath): LayoutIconResult {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const metadata = store.editor.jsxMetadata
      return createLayoutOrElementIconResult(path, metadata, store.editor.allElementProps)
    },
    'useLayoutOrElementIcon',
    (oldResult: LayoutIconResult, newResult: LayoutIconResult) => {
      return (
        oldResult.isPositionAbsolute === newResult.isPositionAbsolute &&
        shallowEqual(oldResult.iconProps, newResult.iconProps)
      )
    },
  )
}

export function useComponentIcon(path: ElementPath): IcnPropsBase | null {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const metadata = store.editor.jsxMetadata
      return createComponentIconProps(path, metadata)
    },
    'useComponentIcon',
  ) // TODO Memoize Icon Result
}

export function createComponentOrElementIconProps(element: ElementInstanceMetadata): IcnPropsBase {
  return (
    createComponentIconPropsFromMetadata(element) ?? createElementIconPropsFromMetadata(element)
  )
}

export function createLayoutOrElementIconResult(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): LayoutIconResult {
  let isPositionAbsolute: boolean = false

  const element = MetadataUtils.findElementByElementPath(metadata, path)
  const elementProps = allElementProps[EP.toString(path)]

  if (element != null && elementProps != null && elementProps.style != null) {
    isPositionAbsolute = elementProps.style['position'] === 'absolute'
  }

  const layoutIcon = createLayoutIconProps(path, metadata)
  if (MetadataUtils.isProbablyScene(metadata, path)) {
    return {
      iconProps: {
        category: 'component',
        type: 'scene',
        width: 18,
        height: 18,
      },

      isPositionAbsolute: false,
    }
  } else if (layoutIcon != null) {
    return {
      iconProps: layoutIcon,
      isPositionAbsolute: isPositionAbsolute,
    }
  } else {
    return {
      iconProps: createElementIconProps(path, metadata),
      isPositionAbsolute: isPositionAbsolute,
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

export function createElementIconPropsFromMetadata(
  element: ElementInstanceMetadata | null,
): IcnPropsBase {
  const isFragment = MetadataUtils.isFragmentFromMetadata(element)
  if (isFragment) {
    return {
      category: 'element',
      type: 'group-open',
      width: 18,
      height: 18,
    }
  }

  const isButton = MetadataUtils.isButtonFromMetadata(element)
  if (isButton) {
    return {
      category: 'element',
      type: 'clickable',
      width: 18,
      height: 18,
    }
  }
  const isText = MetadataUtils.isTextFromMetadata(element)
  if (isText) {
    return {
      category: 'element',
      type: 'pure-text',
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
        type: 'pure-text',
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

export function createElementIconProps(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): IcnPropsBase {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  return createElementIconPropsFromMetadata(element)
}

function createComponentIconPropsFromMetadata(
  element: ElementInstanceMetadata | null,
): IcnPropsBase | null {
  if (MetadataUtils.isProbablySceneFromMetadata(element)) {
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
  const isComponent = MetadataUtils.isFocusableComponentFromMetadata(element)
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

function createComponentIconProps(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): IcnPropsBase | null {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  return createComponentIconPropsFromMetadata(element)
}
