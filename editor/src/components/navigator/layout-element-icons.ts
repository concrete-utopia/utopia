import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  type FilePathMappings,
  isAnimatedElement,
  isImg,
  isImportedComponent,
} from '../../core/model/project-file-utils'
import type {
  ElementInstanceMetadataMap,
  ElementInstanceMetadata,
  JSXElementChild,
} from '../../core/shared/element-template'
import { isJSXElement, isJSXAttributeValue } from '../../core/shared/element-template'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from '../../core/shared/project-file-types'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { isLeft, isRight, maybeEitherToMaybe } from '../../core/shared/either'
import type { IcnPropsBase } from '../../uuiui'
import { shallowEqual } from '../../core/shared/equality-utils'
import type { AllElementProps, NavigatorEntry } from '../editor/store/editor-state'
import { isRegularNavigatorEntry, isSyntheticNavigatorEntry } from '../editor/store/editor-state'
import { getElementFragmentLikeType } from '../canvas/canvas-strategies/strategies/fragment-like-helpers'
import { findMaybeConditionalExpression } from '../../core/model/conditionals'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import { treatElementAsGroupLike } from '../canvas/canvas-strategies/strategies/group-helpers'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import type { ProjectContentTreeRoot } from '../assets'

interface LayoutIconResult {
  iconProps: IcnPropsBase
  isPositionAbsolute: boolean
}

export function useLayoutOrElementIcon(navigatorEntry: NavigatorEntry): LayoutIconResult {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const metadata = store.editor.jsxMetadata
      const pathTrees = store.editor.elementPathTree
      return createElementIconPropsFromMetadata(
        navigatorEntry.elementPath,
        metadata,
        pathTrees,
        navigatorEntry,
        store.editor.allElementProps,
      )
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

export function useComponentIcon(navigatorEntry: NavigatorEntry): IcnPropsBase | null {
  const autoFocusedPaths = useEditorState(
    Substores.derived,
    (store) => store.derived.autoFocusedPaths,
    'useComponentIcon autoFocusedPaths',
  )
  const filePathMappings = useEditorState(
    Substores.derived,
    (store) => store.derived.filePathMappings,
    'useComponentIcon filePathMappings',
  )

  return useEditorState(
    Substores.fullStore,
    (store) => {
      const metadata = store.editor.jsxMetadata
      return createComponentIconProps(
        navigatorEntry.elementPath,
        metadata,
        autoFocusedPaths,
        filePathMappings,
        store.editor.propertyControlsInfo,
        store.editor.projectContents,
      )
    },
    'useComponentIcon',
  ) // TODO Memoize Icon Result
}

export function createComponentOrElementIconProps(
  elementPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  autoFocusedPaths: Array<ElementPath>,
  navigatorEntry: NavigatorEntry | null,
  allElementProps: AllElementProps,
  filePathMappings: FilePathMappings,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): IcnPropsBase {
  return (
    createComponentIconProps(
      elementPath,
      metadata,
      autoFocusedPaths,
      filePathMappings,
      propertyControlsInfo,
      projectContents,
    ) ??
    createElementIconPropsFromMetadata(
      elementPath,
      metadata,
      pathTrees,
      navigatorEntry,
      allElementProps,
    ).iconProps
  )
}

function isConditionalBranchText(
  navigatorEntry: NavigatorEntry | null,
  metadata: ElementInstanceMetadataMap,
): boolean {
  function getNavigatorEntryConditionalElementOrNull(): JSXElementChild | null {
    if (navigatorEntry == null) {
      return null
    }

    if (isSyntheticNavigatorEntry(navigatorEntry)) {
      return navigatorEntry.childOrAttribute
    }

    if (isRegularNavigatorEntry(navigatorEntry)) {
      const parent = EP.parentPath(navigatorEntry.elementPath)
      const conditional = findMaybeConditionalExpression(parent, metadata)
      if (conditional == null) {
        return null
      }
      const original = MetadataUtils.findElementByElementPath(metadata, navigatorEntry.elementPath)
      if (original == null || isLeft(original.element)) {
        return null
      }
      return original.element.value
    }

    return null
  }

  const element = getNavigatorEntryConditionalElementOrNull()
  return element != null && isJSXAttributeValue(element) && typeof element.value === 'string'
}

export function createElementIconPropsFromMetadata(
  elementPath: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  navigatorEntry: NavigatorEntry | null,
  allElementProps: AllElementProps,
): LayoutIconResult {
  let isPositionAbsolute: boolean = false
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const elementProps = allElementProps[EP.toString(elementPath)]

  if (element != null && elementProps != null && elementProps.style != null) {
    isPositionAbsolute = elementProps.style['position'] === 'absolute'
  }

  if (treatElementAsGroupLike(metadata, elementPath)) {
    return {
      iconProps: {
        category: 'element',
        type: 'group-closed',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  if (MetadataUtils.isConditionalFromMetadata(element)) {
    return {
      iconProps: {
        category: 'element',
        type: 'conditional',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isExpressionOtherJavascript = MetadataUtils.isExpressionOtherJavascriptFromMetadata(element)
  if (isExpressionOtherJavascript) {
    return {
      iconProps: {
        category: 'element',
        type: 'genericcode',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: false,
    }
  }

  const isJSXMapExpression = MetadataUtils.isJSXMapExpressionFromMetadata(element)
  if (isJSXMapExpression) {
    return {
      iconProps: {
        category: 'element',
        type: 'lists',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: false,
    }
  }

  const fragmentLikeType = getElementFragmentLikeType(
    metadata,
    allElementProps,
    pathTrees,
    elementPath,
  )

  if (fragmentLikeType === 'fragment') {
    return {
      iconProps: {
        category: 'element',
        type: 'fragment',
        width: 18,
        height: 18,
      },

      isPositionAbsolute: false,
    }
  }

  if (fragmentLikeType !== null) {
    return {
      iconProps: {
        category: 'element',
        type: 'group-open',
        width: 18,
        height: 18,
      },

      isPositionAbsolute: false,
    }
  }

  if (
    MetadataUtils.isProbablyScene(metadata, elementPath) ||
    MetadataUtils.isProbablyRemixScene(metadata, elementPath)
  ) {
    return {
      iconProps: {
        category: 'component',
        type: 'scene',
        width: 18,
        height: 18,
      },

      isPositionAbsolute: false,
    }
  }

  if (MetadataUtils.isProbablyRemixOutletFromMetadata(element)) {
    return {
      iconProps: {
        category: 'component',
        type: 'remix-outlet',
        width: 18,
        height: 18,
      },

      isPositionAbsolute: false,
    }
  }

  if (MetadataUtils.isProbablyRemixLinkFromMetadata(element)) {
    return {
      iconProps: {
        category: 'component',
        type: 'remix-link',
        width: 18,
        height: 18,
      },

      isPositionAbsolute: false,
    }
  }

  if (MetadataUtils.isReactSuspense(element)) {
    return {
      iconProps: {
        category: 'element',
        type: 'div',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isHTMLElement = element != null && MetadataUtils.isHTML(element)
  if (isHTMLElement) {
    return {
      iconProps: {
        category: 'element',
        type: 'html',
        width: 12,
        height: 12,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isBodyElement = element != null && MetadataUtils.isBody(element)
  if (isBodyElement) {
    return {
      iconProps: {
        category: 'element',
        type: 'body',
        width: 12,
        height: 12,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isHead = element != null && MetadataUtils.isHead(element)
  if (isHead) {
    return {
      iconProps: {
        category: 'element',
        type: 'folder',
        width: 12,
        height: 12,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isHeading = element != null && MetadataUtils.isHeading(element)
  if (isHeading) {
    return {
      iconProps: {
        category: 'element',
        type: 'headline',
        width: 12,
        height: 12,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isButton = MetadataUtils.isButtonFromMetadata(element)
  if (isButton) {
    return {
      iconProps: {
        category: 'element',
        type: 'clickable',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isInput = element != null && MetadataUtils.isInput(element)
  if (isInput) {
    return {
      iconProps: {
        category: 'element',
        type: 'input',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isAnchorLink = element != null && MetadataUtils.isAnchorLink(element)
  if (isAnchorLink) {
    return {
      iconProps: {
        category: 'element',
        type: 'link',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isParagraph = element != null && MetadataUtils.isParagraph(element)
  if (isParagraph) {
    return {
      iconProps: {
        category: 'element',
        type: 'paragraph',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isForm = element != null && MetadataUtils.isForm(element)
  if (isForm) {
    return {
      iconProps: {
        category: 'element',
        type: 'form',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isGeneratedText = MetadataUtils.isGeneratedTextFromMetadata(
    elementPath,
    pathTrees,
    metadata,
  )
  if (isGeneratedText) {
    return {
      iconProps: {
        category: 'element',
        type: 'text-generated',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  const isText =
    MetadataUtils.isTextFromMetadata(element) || isConditionalBranchText(navigatorEntry, metadata)
  if (isText) {
    return {
      iconProps: {
        category: 'element',
        type: 'pure-text',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }
  const elementName = MetadataUtils.getJSXElementName(maybeEitherToMaybe(element?.element))
  if (elementName != null && isImg(elementName)) {
    return {
      iconProps: {
        category: 'element',
        type: 'image',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
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
        iconProps: {
          category: 'element',
          type: 'pure-text',
          width: 18,
          height: 18,
        },
        isPositionAbsolute: isPositionAbsolute,
      }
    }
  }

  if (MetadataUtils.isFlexLayoutedContainer(element)) {
    const flexDirection = MetadataUtils.getFlexDirection(element)
    if (flexDirection === 'row' || flexDirection === 'row-reverse') {
      return {
        iconProps: {
          category: 'layout/systems',
          type: 'flex-row',
          width: 18,
          height: 18,
        },
        isPositionAbsolute: isPositionAbsolute,
      }
    } else {
      return {
        iconProps: {
          category: 'layout/systems',
          type: 'flex-column',
          width: 18,
          height: 18,
        },
        isPositionAbsolute: isPositionAbsolute,
      }
    }
  }

  if (MetadataUtils.isGridLayoutedContainer(element)) {
    return {
      iconProps: {
        category: 'layout/systems',
        type: 'grid',
        width: 18,
        height: 18,
      },
      isPositionAbsolute: isPositionAbsolute,
    }
  }

  return {
    iconProps: {
      category: 'element',
      type: 'div',
      width: 18,
      height: 18,
    },
    isPositionAbsolute: isPositionAbsolute,
  }
}

function createComponentIconProps(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  autoFocusedPaths: Array<ElementPath>,
  filePathMappings: FilePathMappings,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): IcnPropsBase | null {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (MetadataUtils.isProbablySceneFromMetadata(element)) {
    return null
  }
  if (element?.isEmotionOrStyledComponent) {
    // todo make 12x12 version if we still support this
    return {
      category: 'component',
      type: 'styled',
      width: 18,
      height: 18,
    }
  }
  const isAnimatedComponent = isAnimatedElement(element)
  if (isAnimatedComponent) {
    // todo make 12x12 version if we still support this
    return {
      category: 'component',
      type: 'animated',
      width: 18,
      height: 18,
    }
  }
  const isRemixComponent =
    MetadataUtils.isImportedComponentFromMetadata(element, '@remix-run/react', null) ||
    MetadataUtils.isProbablyRemixSceneFromMetadata(element)
  if (isRemixComponent) {
    return {
      category: 'navigator-element',
      type: 'remix',
      width: 12,
      height: 12,
    }
  }
  const isImported = isImportedComponent(element, filePathMappings)
  if (isImported) {
    return {
      category: 'component',
      type: 'npm',
      width: 18,
      height: 18,
    }
  }
  const isComponent = MetadataUtils.isAutomaticOrManuallyFocusableComponent(
    path,
    metadata,
    autoFocusedPaths,
    filePathMappings,
    propertyControlsInfo,
    projectContents,
  )
  if (isComponent) {
    return {
      category: 'navigator-element',
      type: 'component-solid',
      width: 12,
      height: 12,
    }
  }

  return null
}
