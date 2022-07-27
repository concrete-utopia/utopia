import * as OPI from 'object-path-immutable'
import { FlexLength, LayoutSystem, Sides } from 'utopia-api/core'
import { getReorderDirection } from '../../components/canvas/controls/select-mode/yoga-utils'
import { getImageSize, scaleImageDimensions } from '../../components/images'
import {
  foldThese,
  makeThat,
  makeThis,
  makeThisAndThat,
  mergeThese,
  setThat,
  These,
} from '../../utils/these'
import Utils, { IndexPosition } from '../../utils/utils'
import { getLayoutProperty } from '../layout/getLayoutProperty'
import { FlexLayoutHelpers, LayoutHelpers } from '../layout/layout-helpers'
import {
  flattenArray,
  mapDropNulls,
  pluck,
  stripNulls,
  flatMapArray,
  uniqBy,
  reverse,
} from '../shared/array-utils'
import { intrinsicHTMLElementNamesThatSupportChildren } from '../shared/dom-utils'
import {
  alternativeEither,
  Either,
  eitherToMaybe,
  flatMapEither,
  foldEither,
  forEachRight,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
  traverseEither,
  Left,
  Right,
  maybeEitherToMaybe,
} from '../shared/either'
import {
  ElementInstanceMetadata,
  ElementsByUID,
  getJSXElementNameLastPart,
  getJSXElementNameNoPathName,
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXTextBlock,
  JSXAttributes,
  JSXElement,
  JSXElementChild,
  UtopiaJSXComponent,
  JSXElementName,
  getJSXElementNameAsString,
  isIntrinsicElement,
  jsxElementName,
  ElementInstanceMetadataMap,
  isIntrinsicHTMLElement,
  getJSXAttribute,
} from '../shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attributes'
import {
  CanvasPoint,
  CanvasRectangle,
  canvasRectangle,
  canvasRectangleToLocalRectangle,
  LocalRectangle,
  localRectangle,
  SimpleRectangle,
  Size,
} from '../shared/math-utils'
import { optionalMap } from '../shared/optional-utils'
import {
  ElementOriginType,
  Imports,
  isUnknownOrGeneratedElement,
  NodeModules,
  PropertyPath,
  ElementPath,
} from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import { findJSXElementChildAtPath, getUtopiaID, isSceneElement } from './element-template-utils'
import {
  isImportedComponent,
  isAnimatedElement,
  isGivenUtopiaAPIElement,
  isUtopiaAPIComponent,
  getUtopiaJSXComponentsFromSuccess,
  isViewLikeFromMetadata,
  isSceneFromMetadata,
  isUtopiaAPIComponentFromMetadata,
  isGivenUtopiaElementFromMetadata,
  isImportedComponentNPM,
} from './project-file-utils'
import { ResizesContentProp } from './scene-utils'
import { fastForEach } from '../shared/utils'
import { objectValues, omit } from '../shared/object-utils'
import { UTOPIA_LABEL_KEY } from './utopia-constants'
import { AllElementProps, withUnderlyingTarget } from '../../components/editor/store/editor-state'
import { ProjectContentTreeRoot } from '../../components/assets'
import { memoize } from '../shared/memoize'
import { buildTree, ElementPathTree, getSubTree } from '../shared/element-path-tree'
const ObjectPathImmutable: any = OPI

export const getChildrenOfCollapsedViews = (
  elementPaths: ElementPath[],
  collapsedViews: Array<ElementPath>,
): Array<ElementPath> => {
  return Utils.flatMapArray((view) => {
    return Utils.stripNulls(
      elementPaths.map((childPath) => {
        return EP.isDescendantOf(childPath, view) ? childPath : null
      }),
    )
  }, collapsedViews)
}

const ElementsToDrillIntoForTextContent = ['div', 'span']

export const MetadataUtils = {
  getElementOriginType(
    elements: Array<UtopiaJSXComponent>,
    target: ElementPath,
  ): ElementOriginType {
    const staticTarget = EP.dynamicPathToStaticPath(target)
    if (staticTarget == null) {
      return 'unknown-element'
    } else {
      if (EP.pathsEqual(target, staticTarget)) {
        return 'statically-defined'
      } else {
        const element = findJSXElementChildAtPath(elements, staticTarget)
        if (element != null && isJSXElement(element)) {
          return 'generated-static-definition-present'
        } else {
          return 'unknown-element'
        }
      }
    }
  },
  anyUnknownOrGeneratedElements(
    projectContents: ProjectContentTreeRoot,
    nodeModules: NodeModules,
    openFile: string | null,
    targets: Array<ElementPath>,
  ): boolean {
    return targets.some((target) => {
      const elementOriginType = withUnderlyingTarget<ElementOriginType>(
        target,
        projectContents,
        nodeModules,
        openFile,
        'unknown-element',
        (success, element, underlyingTarget, underlyingFilePath, underlyingDynamicTarget) => {
          return MetadataUtils.getElementOriginType(
            getUtopiaJSXComponentsFromSuccess(success),
            underlyingDynamicTarget,
          )
        },
      )
      return isUnknownOrGeneratedElement(elementOriginType)
    })
  },
  findElementByElementPath(
    elementMap: ElementInstanceMetadataMap,
    path: ElementPath | null,
  ): ElementInstanceMetadata | null {
    if (path == null) {
      return null
    } else {
      return elementMap[EP.toString(path)] ?? null
    }
  },
  findElementsByElementPath(
    elementMap: ElementInstanceMetadataMap,
    paths: Array<ElementPath>,
  ): Array<ElementInstanceMetadata> {
    return stripNulls(paths.map((path) => MetadataUtils.findElementByElementPath(elementMap, path)))
  },
  isSceneTreatedAsGroup(allElementProps: AllElementProps, path: ElementPath): boolean {
    return allElementProps?.[EP.toString(path)]?.[ResizesContentProp] ?? false
  },
  isProbablySceneFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element != null &&
      element.importInfo != null &&
      foldEither(
        (_) => false,
        (info) => info.path === 'utopia-api' && info.originalName === 'Scene',
        element.importInfo,
      )
    )
  },
  isProbablyScene(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    return MetadataUtils.isProbablySceneFromMetadata(elementMetadata)
  },
  getViewZIndexFromMetadata(metadata: ElementInstanceMetadataMap, target: ElementPath): number {
    const siblings = MetadataUtils.getSiblings(metadata, target)
    return siblings.findIndex((child) => {
      return getUtopiaID(child) === EP.toUid(target)
    })
  },
  getParent(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): ElementInstanceMetadata | null {
    if (target == null) {
      return null
    }
    const parentPath = EP.parentPath(target)
    return MetadataUtils.findElementByElementPath(metadata, parentPath)
  },
  getSiblings(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    if (target == null) {
      return []
    }

    const parentPath = EP.parentPath(target)
    const siblingPathsOrNull = EP.isRootElementOfInstance(target)
      ? MetadataUtils.getRootViewPaths(metadata, parentPath)
      : MetadataUtils.getChildrenPaths(metadata, parentPath)
    const siblingPaths = siblingPathsOrNull ?? []
    return MetadataUtils.findElementsByElementPath(metadata, siblingPaths)
  },
  isParentYogaLayoutedContainerAndElementParticipatesInLayout(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): boolean {
    const instance = MetadataUtils.findElementByElementPath(metadata, path)
    return (
      optionalMap(
        MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout,
        instance,
      ) ?? false
    )
  },
  isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
    element: ElementInstanceMetadata | null,
  ): boolean {
    if (element == null) {
      return false
    }
    return (
      MetadataUtils.isParentYogaLayoutedContainerForElement(element) &&
      !MetadataUtils.isPositionAbsolute(element)
    )
  },
  isParentYogaLayoutedContainerForElement(element: ElementInstanceMetadata): boolean {
    return element.specialSizeMeasurements.parentLayoutSystem === 'flex'
  },
  isFlexLayoutedContainer(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.layoutSystemForChildren === 'flex'
  },
  isGridLayoutedContainer(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.layoutSystemForChildren === 'grid'
  },
  isPositionAbsolute(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.position === 'absolute'
  },
  isButtonFromMetadata(element: ElementInstanceMetadata | null): boolean {
    const elementName = MetadataUtils.getJSXElementName(maybeEitherToMaybe(element?.element))
    if (
      elementName != null &&
      PP.depth(elementName.propertyPath) === 0 &&
      elementName.baseVariable === 'button'
    ) {
      return true
    }
    let buttonRoleFound: boolean = false
    if (element != null) {
      forEachRight(element.element, (elem) => {
        if (isJSXElement(elem)) {
          const attrResult = getSimpleAttributeAtPath(right(elem.props), PP.create(['role']))
          forEachRight(attrResult, (value) => {
            if (value === 'button') {
              buttonRoleFound = true
            }
          })
        }
      })
    }
    if (buttonRoleFound) {
      return true
    } else {
      return element?.specialSizeMeasurements.htmlElementName.toLowerCase() === 'button'
    }
  },
  isButton(target: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const instance = MetadataUtils.findElementByElementPath(metadata, target)
    return MetadataUtils.isButtonFromMetadata(instance)
  },
  getYogaSizeProps(
    target: ElementPath,
    metadata: ElementInstanceMetadataMap,
    propertyTarget: Array<string>,
  ): Partial<Size> {
    const parentInstance = this.getParent(metadata, target)
    if (parentInstance == null) {
      return {}
    } else {
      const flexDirection = getReorderDirection(this.getFlexDirection(parentInstance))

      const staticTarget = EP.dynamicPathToStaticPath(target)
      if (staticTarget == null) {
        return {}
      } else {
        const element = maybeEitherToMaybe(
          MetadataUtils.findElementByElementPath(metadata, target)?.element,
        )
        if (element != null && isJSXElement(element)) {
          const widthLookupAxis = flexDirection === 'horizontal' ? 'flexBasis' : 'width'
          const heightLookupAxis = flexDirection === 'vertical' ? 'flexBasis' : 'height'
          let result: Partial<Size> = {}
          const width: Either<string, FlexLength> = alternativeEither(
            getLayoutProperty(widthLookupAxis, right(element.props), propertyTarget),
            getLayoutProperty('width', right(element.props), propertyTarget),
          )
          const height: Either<string, FlexLength> = alternativeEither(
            getLayoutProperty(heightLookupAxis, right(element.props), propertyTarget),
            getLayoutProperty('height', right(element.props), propertyTarget),
          )
          // FIXME We should really be supporting string values here
          forEachRight(width, (w) => {
            if (w != null && typeof w === 'number') {
              result.width = w
            }
          })
          forEachRight(height, (h) => {
            if (h != null && typeof h === 'number') {
              result.height = h
            }
          })
          return result
        } else {
          return {}
        }
      }
    }
  },
  getElementMargin(path: ElementPath, metadata: ElementInstanceMetadataMap): Partial<Sides> | null {
    const instance = MetadataUtils.findElementByElementPath(metadata, path)
    if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
      return instance.specialSizeMeasurements.margin
    } else {
      return null
    }
  },
  getElementPadding(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): Partial<Sides> | null {
    const instance = MetadataUtils.findElementByElementPath(metadata, path)
    if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
      return instance.specialSizeMeasurements.padding
    } else {
      return null
    }
  },
  getFlexDirection: function (instance: ElementInstanceMetadata | null): string {
    return instance?.specialSizeMeasurements?.flexDirection ?? 'row'
  },
  findParent(metadata: ElementInstanceMetadataMap, target: ElementPath): ElementPath | null {
    const parentPath = EP.parentPath(target)

    if (parentPath == null) {
      return null
    } else if (EP.isStoryboardChild(parentPath)) {
      // we've reached the top
      return parentPath
    } else {
      return parentPath
    }
  },
  setPropertyDirectlyIntoMetadata(
    allElementProps: AllElementProps,
    target: ElementPath,
    property: PropertyPath,
    value: any,
  ): AllElementProps {
    return ObjectPathImmutable.set(
      allElementProps,
      [EP.toString(target), ...PP.getElements(property)],
      value,
    )
  },
  unsetPropertyDirectlyIntoMetadata(
    allElementProps: AllElementProps,
    target: ElementPath,
    property: PropertyPath,
  ): AllElementProps {
    return ObjectPathImmutable.del(allElementProps, [
      EP.toString(target),
      ...PP.getElements(property),
    ])
  },
  getRootViewPaths(elements: ElementInstanceMetadataMap, target: ElementPath): Array<ElementPath> {
    const possibleRootElementsOfTarget = mapDropNulls((elementPathString) => {
      const elementPath = EP.fromString(elementPathString)
      if (EP.isRootElementOf(elementPath, target)) {
        return elementPath
      } else {
        return null
      }
    }, Object.keys(elements))
    return possibleRootElementsOfTarget
  },
  getRootViews(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    let result: Array<ElementInstanceMetadata> = []
    for (const elementKey in elements) {
      const element = elements[elementKey]
      const elementPath = element.elementPath
      if (EP.isRootElementOf(elementPath, target)) {
        result.push(element)
      }
    }
    return result
  },
  getChildrenPaths(elements: ElementInstanceMetadataMap, target: ElementPath): Array<ElementPath> {
    const possibleChildren = mapDropNulls((elementPathString) => {
      const elementPath = EP.fromString(elementPathString)
      if (EP.isChildOf(elementPath, target) && !EP.isRootElementOfInstance(elementPath)) {
        return elementPath
      } else {
        return null
      }
    }, Object.keys(elements))
    return possibleChildren
  },
  getChildren(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    let result: Array<ElementInstanceMetadata> = []
    for (const elementKey in elements) {
      const element = elements[elementKey]
      const elementPath = element.elementPath
      if (EP.isChildOf(elementPath, target) && !EP.isRootElementOfInstance(elementPath)) {
        result.push(element)
      }
    }
    return result
  },
  getImmediateChildrenPaths(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
    const element = MetadataUtils.findElementByElementPath(elements, target)
    if (element == null) {
      return []
    } else {
      const rootPaths = MetadataUtils.getRootViewPaths(elements, target)
      const childrenPaths = MetadataUtils.getChildrenPaths(elements, target)
      return [...rootPaths, ...childrenPaths]
    }
  },
  getImmediateChildren(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const roots = MetadataUtils.getRootViews(metadata, target)
    const children = MetadataUtils.getChildren(metadata, target)
    return [...roots, ...children]
  },
  getStoryboardMetadata(metadata: ElementInstanceMetadataMap): ElementInstanceMetadata | null {
    for (const metadataKey in metadata) {
      const metadataEntry = metadata[metadataKey]
      if (EP.isStoryboardPath(metadataEntry.elementPath)) {
        return metadataEntry
      }
    }
    return null
  },
  getAllStoryboardChildren(metadata: ElementInstanceMetadataMap): ElementInstanceMetadata[] {
    const storyboardMetadata = MetadataUtils.getStoryboardMetadata(metadata)
    return storyboardMetadata == null
      ? []
      : MetadataUtils.getImmediateChildren(metadata, storyboardMetadata.elementPath)
  },
  getAllStoryboardChildrenPaths(metadata: ElementInstanceMetadataMap): ElementPath[] {
    const storyboardMetadata = MetadataUtils.getStoryboardMetadata(metadata)
    return storyboardMetadata == null
      ? []
      : MetadataUtils.getImmediateChildrenPaths(metadata, storyboardMetadata.elementPath)
  },
  getAllCanvasRootPaths(metadata: ElementInstanceMetadataMap): ElementPath[] {
    const rootScenesAndElements = MetadataUtils.getAllStoryboardChildren(metadata)
    return flatMapArray<ElementInstanceMetadata, ElementPath>((root) => {
      const rootElements = MetadataUtils.getRootViewPaths(metadata, root.elementPath)
      if (rootElements.length > 0) {
        return rootElements
      } else {
        return [root.elementPath]
      }
    }, rootScenesAndElements)
  },
  getAllPaths: memoize(
    (metadata: ElementInstanceMetadataMap): ElementPath[] => {
      // Note: This will not necessarily be representative of the structured ordering in
      // the code that produced these elements.
      const paths = objectValues(metadata).map((m) => m.elementPath)
      const projectTree = buildTree(paths)

      // This function needs to explicitly return the paths in a depth first manner
      let result: Array<ElementPath> = []
      function recurseElement(tree: ElementPathTree): void {
        result.push(tree.path)
        fastForEach(tree.children, (childTree) => {
          recurseElement(childTree)
        })
      }

      const storyboardChildren = MetadataUtils.getAllStoryboardChildrenPaths(metadata)
      fastForEach(storyboardChildren, (childPath) => {
        const subTree = getSubTree(projectTree, childPath)
        if (subTree != null) {
          recurseElement(subTree)
        }
      })

      const uniqueResult = uniqBy<ElementPath>(result, EP.pathsEqual)

      return uniqueResult
    },
    { maxSize: 1 },
  ),
  getAllPathsIncludingUnfurledFocusedComponents(
    metadata: ElementInstanceMetadataMap,
  ): ElementPath[] {
    // Note: This will not necessarily be representative of the structured ordering in
    // the code that produced these elements.
    const projectTree = buildTree(objectValues(metadata).map((m) => m.elementPath))
    // This function needs to explicitly return the paths in a depth first manner
    let result: Array<ElementPath> = []
    function recurseElement(tree: ElementPathTree | null): void {
      if (tree != null) {
        result.push(tree.path)

        fastForEach(tree.children, (childTree) => {
          recurseElement(childTree)
        })
      }
    }

    const rootInstances = this.getAllStoryboardChildrenPaths(metadata)

    fastForEach(rootInstances, (rootInstance) => {
      const element = MetadataUtils.findElementByElementPath(metadata, rootInstance)
      if (element != null) {
        result.push(rootInstance)
        const rootElements = MetadataUtils.getRootViewPaths(metadata, element.elementPath)
        fastForEach(rootElements, (rootPath) => {
          const subTree = getSubTree(projectTree, rootPath)
          recurseElement(subTree)
        })
        const children = MetadataUtils.getChildrenPaths(metadata, element.elementPath)
        fastForEach(children, (child) => {
          const subTree = getSubTree(projectTree, child)
          recurseElement(subTree)
        })
      }
    })

    return uniqBy<ElementPath>(result, EP.pathsEqual)
  },
  isElementOfType(instance: ElementInstanceMetadata, elementType: string): boolean {
    return foldEither(
      (name) => name === elementType,
      (element) => isJSXElement(element) && getJSXElementNameLastPart(element.name) === elementType,
      instance.element,
    )
  },
  isUtopiaAPIElementFromImports(imports: Imports, instance: ElementInstanceMetadata): boolean {
    return foldEither(
      (_) => false,
      (element) => isJSXElement(element) && isUtopiaAPIComponent(element.name, imports),
      instance.element,
    )
  },
  isGivenUtopiaAPIElementFromImports(
    instance: ElementInstanceMetadata,
    elementType: string,
  ): boolean {
    // KILLME Replace with isGivenUtopiaElementFromMetadata from project-file-utils.ts
    return isGivenUtopiaElementFromMetadata(instance, elementType)
  },
  isViewAgainstImports(instance: ElementInstanceMetadata | null): boolean {
    return instance != null && MetadataUtils.isGivenUtopiaAPIElementFromImports(instance, 'View')
  },
  isImg(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'img')
  },
  isTextAgainstImports(instance: ElementInstanceMetadata | null): boolean {
    return instance != null && MetadataUtils.isGivenUtopiaAPIElementFromImports(instance, 'Text')
  },
  isDiv(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'div')
  },
  isSpan(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'span')
  },
  overflows(allElementProps: AllElementProps, path: ElementPath): boolean {
    const elementProps = allElementProps[EP.toString(path)] ?? {}
    const styleProps = elementProps.style ?? null
    if (styleProps != null) {
      const overflow = Utils.propOr('visible', 'overflow', styleProps)
      return overflow !== 'hidden' && overflow !== 'clip'
    } else {
      return false
    }
  },
  targetElementSupportsChildren(instance: ElementInstanceMetadata): boolean {
    // FIXME Replace with a property controls check
    const elementEither = instance.element

    if (isLeft(elementEither)) {
      return intrinsicHTMLElementNamesThatSupportChildren.includes(elementEither.value)
    } else {
      const element = elementEither.value
      if (isJSXElement(element) && isUtopiaAPIComponentFromMetadata(instance)) {
        // Explicitly prevent components / elements that we *know* don't support children
        return (
          isViewLikeFromMetadata(instance) ||
          isSceneFromMetadata(instance) ||
          isGivenUtopiaElementFromMetadata(instance, 'Text')
        )
      } else {
        // We don't know at this stage
        return true
      }
    }
  },
  targetSupportsChildren(metadata: ElementInstanceMetadataMap, target: ElementPath): boolean {
    const instance = MetadataUtils.findElementByElementPath(metadata, target)
    return instance == null ? false : MetadataUtils.targetElementSupportsChildren(instance)
  },
  getTextContentOfElement(element: ElementInstanceMetadata): string | null {
    if (isRight(element.element) && isJSXElement(element.element.value)) {
      if (element.element.value.children.length === 1) {
        const childElement = element.element.value.children[0]
        if (isJSXTextBlock(childElement)) {
          return childElement.text
        } else if (isJSXArbitraryBlock(childElement)) {
          return `{${childElement.originalJavascript}}`
        }
      } else if (element.element.value.children.length === 0) {
        return ''
      }
    }
    return null
  },
  // TODO update this to work with the natural width / height
  getImageMultiplier(
    metadata: ElementInstanceMetadataMap,
    targets: Array<ElementPath>,
    allElementProps: AllElementProps,
  ): number | null {
    const multipliers: Set<number> = Utils.emptySet()
    Utils.fastForEach(targets, (target) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, target)
      if (instance != null && this.isImg(instance)) {
        const componentFrame = instance.localFrame
        if (componentFrame != null) {
          const imageSize = getImageSize(allElementProps, instance)
          const widthMultiplier = imageSize.width / componentFrame.width
          const roundedMultiplier = Utils.roundTo(widthMultiplier, 0)
          // Recalculate the scaled dimensions to see if they match.
          const scaledSize = scaleImageDimensions(imageSize, roundedMultiplier)
          const roundedSize = {
            width: Utils.roundTo(scaledSize.width, 0),
            height: Utils.roundTo(scaledSize.height, 0),
          }
          if (
            roundedSize.width === componentFrame.width &&
            roundedSize.height === componentFrame.height
          ) {
            multipliers.add(roundedMultiplier)
          }
        }
      }
    })
    if (multipliers.size === 1) {
      return multipliers.values().next().value
    } else {
      return null
    }
  },
  getAllChildrenIncludingUnfurledFocusedComponents(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): { children: Array<ElementPath>; unfurledComponents: Array<ElementPath> } {
    return {
      children: MetadataUtils.getChildrenPaths(metadata, path),
      unfurledComponents: MetadataUtils.getRootViewPaths(metadata, path),
    }
  },
  getAllChildrenElementsIncludingUnfurledFocusedComponents(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): {
    children: Array<ElementInstanceMetadata>
    unfurledComponents: Array<ElementInstanceMetadata>
  } {
    return {
      children: MetadataUtils.getChildren(metadata, path),
      unfurledComponents: MetadataUtils.getRootViews(metadata, path),
    }
  },
  createOrderedElementPathsFromElements: memoize(
    (
      metadata: ElementInstanceMetadataMap,
      collapsedViews: Array<ElementPath>,
    ): {
      navigatorTargets: Array<ElementPath>
      visibleNavigatorTargets: Array<ElementPath>
    } => {
      // Note: This will not necessarily be representative of the structured ordering in
      // the code that produced these elements.
      const projectTree = buildTree(objectValues(metadata).map((m) => m.elementPath))

      // This function exists separately from getAllPaths because the Navigator handles collapsed views
      let navigatorTargets: Array<ElementPath> = []
      let visibleNavigatorTargets: Array<ElementPath> = []

      function walkAndAddKeys(subTree: ElementPathTree | null, collapsedAncestor: boolean): void {
        if (subTree != null) {
          const path = subTree.path
          navigatorTargets.push(path)
          if (!collapsedAncestor) {
            visibleNavigatorTargets.push(path)
          }

          const isCollapsed = EP.containsPath(path, collapsedViews)
          const newCollapsedAncestor = collapsedAncestor || isCollapsed

          let unfurledComponents: Array<ElementPathTree> = []
          fastForEach(subTree.children, (child) => {
            if (EP.isRootElementOfInstance(child.path)) {
              unfurledComponents.push(child)
            } else {
              walkAndAddKeys(child, newCollapsedAncestor)
            }
          })

          fastForEach(unfurledComponents, (unfurledComponent) => {
            walkAndAddKeys(unfurledComponent, newCollapsedAncestor)
          })
        }
      }

      const canvasRoots = MetadataUtils.getAllStoryboardChildrenPaths(metadata)
      fastForEach(canvasRoots, (childElement) => {
        const subTree = getSubTree(projectTree, childElement)

        walkAndAddKeys(subTree, false)
      })

      return {
        navigatorTargets: navigatorTargets,
        visibleNavigatorTargets: visibleNavigatorTargets,
      }
    },
    {
      maxSize: 1,
    },
  ),
  transformAtPathOptionally(
    elementMap: ElementInstanceMetadataMap,
    path: ElementPath,
    transform: (element: ElementInstanceMetadata) => ElementInstanceMetadata,
  ): ElementInstanceMetadataMap {
    const existing = MetadataUtils.findElementByElementPath(elementMap, path)
    if (existing == null) {
      return elementMap
    } else {
      const transformed = transform(existing)
      if (transformed === existing) {
        return elementMap
      } else {
        return {
          ...elementMap,
          [EP.toString(path)]: transformed,
        }
      }
    }
  },
  getFrameInCanvasCoords(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): CanvasRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return Utils.optionalMap((e) => e.globalFrame, element)
  },
  getFrame(path: ElementPath, metadata: ElementInstanceMetadataMap): LocalRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return Utils.optionalMap((e) => e.localFrame, element)
  },
  getFrameRelativeTo: function (
    parent: ElementPath | null,
    metadata: ElementInstanceMetadataMap,
    frame: CanvasRectangle,
  ): LocalRectangle {
    if (parent == null) {
      return Utils.asLocal(frame)
    } else {
      const paths = EP.allPathsForLastPart(parent)
      const parentFrames: Array<LocalRectangle> = Utils.stripNulls(
        paths.map((path) => this.getFrame(path, metadata)),
      )
      return parentFrames.reduce((working, next) => {
        return Utils.offsetRect(working, {
          x: -next.x,
          y: -next.y,
        } as LocalRectangle)
      }, Utils.asLocal(frame))
    }
  },
  getFrameRelativeToTargetContainingBlock: function (
    targetParent: ElementPath | null,
    metadata: ElementInstanceMetadataMap,
    frame: CanvasRectangle,
  ): LocalRectangle {
    const parent = this.findElementByElementPath(metadata, targetParent)
    if (parent != null) {
      if (
        parent.specialSizeMeasurements.providesBoundsForAbsoluteChildren &&
        parent.globalFrame != null
      ) {
        return canvasRectangleToLocalRectangle(frame, parent.globalFrame)
      }
      if (parent.specialSizeMeasurements.coordinateSystemBounds != null) {
        return canvasRectangleToLocalRectangle(
          frame,
          parent.specialSizeMeasurements.coordinateSystemBounds,
        )
      }
    }
    return Utils.asLocal(frame)
  },
  getElementLabelFromProps(allElementProps: AllElementProps, path: ElementPath): string | null {
    const dataLabelProp = allElementProps?.[EP.toString(path)]?.[UTOPIA_LABEL_KEY]
    if (dataLabelProp != null && typeof dataLabelProp === 'string' && dataLabelProp.length > 0) {
      return dataLabelProp
    } else {
      return null
    }
  },
  getElementLabelFromMetadata(
    allElementProps: AllElementProps,
    element: ElementInstanceMetadata,
    staticName: JSXElementName | null = null,
  ): string {
    const sceneLabel = element.label // KILLME?
    const dataLabelProp = MetadataUtils.getElementLabelFromProps(
      allElementProps,
      element.elementPath,
    )
    if (dataLabelProp != null) {
      return dataLabelProp
    } else if (sceneLabel != null) {
      return sceneLabel
    } else {
      const possibleName: string = foldEither(
        (tagName) => {
          const staticNameString = optionalMap(getJSXElementNameAsString, staticName)
          return staticNameString ?? tagName
        },
        (jsxElement) => {
          switch (jsxElement.type) {
            case 'JSX_ELEMENT':
              const lastNamePart = getJSXElementNameLastPart(jsxElement.name)
              // Check for certain elements and check if they have text content within them.
              if (ElementsToDrillIntoForTextContent.includes(lastNamePart)) {
                const firstChild = jsxElement.children[0]
                if (firstChild != null) {
                  if (isJSXTextBlock(firstChild)) {
                    return firstChild.text
                  }
                  if (isJSXArbitraryBlock(firstChild)) {
                    return `{${firstChild.originalJavascript}}`
                  }
                }
              }
              // With images, take their alt and src properties as possible names first.
              const elementProps = allElementProps[EP.toString(element.elementPath)] ?? {}
              if (lastNamePart === 'img') {
                const alt = elementProps['alt']
                if (alt != null && typeof alt === 'string' && alt.length > 0) {
                  return alt
                }
                const src = elementProps['src']
                if (src != null && typeof src === 'string' && src.length > 0) {
                  return src
                }
              }
              // For Text elements, use their text property if it exists.
              if (lastNamePart === 'Text') {
                const text = elementProps['text']
                if (text != null && typeof text === 'string' && text.length > 0) {
                  return text
                }
              }

              return lastNamePart
            case 'JSX_TEXT_BLOCK':
              return '(text)'
            case 'JSX_ARBITRARY_BLOCK':
              return '(code)'
            case 'JSX_FRAGMENT':
              return '(fragment)'
            default:
              const _exhaustiveCheck: never = jsxElement
              throw new Error(`Unexpected element type ${jsxElement}`)
          }
        },
        element.element,
      )
      if (possibleName != null) {
        return possibleName
      }
      if (isRight(element.element)) {
        if (isJSXElement(element.element.value)) {
          return getJSXElementNameLastPart(element.element.value.name).toString()
        }
      }
    }

    // Default catch all name, will probably avoid some odd cases in the future.
    return 'Element'
  },
  getElementLabel(
    allElementProps: AllElementProps,
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    staticName: JSXElementName | null = null,
  ): string {
    const element = this.findElementByElementPath(metadata, path)
    if (element != null) {
      return MetadataUtils.getElementLabelFromMetadata(allElementProps, element, staticName)
    }

    // Default catch all name, will probably avoid some odd cases in the future.
    return 'Element'
  },
  getJSXElementFromMetadata(
    metadata: ElementInstanceMetadataMap,
    path: ElementPath,
  ): JSXElement | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    if (element == null) {
      return null
    } else {
      return foldEither(
        (_) => null,
        (e) => (isJSXElement(e) ? e : null),
        element.element,
      )
    }
  },
  getJSXElementName(jsxElement: JSXElementChild | null): JSXElementName | null {
    if (jsxElement != null) {
      if (isJSXElement(jsxElement)) {
        return jsxElement.name
      } else {
        return null
      }
    } else {
      return null
    }
  },
  getJSXElementNameFromMetadata(
    metadata: ElementInstanceMetadataMap,
    path: ElementPath,
  ): JSXElementName | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    if (element != null) {
      return MetadataUtils.getJSXElementName(eitherToMaybe(element.element))
    } else {
      return null
    }
  },
  getJSXElementBaseName(path: ElementPath, components: Array<UtopiaJSXComponent>): string | null {
    const jsxElement = findElementAtPath(path, components)
    if (jsxElement != null) {
      if (isJSXElement(jsxElement)) {
        return jsxElement.name.baseVariable
      } else {
        return null
      }
    } else {
      return null
    }
  },
  getJSXElementTagName(path: ElementPath, components: Array<UtopiaJSXComponent>): string | null {
    const jsxElement = findElementAtPath(path, components)
    if (jsxElement != null) {
      if (isJSXElement(jsxElement)) {
        return getJSXElementNameAsString(jsxElement.name)
      } else {
        return null
      }
    } else {
      return null
    }
  },
  getDuplicationParentTargets(targets: ElementPath[]): ElementPath | null {
    return EP.getCommonParent(targets)
  },
  mergeComponentMetadata(
    elementsByUID: ElementsByUID,
    fromSpy: ElementInstanceMetadataMap,
    fromDOM: ElementInstanceMetadataMap,
  ): ElementInstanceMetadataMap {
    // This logic effectively puts everything from the spy first,
    // then anything missed out from the DOM right after it.
    // Ideally this would function like a VCS diff inserting runs of new elements
    // inbetween matching metadata, so it may be necessary to implement something
    // like that in the future. But for now this is likely "good enough" that it
    // wont make any difference.
    let workingElements: ElementInstanceMetadataMap = { ...fromSpy }
    let newlyFoundElements: Array<ElementPath> = []
    fastForEach(Object.keys(fromDOM), (pathStr) => {
      const domElem = fromDOM[pathStr]
      const spyElem = fromSpy[pathStr]

      if (spyElem == null) {
        workingElements[pathStr] = domElem
        newlyFoundElements.push(domElem.elementPath)
      } else {
        let componentInstance = spyElem.componentInstance || domElem.componentInstance
        let jsxElement = alternativeEither(spyElem.element, domElem.element)

        const elemUID: string | null = EP.toStaticUid(domElem.elementPath)
        const possibleElement = elementsByUID[elemUID]
        if (possibleElement != null) {
          if (!isIntrinsicElement(possibleElement.name)) {
            componentInstance = true
            jsxElement = right(possibleElement)
          }
        }

        const elem: ElementInstanceMetadata = {
          ...domElem,
          element: jsxElement,
          componentInstance: componentInstance,
          isEmotionOrStyledComponent: spyElem.isEmotionOrStyledComponent,
          label: spyElem.label,
          importInfo: spyElem.importInfo,
        }
        workingElements[EP.toString(domElem.elementPath)] = elem
      }
    })

    return workingElements
  },
  isStaticElement(elements: Array<UtopiaJSXComponent>, target: ElementPath): boolean {
    const originType = this.getElementOriginType(elements, target)
    return originType === 'statically-defined'
  },
  removeElementMetadataChild(
    target: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): ElementInstanceMetadataMap {
    // Note this only removes the child element from the metadata, but keeps grandchildren in there (inaccessible). Is this a memory leak?
    let remainingElements: ElementInstanceMetadataMap = omit([EP.toString(target)], metadata)
    if (Object.keys(remainingElements).length === Object.keys(metadata).length) {
      // Nothing was removed
      return metadata
    } else {
      return remainingElements
    }
  },
  insertElementMetadataChild(
    targetParent: ElementPath | null,
    elementToInsert: ElementInstanceMetadata,
    metadata: ElementInstanceMetadataMap,
  ): ElementInstanceMetadataMap {
    // Insert into the map
    if (!EP.pathsEqual(EP.parentPath(elementToInsert.elementPath), targetParent)) {
      throw new Error(
        `insertElementMetadataChild: trying to insert child metadata with incorrect parent path prefix.
        Target parent: ${EP.toString(targetParent!)},
        child path: ${EP.toString(elementToInsert.elementPath)}`,
      )
    }

    const withNewElement: ElementInstanceMetadataMap = {
      ...metadata,
      [EP.toString(elementToInsert.elementPath)]: elementToInsert,
    }
    return withNewElement
  },
  duplicateElementMetadataAtPath(
    oldPath: ElementPath,
    newPath: ElementPath,
    newElement: Either<string, JSXElementChild>,
    metadata: ElementInstanceMetadataMap,
  ): ElementInstanceMetadataMap {
    let workingElements = { ...metadata }

    function duplicateElementMetadata(
      element: ElementInstanceMetadata,
      pathToReplace: ElementPath,
      pathToReplaceWith: ElementPath,
      newElementInner: Either<string, JSXElementChild>,
    ): ElementPath {
      const newElementPath = EP.replaceIfAncestor(
        element.elementPath,
        pathToReplace,
        pathToReplaceWith,
      )!

      const newElementMetadata: ElementInstanceMetadata = {
        ...element,
        elementPath: newElementPath,
        element: newElementInner,
      }

      workingElements[EP.toString(newElementPath)] = newElementMetadata
      return newElementPath
    }

    // Everything about this feels wrong
    const originalMetadata = MetadataUtils.findElementByElementPath(metadata, oldPath)
    if (originalMetadata == null) {
      return metadata
    } else {
      duplicateElementMetadata(originalMetadata, oldPath, newPath, newElement)

      return workingElements
    }
  },
  transformAllPathsInMetadata(
    metadata: ElementInstanceMetadataMap,
    replaceSearch: ElementPath,
    replaceWith: ElementPath | null,
  ): ElementInstanceMetadataMap {
    let updatedElements: ElementInstanceMetadataMap = { ...metadata }

    const allPathsWithReplacements = Object.values(metadata)
      .map((e) => e.elementPath)
      .filter((path) => EP.isDescendantOfOrEqualTo(path, replaceSearch))
      .map((path) => {
        const replacement = EP.replaceOrDefault(path, replaceSearch, replaceWith)
        return {
          path: path,
          replacement: replacement,
          pathString: EP.toString(path),
          replacementString: EP.toString(replacement),
        }
      })

    // TODO updateChildren should actually change the keys of the children in the metadata...
    function updateChildren(children: ElementPath[]): ElementPath[] {
      let childWasUpdated = false
      const updatedChildren = children.map((child) => {
        const replacementChild = allPathsWithReplacements.find((pathWithReplacement) =>
          EP.pathsEqual(pathWithReplacement.path, child),
        )
        childWasUpdated = childWasUpdated && replacementChild != null
        return replacementChild == null ? child : replacementChild.replacement
      })

      return childWasUpdated ? updatedChildren : children
    }

    fastForEach(
      allPathsWithReplacements,
      ({ path, replacement, pathString, replacementString }) => {
        const existing = MetadataUtils.findElementByElementPath(updatedElements, path)
        if (existing != null) {
          delete updatedElements[pathString]
          updatedElements[replacementString] = {
            ...existing,
            elementPath: replacement,
          }
        }
      },
    )

    return updatedElements
  },
  findElementMetadata(
    target: ElementPath,
    elements: ReadonlyArray<ElementInstanceMetadata>,
  ): ElementInstanceMetadata | null {
    return elements.find((elem) => EP.pathsEqual(target, elem.elementPath)) ?? null
  },
  getStaticElementName(
    path: ElementPath,
    rootElements: Array<UtopiaJSXComponent>,
  ): JSXElementName | null {
    const staticPath = EP.dynamicPathToStaticPath(path)
    const jsxElement = optionalMap((p) => findJSXElementChildAtPath(rootElements, p), staticPath)
    return optionalMap((element) => (isJSXElement(element) ? element.name : null), jsxElement)
  },
  isComponentInstance(path: ElementPath, rootElements: Array<UtopiaJSXComponent>): boolean {
    const elementName = MetadataUtils.getStaticElementName(path, rootElements)
    return elementName != null && !isIntrinsicHTMLElement(elementName)
  },
  isPinnedAndNotAbsolutePositioned(
    metadata: ElementInstanceMetadataMap,
    view: ElementPath,
  ): boolean {
    // Disable snapping and guidelines for pinned elements marked with relative positioning:
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, view)
    return (
      elementMetadata != null &&
      elementMetadata.specialSizeMeasurements.parentLayoutSystem === 'flow' &&
      !MetadataUtils.isPositionAbsolute(elementMetadata)
    )
  },
  walkMetadata(
    metadata: ElementInstanceMetadataMap,
    withEachElement: (
      element: ElementInstanceMetadata,
      parentMetadata: ElementInstanceMetadata | null,
    ) => void,
  ): void {
    fastForEach(Object.values(metadata), (elem) => {
      const parentPath = EP.parentPath(elem.elementPath)
      const parent = MetadataUtils.findElementByElementPath(metadata, parentPath)
      withEachElement(elem, parent)
    })
  },
  findContainingBlock(
    elementMap: ElementInstanceMetadataMap,
    path: ElementPath,
  ): ElementPath | null {
    const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
      elementMap,
      path,
    )?.specialSizeMeasurements
    const parentPath = EP.parentPath(path)
    if (parentPath == null || specialSizeMeasurements == null) {
      return null
    }
    if (specialSizeMeasurements.immediateParentProvidesLayout) {
      return parentPath
    } else {
      return this.findContainingBlock(elementMap, parentPath)
    }
  },
  findNearestAncestorFlexDirectionChange(
    elementMap: ElementInstanceMetadataMap,
    path: ElementPath,
  ): ElementPath | null {
    const parentPath = EP.parentPath(path)
    const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
      elementMap,
      path,
    )?.specialSizeMeasurements
    const parentSizeMeasurements = MetadataUtils.findElementByElementPath(
      elementMap,
      parentPath,
    )?.specialSizeMeasurements
    if (parentPath == null || specialSizeMeasurements == null || parentSizeMeasurements == null) {
      return null
    }
    if (specialSizeMeasurements.flexDirection !== parentSizeMeasurements.flexDirection) {
      return parentPath
    } else {
      return this.findNearestAncestorFlexDirectionChange(elementMap, parentPath)
    }
  },
  isFocusableComponentFromMetadata(element: ElementInstanceMetadata | null): boolean {
    const elementName = MetadataUtils.getJSXElementName(maybeEitherToMaybe(element?.element))
    if (element?.isEmotionOrStyledComponent) {
      return false
    }
    const isAnimatedComponent = isAnimatedElement(element)
    if (isAnimatedComponent) {
      return false
    }
    const isImported = isImportedComponent(element)
    if (isImported) {
      return false
    }
    const isComponent = elementName != null && !isIntrinsicElement(elementName)
    if (isComponent) {
      return true
    } else {
      return false
    }
  },
  isFocusableComponent(path: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return MetadataUtils.isFocusableComponentFromMetadata(element)
  },
  isFocusableLeafComponent(path: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    return (
      MetadataUtils.getChildrenPaths(metadata, path).length === 0 &&
      MetadataUtils.isFocusableComponent(path, metadata)
    )
  },
  isEmotionOrStyledComponent(path: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return element?.isEmotionOrStyledComponent ?? false
  },
  // localFrame is stored in the metadata root, but it is not correct in all cases. Calculating it from specialSizeMeasurements is more reliable
  getLocalFrameFromSpecialSizeMeasurements(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): LocalRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    const globalFrame = element?.globalFrame ?? null
    const elementContainerBounds = element?.specialSizeMeasurements.coordinateSystemBounds ?? null
    const localFrame =
      globalFrame != null && elementContainerBounds != null
        ? canvasRectangleToLocalRectangle(globalFrame, elementContainerBounds)
        : null
    return localFrame
  },
}

export function findElementAtPath(
  target: ElementPath | null,
  components: Array<UtopiaJSXComponent>,
): JSXElementChild | null {
  if (target == null) {
    return null
  } else {
    const staticTarget = EP.dynamicPathToStaticPath(target)
    return findJSXElementChildAtPath(components, staticTarget)
  }
}

export function findJSXElementAtPath(
  target: ElementPath | null,
  components: Array<UtopiaJSXComponent>,
): JSXElement | null {
  const elem = findElementAtPath(target, components)
  return Utils.optionalMap((e) => {
    if (isJSXElement(e)) {
      return e
    } else {
      return null
    }
  }, elem)
}

export function getScenePropsOrElementAttributes(
  target: ElementPath,
  metadata: ElementInstanceMetadataMap,
): PropsOrJSXAttributes | null {
  const targetMetadata = MetadataUtils.findElementByElementPath(metadata, target)
  if (targetMetadata == null) {
    return null
  } else {
    return foldEither(
      () => null,
      (element) => {
        if (isJSXElement(element)) {
          return right(element.props)
        } else {
          return null
        }
      },
      targetMetadata.element,
    )
  }
}

export type PropsOrJSXAttributes = Either<any, JSXAttributes>

export function getSimpleAttributeAtPath(
  propsOrAttributes: PropsOrJSXAttributes,
  path: PropertyPath,
): Either<string, any> {
  return foldEither(
    (props) => {
      const possibleValue = Utils.path(PP.getElements(path), props)
      if (possibleValue == null) {
        return right(undefined)
      } else {
        return right(possibleValue)
      }
    },
    (attributes) => {
      const getAttrResult = getModifiableJSXAttributeAtPath(attributes, path)
      return flatMapEither((attr) => jsxSimpleAttributeToValue(attr), getAttrResult)
    },
    propsOrAttributes,
  )
}
