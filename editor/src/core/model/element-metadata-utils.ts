import * as OPI from 'object-path-immutable'
import { FlexLength, sides, Sides } from 'utopia-api/core'
import { getReorderDirection } from '../../components/canvas/controls/select-mode/yoga-utils'
import { getImageSize, scaleImageDimensions } from '../../components/images'
import Utils from '../../utils/utils'
import { getLayoutProperty } from '../layout/getLayoutProperty'
import {
  mapDropNulls,
  stripNulls,
  flatMapArray,
  uniqBy,
  mapAndFilter,
  allElemsEqual,
} from '../shared/array-utils'
import {
  intrinsicHTMLElementNamesThatSupportChildren,
  TextElements,
  VoidElementsToFilter,
} from '../shared/dom-utils'
import {
  alternativeEither,
  Either,
  eitherToMaybe,
  flatMapEither,
  foldEither,
  forEachRight,
  isRight,
  right,
  maybeEitherToMaybe,
  isLeft,
} from '../shared/either'
import {
  ElementInstanceMetadata,
  ElementsByUID,
  getJSXElementNameLastPart,
  isJSExpressionOtherJavaScript,
  isJSXElement,
  isJSXTextBlock,
  JSXAttributes,
  JSXElement,
  JSXElementChild,
  UtopiaJSXComponent,
  JSXElementName,
  getJSXElementNameAsString,
  isIntrinsicElement,
  ElementInstanceMetadataMap,
  isIntrinsicHTMLElement,
  emptySpecialSizeMeasurements,
  elementInstanceMetadata,
  isImportedOrigin,
  isJSXFragment,
  isJSXConditionalExpression,
  emptyComputedStyle,
  emptyAttributeMetadatada,
  DetectedLayoutSystem,
  JSXConditionalExpression,
  ConditionValue,
  isJSXElementLike,
  JSXElementLike,
} from '../shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attributes'
import {
  boundingRectangleArray,
  CanvasRectangle,
  canvasRectangleToLocalRectangle,
  getLocalRectangleInNewParentContext,
  infinityCanvasRectangle,
  infinityLocalRectangle,
  isInfinityRectangle,
  isFiniteRectangle,
  localRectangle,
  LocalRectangle,
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
  Size,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
  nullIfInfinity,
} from '../shared/math-utils'
import { optionalMap } from '../shared/optional-utils'
import { Imports, PropertyPath, ElementPath, NodeModules } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import {
  componentHonoursPropsPosition,
  componentHonoursPropsSize,
  componentUsesProperty,
  findJSXElementChildAtPath,
  ElementSupportsChildren,
  elementChildSupportsChildrenAlsoText,
} from './element-template-utils'
import {
  isImportedComponent,
  isAnimatedElement,
  isUtopiaAPIComponent,
  isViewLikeFromMetadata,
  isSceneFromMetadata,
  isUtopiaAPIComponentFromMetadata,
  isGivenUtopiaElementFromMetadata,
} from './project-file-utils'
import { fastForEach } from '../shared/utils'
import { objectValues, omit } from '../shared/object-utils'
import { UTOPIA_LABEL_KEY } from './utopia-constants'
import {
  AllElementProps,
  LockedElements,
  withUnderlyingTarget,
} from '../../components/editor/store/editor-state'
import { ProjectContentTreeRoot } from '../../components/assets'
import { memoize } from '../shared/memoize'
import {
  buildTree,
  ElementPathTree,
  ElementPathTreeRoot,
  getSubTree,
  reorderTree,
} from '../shared/element-path-tree'
import { findUnderlyingTargetComponentImplementationFromImportInfo } from '../../components/custom-code/code-file'
import {
  Direction,
  FlexDirection,
  ForwardOrReverse,
  SimpleFlexDirection,
} from '../../components/inspector/common/css-utils'
import { getConditionalClausePath, reorderConditionalChildPathTrees } from './conditionals'
import { getUtopiaID } from '../shared/uid-utils'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  InsertionPath,
  isChildInsertionPath,
} from '../../components/editor/store/insertion-path'
import { getElementContentAffectingType } from '../../components/canvas/canvas-strategies/strategies/group-like-helpers'

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
  isElementGenerated(target: ElementPath): boolean {
    const staticTarget = EP.dynamicPathToStaticPath(target)
    return !EP.pathsEqual(target, staticTarget)
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
  isProbablySceneFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element != null &&
      element.importInfo != null &&
      isImportedOrigin(element.importInfo) &&
      element.importInfo.filePath === 'utopia-api' &&
      element.importInfo.exportedName === 'Scene'
    )
  },
  isProbablyScene(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    return MetadataUtils.isProbablySceneFromMetadata(elementMetadata)
  },
  getIndexInParent(metadata: ElementInstanceMetadataMap, target: ElementPath): number {
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, target)
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
  getSiblingsOrdered(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    if (target == null) {
      return []
    }

    const parentPath = EP.parentPath(target)
    const siblingPathsOrNull = EP.isRootElementOfInstance(target)
      ? MetadataUtils.getRootViewPathsOrdered(metadata, parentPath)
      : MetadataUtils.getChildrenPathsOrdered(metadata, parentPath)
    const siblingPaths = siblingPathsOrNull ?? []
    return MetadataUtils.findElementsByElementPath(metadata, siblingPaths)
  },
  getSiblingsUnordered(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    if (target == null) {
      return []
    }

    const parentPath = EP.parentPath(target)
    const siblingPathsOrNull = EP.isRootElementOfInstance(target)
      ? MetadataUtils.getRootViewPathsUnordered(metadata, parentPath)
      : MetadataUtils.getChildrenPathsUnordered(metadata, parentPath)
    const siblingPaths = siblingPathsOrNull ?? []
    return MetadataUtils.findElementsByElementPath(metadata, siblingPaths)
  },
  getSiblingsParticipatingInAutolayoutUnordered(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    return MetadataUtils.getSiblingsUnordered(metadata, target).filter(
      MetadataUtils.elementParticipatesInAutoLayout,
    )
  },
  getSiblingsParticipatingInAutolayoutOrdered(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    return MetadataUtils.getSiblingsOrdered(metadata, target).filter(
      MetadataUtils.elementParticipatesInAutoLayout,
    )
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
      MetadataUtils.isParentFlexLayoutedContainerForElement(element) &&
      !MetadataUtils.isPositionAbsolute(element)
    )
  },
  isParentFlexLayoutedContainerForElement(element: ElementInstanceMetadata | null): boolean {
    return element?.specialSizeMeasurements.parentLayoutSystem === 'flex'
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
  isPositionFixed(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.position === 'fixed'
  },
  isPositionStatic(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.position === 'static'
  },
  isPositionSticky(instance: ElementInstanceMetadata | null): boolean {
    return (
      instance?.specialSizeMeasurements.position === 'sticky' ||
      instance?.specialSizeMeasurements.position === '-webkit-sticky'
    )
  },
  isPositionRelative(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.position === 'relative'
  },
  isPositionedByFlow(instance: ElementInstanceMetadata | null): boolean {
    if (instance === null) {
      return false
    }

    const containerLayoutSystem = instance.specialSizeMeasurements.parentLayoutSystem
    const position = instance.specialSizeMeasurements.position
    const participatesInFlow =
      position === 'relative' || position === 'static' || position === 'sticky'
    return containerLayoutSystem === 'flow' && participatesInFlow
  },
  elementParticipatesInAutoLayout(element: ElementInstanceMetadata | null): boolean {
    // this contains the ruleset about style properties that make an element autolayouted
    // TODO extend with transform: translate, relative offset etc.
    return !MetadataUtils.isPositionAbsolute(element)
  },
  targetParticipatesInAutoLayout(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): boolean {
    return MetadataUtils.elementParticipatesInAutoLayout(
      MetadataUtils.findElementByElementPath(elements, target),
    )
  },
  getOrderedChildrenParticipatingInAutoLayout(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    return MetadataUtils.getChildrenOrdered(elements, target).filter(
      MetadataUtils.elementParticipatesInAutoLayout,
    )
  },
  hasStaticChildren(elements: ElementInstanceMetadataMap, target: ElementPath): boolean {
    return MetadataUtils.getChildrenUnordered(elements, target).some(
      MetadataUtils.elementParticipatesInAutoLayout,
    )
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
          const attrResult = getSimpleAttributeAtPath(right(elem.props), PP.create('role'))
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
  isTextFromMetadata(element: ElementInstanceMetadata | null): boolean {
    if (element == null) {
      return false
    }
    const isTextElement = foldEither(
      (elementString) => TextElements.includes(elementString),
      (elementInstance) =>
        isJSXElement(elementInstance) && TextElements.includes(elementInstance.name.baseVariable), // TODO this should include a check to make sure the element is a leaf
      element.element,
    )
    {
      return isTextElement
    }
  },
  isGeneratedTextFromMetadata(target: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    if (element == null) {
      return false
    }
    if (isLeft(element.element)) {
      return false
    }
    if (!isJSXElementLike(element.element.value)) {
      return false
    }
    const jsxElement = element.element.value
    // to mark something as text-like, we need to make sure it's a leaf in the metadata graph
    const childrenElementsFromMetadata = MetadataUtils.getChildrenUnordered(metadata, target)
    if (childrenElementsFromMetadata.length !== 0) {
      return false
    }
    return !jsxElement.children.every((c) => isJSXElementLike(c) || isJSXTextBlock(c))
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
  getFlexDirection: function (instance: ElementInstanceMetadata | null): FlexDirection {
    return instance?.specialSizeMeasurements?.flexDirection ?? 'row'
  },
  getSimpleFlexDirection: function (instance: ElementInstanceMetadata | null): SimpleFlexDirection {
    return MetadataUtils.flexDirectionToSimpleFlexDirection(
      MetadataUtils.getFlexDirection(instance),
    )
  },
  flexDirectionToSimpleFlexDirection: function (flexDirection: FlexDirection): {
    direction: Direction
    forwardOrReverse: ForwardOrReverse
  } {
    const direction: Direction = (() => {
      switch (flexDirection) {
        case 'row':
        case 'row-reverse':
          return 'horizontal'
        case 'column':
        case 'column-reverse':
          return 'vertical'
        default:
          return 'horizontal'
      }
    })()

    const forwardOrReverse: ForwardOrReverse = (() => {
      switch (flexDirection) {
        case 'row':
        case 'column':
          return 'forward'
        case 'row-reverse':
        case 'column-reverse':
          return 'reverse'
        default:
          return 'forward'
      }
    })()

    return {
      direction: direction,
      forwardOrReverse: forwardOrReverse,
    }
  },
  getParentFlexGap: function (path: ElementPath, metadata: ElementInstanceMetadataMap): number {
    const instance = MetadataUtils.findElementByElementPath(metadata, path)
    if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
      return instance?.specialSizeMeasurements?.parentFlexGap ?? 0
    } else {
      return 0
    }
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
  getRootViewPathsUnordered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
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
  getRootViewPathsOrdered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
    const possibleRootElementsOfTarget = mapDropNulls((elementPath) => {
      if (EP.isRootElementOf(elementPath, target)) {
        return elementPath
      } else {
        return null
      }
    }, MetadataUtils.createOrderedElementPathsFromElements(elements, [], []).navigatorTargets)
    return possibleRootElementsOfTarget
  },
  getRootViewsUnordered(
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
  getChildrenPathsUnordered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
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
  getChildrenPathsOrdered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
    const possibleChildren = mapDropNulls((elementPath) => {
      if (EP.isChildOf(elementPath, target) && !EP.isRootElementOfInstance(elementPath)) {
        return elementPath
      } else {
        return null
      }
    }, MetadataUtils.createOrderedElementPathsFromElements(elements, [], []).navigatorTargets)
    return possibleChildren
  },
  getChildrenUnordered(
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
  getChildrenOrdered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    return mapDropNulls((elementPath) => {
      if (EP.isChildOf(elementPath, target) && !EP.isRootElementOfInstance(elementPath)) {
        return MetadataUtils.findElementByElementPath(elements, elementPath)
      } else {
        return null
      }
    }, MetadataUtils.createOrderedElementPathsFromElements(elements, [], []).navigatorTargets)
  },
  getDescendantPathsUnordered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
    return mapAndFilter(
      (element) => element.elementPath,
      (path) => EP.isDescendantOf(path, target),
      Object.values(elements),
    )
  },
  getImmediateChildrenPathsUnordered(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
    const element = MetadataUtils.findElementByElementPath(elements, target)
    if (element == null) {
      return []
    } else {
      const rootPaths = MetadataUtils.getRootViewPathsUnordered(elements, target)
      const childrenPaths = MetadataUtils.getChildrenPathsUnordered(elements, target)
      return [...rootPaths, ...childrenPaths]
    }
  },
  getImmediateChildrenUnordered(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const roots = MetadataUtils.getRootViewsUnordered(metadata, target)
    const children = MetadataUtils.getChildrenUnordered(metadata, target)
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
  getAllStoryboardChildrenPathsUnordered(metadata: ElementInstanceMetadataMap): ElementPath[] {
    const storyboardMetadata = MetadataUtils.getStoryboardMetadata(metadata)
    return storyboardMetadata == null
      ? []
      : MetadataUtils.getImmediateChildrenPathsUnordered(metadata, storyboardMetadata.elementPath)
  },
  getAllCanvasSelectablePathsUnordered(metadata: ElementInstanceMetadataMap): ElementPath[] {
    // 1) Get the storyboard children
    const allPaths = objectValues(metadata).map((m) => m.elementPath)
    const storyboardChildren = allPaths.filter(EP.isStoryboardChild)

    // 2) Skip over any Scenes with children at this level
    const withScenesSkipped = flatMapArray((path) => {
      if (MetadataUtils.targetIsScene(metadata, path)) {
        const sceneChildren = MetadataUtils.getChildrenPathsUnordered(metadata, path)
        return sceneChildren.length > 0 ? sceneChildren : [path]
      } else {
        return [path]
      }
    }, storyboardChildren)

    // 3) Replace (focused) component instances at this level with their root paths and children
    const rootPaths = allPaths.filter(EP.isRootElementOfInstance)
    const withComponentInstancesReplaced = flatMapArray((path) => {
      const rootPath = rootPaths.find((rp) => EP.isRootElementOf(rp, path))
      if (rootPath == null) {
        return [path]
      } else {
        const componentChildren = MetadataUtils.getChildrenPathsUnordered(metadata, path)

        // 4) Replace any root paths with their children
        const rootPathChildren = MetadataUtils.getChildrenPathsUnordered(metadata, rootPath)
        const rootPathOrRootChildren = rootPathChildren.length > 0 ? rootPathChildren : [rootPath]
        return [...rootPathOrRootChildren, ...componentChildren]
      }
    }, withScenesSkipped)

    return withComponentInstancesReplaced
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

      const storyboardChildren = MetadataUtils.getAllStoryboardChildrenPathsUnordered(metadata)
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

    const rootInstances = this.getAllStoryboardChildrenPathsUnordered(metadata)

    fastForEach(rootInstances, (rootInstance) => {
      const element = MetadataUtils.findElementByElementPath(metadata, rootInstance)
      if (element != null) {
        result.push(rootInstance)
        const rootElements = MetadataUtils.getRootViewPathsUnordered(metadata, element.elementPath)
        fastForEach(rootElements, (rootPath) => {
          const subTree = getSubTree(projectTree, rootPath)
          recurseElement(subTree)
        })
        const children = MetadataUtils.getChildrenPathsUnordered(metadata, element.elementPath)
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
  isDiv(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'div')
  },
  isSpan(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'span')
  },
  targetIsScene(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
    return elementMetadata != null && isSceneFromMetadata(elementMetadata)
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
  targetElementSupportsChildren(
    projectContents: ProjectContentTreeRoot,
    instance: ElementInstanceMetadata,
  ): boolean {
    return (
      this.targetElementSupportsChildrenAlsoText(projectContents, instance) === 'supportsChildren'
    )
  },
  targetElementSupportsChildrenAlsoText(
    projectContents: ProjectContentTreeRoot,
    instance: ElementInstanceMetadata,
  ): ElementSupportsChildren {
    return foldEither(
      (elementString) => {
        return intrinsicHTMLElementNamesThatSupportChildren.includes(elementString)
          ? 'supportsChildren'
          : 'doesNotSupportChildren'
      },
      (element) => {
        const elementResult = elementChildSupportsChildrenAlsoText(element)
        if (elementResult != null) {
          return elementResult
        } else if (isUtopiaAPIComponentFromMetadata(instance)) {
          // Explicitly prevent components / elements that we *know* don't support children
          return isViewLikeFromMetadata(instance) ||
            isSceneFromMetadata(instance) ||
            EP.isStoryboardPath(instance.elementPath)
            ? 'supportsChildren'
            : 'doesNotSupportChildren'
        } else {
          return MetadataUtils.targetUsesProperty(projectContents, instance, 'children')
            ? 'supportsChildren'
            : 'doesNotSupportChildren'
        }
      },
      instance.element,
    )
  },
  targetSupportsChildren(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadataMap,
    nodeModules: NodeModules,
    openFile: string | null | undefined,
    target: ElementPath | null,
  ): boolean {
    return (
      this.targetSupportsChildrenAlsoText(
        projectContents,
        metadata,
        nodeModules,
        openFile,
        target,
      ) !== 'doesNotSupportChildren'
    )
  },
  targetSupportsChildrenAlsoText(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadataMap,
    nodeModules: NodeModules,
    openFile: string | null | undefined,
    target: ElementPath | null,
  ): ElementSupportsChildren {
    if (target == null) {
      // Assumed to be reparenting to the canvas root.
      return 'supportsChildren'
    } else {
      const instance = MetadataUtils.findElementByElementPath(metadata, target)
      if (instance == null) {
        return withUnderlyingTarget(
          target,
          projectContents,
          nodeModules,
          openFile,
          'doesNotSupportChildren',
          (_, element) => {
            return elementChildSupportsChildrenAlsoText(element) ?? 'doesNotSupportChildren'
          },
        )
      } else {
        return MetadataUtils.targetElementSupportsChildrenAlsoText(projectContents, instance)
      }
    }
  },
  targetUsesProperty(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadata | null,
    property: string,
  ): boolean {
    if (metadata == null) {
      return false
    } else {
      const underlyingComponent = findUnderlyingTargetComponentImplementationFromImportInfo(
        projectContents,
        metadata.importInfo,
      )
      if (underlyingComponent == null) {
        // Could be an external third party component, assuming true for now.
        return true
      } else {
        return componentUsesProperty(underlyingComponent, property)
      }
    }
  },
  targetHonoursPropsSize(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadata | null,
  ): boolean {
    if (metadata == null) {
      return false
    } else {
      const underlyingComponent = findUnderlyingTargetComponentImplementationFromImportInfo(
        projectContents,
        metadata.importInfo,
      )
      if (underlyingComponent == null) {
        // Could be an external third party component, assuming true for now.
        return true
      } else {
        return componentHonoursPropsSize(underlyingComponent)
      }
    }
  },
  targetHonoursPropsPosition(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadata | null,
  ): boolean {
    if (metadata == null) {
      return false
    } else {
      const underlyingComponent = findUnderlyingTargetComponentImplementationFromImportInfo(
        projectContents,
        metadata.importInfo,
      )
      if (underlyingComponent == null) {
        // Could be an external third party component, assuming true for now.
        return true
      } else {
        return componentHonoursPropsPosition(underlyingComponent)
      }
    }
  },
  targetTextEditable(metadata: ElementInstanceMetadataMap, target: ElementPath | null): boolean {
    if (target == null) {
      return false
    }
    const children = MetadataUtils.getChildrenUnordered(metadata, target)
    const hasNonEditableChildren = children
      .map((c) =>
        foldEither(
          () => null,
          (v) => (isJSXElement(v) ? v.name.baseVariable : null),
          c.element,
        ),
      )
      .some((e) => e !== 'br')
    return children.length === 0 || !hasNonEditableChildren
  },
  targetTextEditableAndHasText(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): boolean {
    if (!MetadataUtils.targetTextEditable(metadata, target)) {
      return false
    }

    const element = MetadataUtils.findElementByElementPath(metadata, target)
    if (element == null) {
      return false
    }

    if (isRight(element.element) && isJSXElement(element.element.value)) {
      const elementValue = element.element.value
      return (
        elementValue.children.length >= 1 &&
        elementValue.children.some((c) => isJSXTextBlock(c) || isJSExpressionOtherJavaScript(c))
      )
    }
    return false
  },
  getTextContentOfElement(element: ElementInstanceMetadata): string | null {
    if (isRight(element.element) && isJSXElement(element.element.value)) {
      if (element.element.value.children.length === 1) {
        const childElement = element.element.value.children[0]
        if (isJSXTextBlock(childElement)) {
          return childElement.text
        } else if (isJSExpressionOtherJavaScript(childElement)) {
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
        if (componentFrame != null && isFiniteRectangle(componentFrame)) {
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
  getAllChildrenIncludingUnfurledFocusedComponentsUnordered(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): { children: Array<ElementPath>; unfurledComponents: Array<ElementPath> } {
    return {
      children: MetadataUtils.getChildrenPathsUnordered(metadata, path),
      unfurledComponents: MetadataUtils.getRootViewPathsUnordered(metadata, path),
    }
  },
  getAllChildrenElementsIncludingUnfurledFocusedComponentsUnordered(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): {
    children: Array<ElementInstanceMetadata>
    unfurledComponents: Array<ElementInstanceMetadata>
  } {
    return {
      children: MetadataUtils.getChildrenUnordered(metadata, path),
      unfurledComponents: MetadataUtils.getRootViewsUnordered(metadata, path),
    }
  },
  createOrderedElementPathsFromElements: memoize(
    (
      metadata: ElementInstanceMetadataMap,
      collapsedViews: Array<ElementPath>,
      hiddenInNavigator: Array<ElementPath>,
    ): {
      navigatorTargets: Array<ElementPath>
      visibleNavigatorTargets: Array<ElementPath>
    } => {
      // Note: This will not necessarily be representative of the structured ordering in
      // the code that produced these elements.
      const projectTree = buildTree(objectValues(metadata).map((m) => m.elementPath)).map(
        (subTree) => {
          return reorderTree(subTree, metadata)
        },
      )

      // This function exists separately from getAllPaths because the Navigator handles collapsed views
      let navigatorTargets: Array<ElementPath> = []
      let visibleNavigatorTargets: Array<ElementPath> = []

      function walkAndAddKeys(subTree: ElementPathTree | null, collapsedAncestor: boolean): void {
        if (subTree != null) {
          const path = subTree.path
          const isHiddenInNavigator = EP.containsPath(path, hiddenInNavigator)
          const isConditional = MetadataUtils.isElementPathConditionalFromMetadata(metadata, path)
          navigatorTargets.push(path)
          if (
            !collapsedAncestor &&
            !isHiddenInNavigator &&
            !MetadataUtils.isElementTypeHiddenInNavigator(path, metadata)
          ) {
            visibleNavigatorTargets.push(path)
          }

          const isCollapsed = EP.containsPath(path, collapsedViews)
          const newCollapsedAncestor = collapsedAncestor || isCollapsed || isHiddenInNavigator

          let unfurledComponents: Array<ElementPathTree> = []

          let subTreeChildren: ElementPathTreeRoot = subTree.children
          // For a conditional, we want to ensure that the whenTrue case comes before the whenFalse
          // case for consistent ordering.
          if (isConditional) {
            const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
            if (
              elementMetadata != null &&
              isRight(elementMetadata.element) &&
              isJSXConditionalExpression(elementMetadata.element.value)
            ) {
              const jsxConditionalElement: JSXConditionalExpression = elementMetadata.element.value
              subTreeChildren = reorderConditionalChildPathTrees(
                jsxConditionalElement,
                path,
                subTreeChildren,
              )
            } else {
              throw new Error(
                `Unexpected non-conditional expression retrieved at ${EP.toString(path)}`,
              )
            }
          }

          fastForEach(subTreeChildren, (child) => {
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

      const canvasRoots = MetadataUtils.getAllStoryboardChildrenPathsUnordered(metadata)
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
  isElementTypeHiddenInNavigator(path: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    if (element == null) {
      return false
    } else {
      return foldEither(
        (l) => VoidElementsToFilter.includes(l),
        (r) => (isJSXElement(r) ? VoidElementsToFilter.includes(r.name.baseVariable) : false),
        element.element,
      )
    }
  },
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
  ): MaybeInfinityCanvasRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return Utils.optionalMap((e) => e.globalFrame, element)
  },
  getFrameOrZeroRectInCanvasCoords(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): CanvasRectangle {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    const frame = Utils.optionalMap((e) => e.globalFrame, element)
    return zeroRectIfNullOrInfinity(frame)
  },
  getBoundingRectangleInCanvasCoords(
    paths: Array<ElementPath>,
    metadata: ElementInstanceMetadataMap,
  ): MaybeInfinityCanvasRectangle | null {
    const frames = mapDropNulls(
      (path) => MetadataUtils.getFrameInCanvasCoords(path, metadata),
      paths,
    )
    const nonInfinityFrames = frames.filter(isFiniteRectangle)
    if (frames.length > nonInfinityFrames.length) {
      return infinityCanvasRectangle
    } else {
      return boundingRectangleArray(nonInfinityFrames)
    }
  },
  getFrame(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): MaybeInfinityLocalRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return Utils.optionalMap((e) => e.localFrame, element)
  },
  getFrameOrZeroRect(path: ElementPath, metadata: ElementInstanceMetadataMap): LocalRectangle {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    const frame = Utils.optionalMap((e) => e.localFrame, element)
    return zeroRectIfNullOrInfinity(frame)
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
      const parentFrames: Array<MaybeInfinityLocalRectangle> = Utils.stripNulls(
        paths.map((path) => this.getFrame(path, metadata)),
      )
      return parentFrames.reduce<LocalRectangle>((working, next) => {
        if (isInfinityRectangle(next)) {
          return working
        }

        return Utils.offsetRect(working, {
          x: -next.x,
          y: -next.y,
        } as LocalRectangle)
      }, Utils.asLocal(frame))
    }
  },
  getGlobalContentBoxForChildren: function (
    parent: ElementInstanceMetadata,
  ): CanvasRectangle | null {
    if (
      parent.specialSizeMeasurements.globalContentBoxForChildren != null &&
      isFiniteRectangle(parent.specialSizeMeasurements.globalContentBoxForChildren)
    ) {
      return parent.specialSizeMeasurements.globalContentBoxForChildren
    }

    if (EP.isStoryboardPath(parent.elementPath)) {
      return zeroCanvasRect
    }

    return null
  },
  getFrameRelativeToTargetContainingBlock: function (
    targetParent: ElementPath,
    metadata: ElementInstanceMetadataMap,
    frame: CanvasRectangle,
  ): LocalRectangle | null {
    const targetParentInstance = MetadataUtils.findElementByElementPath(metadata, targetParent)
    if (targetParentInstance == null) {
      return null
    }

    const globalContentBox = MetadataUtils.getGlobalContentBoxForChildren(targetParentInstance)
    if (globalContentBox == null) {
      return null
    }
    return canvasRectangleToLocalRectangle(frame, globalContentBox)
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
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
    element: ElementInstanceMetadata,
    staticName: JSXElementName | null = null,
  ): string {
    const elementContentAffectingType = getElementContentAffectingType(
      metadata,
      allElementProps,
      element.elementPath,
    )

    const isElementGroup =
      elementContentAffectingType != null &&
      elementContentAffectingType !== 'fragment' &&
      elementContentAffectingType !== 'conditional'

    const sceneLabel = element.label // KILLME?
    const dataLabelProp = MetadataUtils.getElementLabelFromProps(
      allElementProps,
      element.elementPath,
    )
    if (dataLabelProp != null) {
      return dataLabelProp
    } else if (sceneLabel != null) {
      return sceneLabel
    } else if (isElementGroup) {
      return 'Group'
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
                if (element.textContent != null) {
                  return element.textContent
                }

                const firstChild = jsxElement.children[0]
                if (firstChild != null) {
                  if (isJSXTextBlock(firstChild)) {
                    return firstChild.text
                  }
                  if (isJSExpressionOtherJavaScript(firstChild)) {
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
                  if (src.startsWith('data:') && src.includes('base64')) {
                    return '<Base64 data>'
                  }
                  return src
                }
              }

              return lastNamePart
            case 'JSX_TEXT_BLOCK':
              return '(text)'
            case 'ATTRIBUTE_OTHER_JAVASCRIPT':
              return '(code)'
            case 'JSX_FRAGMENT':
              return 'Fragment'
            case 'JSX_CONDITIONAL_EXPRESSION':
              return 'Conditional'
            case 'ATTRIBUTE_VALUE':
              return `${jsxElement.value}`
            case 'ATTRIBUTE_NESTED_ARRAY':
              return '(code)'
            case 'ATTRIBUTE_NESTED_OBJECT':
              return '(code)'
            case 'ATTRIBUTE_FUNCTION_CALL':
              return '(code)'
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
      return MetadataUtils.getElementLabelFromMetadata(
        metadata,
        allElementProps,
        element,
        staticName,
      )
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
    projectContents: ProjectContentTreeRoot,
    nodeModules: NodeModules,
    openFile: string | null | undefined,
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

    const spyOnlyElements = fillSpyOnlyMetadata(
      fromSpy,
      fromDOM,
      projectContents,
      nodeModules,
      openFile,
    )

    workingElements = {
      ...workingElements,
      ...spyOnlyElements,
    }

    const elementsInheritingFromAncestors = fillMissingDataFromAncestors(workingElements)
    return {
      ...workingElements,
      ...elementsInheritingFromAncestors,
    }
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
    const isAnimatedComponent = isAnimatedElement(element)
    if (isAnimatedComponent) {
      return false
    }
    const isImported = isImportedComponent(element)
    if (isImported) {
      return false
    }
    if (element?.isEmotionOrStyledComponent) {
      return false
    }
    const elementName = MetadataUtils.getJSXElementName(maybeEitherToMaybe(element?.element))
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
      MetadataUtils.getChildrenPathsUnordered(metadata, path).length === 0 &&
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
      globalFrame != null && isFiniteRectangle(globalFrame) && elementContainerBounds != null
        ? canvasRectangleToLocalRectangle(globalFrame, elementContainerBounds)
        : null
    return localFrame
  },
  isDescendantOfHierarchyLockedElement(path: ElementPath, lockedElements: LockedElements): boolean {
    return lockedElements.hierarchyLock.some((lockedPath) => EP.isDescendantOf(path, lockedPath))
  },
  collectParentsAndSiblings(
    componentMetadata: ElementInstanceMetadataMap,
    targets: Array<ElementPath>,
  ): Array<ElementPath> {
    const allPaths = MetadataUtils.getAllPaths(componentMetadata)
    const result: Array<ElementPath> = []
    Utils.fastForEach(targets, (target) => {
      const parent = EP.parentPath(target)
      Utils.fastForEach(allPaths, (maybeTarget) => {
        const isSibling = EP.isSiblingOf(maybeTarget, target)
        const isParent = EP.pathsEqual(parent, maybeTarget)
        const notSelectedOrDescendantOfSelected = targets.every(
          (view) => !EP.isDescendantOfOrEqualTo(maybeTarget, view),
        )
        if ((isSibling || isParent) && notSelectedOrDescendantOfSelected) {
          result.push(maybeTarget)
        }
      })
    })

    return result
  },
  isElementPathFragmentFromMetadata(
    componentMetadata: ElementInstanceMetadataMap,
    elementPath: ElementPath | null,
  ): boolean {
    const element = MetadataUtils.findElementByElementPath(componentMetadata, elementPath)

    return MetadataUtils.isFragmentFromMetadata(element)
  },
  isElementPathConditionalFromMetadata(
    componentMetadata: ElementInstanceMetadataMap,
    elementPath: ElementPath | null,
  ): boolean {
    const element = MetadataUtils.findElementByElementPath(componentMetadata, elementPath)

    return MetadataUtils.isConditionalFromMetadata(element)
  },
  isElementPathConditionalClauseFromMetadata(
    componentMetadata: ElementInstanceMetadataMap,
    elementPath: ElementPath | null,
  ): boolean {
    if (elementPath == null) {
      return false
    } else {
      const parentPath = EP.parentPath(elementPath)
      const parentMetadata = MetadataUtils.findElementByElementPath(componentMetadata, parentPath)

      return MetadataUtils.isConditionalFromMetadata(parentMetadata)
    }
  },
  isFragmentFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element?.element != null && isRight(element.element) && isJSXFragment(element.element.value)
    )
  },
  isConditionalFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element?.element != null &&
      isRight(element.element) &&
      isJSXConditionalExpression(element.element.value)
    )
  },
  resolveReparentTargetParentToPath(
    metadata: ElementInstanceMetadataMap,
    reparentTargetParent: InsertionPath,
  ): ElementPath {
    if (isChildInsertionPath(reparentTargetParent)) {
      // This is an element path, so return directly.
      return reparentTargetParent.intendedParentPath
    } else {
      // Resolve this to the element in the clause.
      const targetElement = this.findElementByElementPath(
        metadata,
        reparentTargetParent.intendedParentPath,
      )
      if (targetElement == null) {
        throw new Error(
          `Did not find a conditional at ${EP.toString(reparentTargetParent.intendedParentPath)}.`,
        )
      } else {
        return foldEither(
          () => {
            throw new Error(
              `Did not find a conditional at ${EP.toString(
                reparentTargetParent.intendedParentPath,
              )}.`,
            )
          },
          (element) => {
            if (isJSXConditionalExpression(element)) {
              return getConditionalClausePath(
                reparentTargetParent.intendedParentPath,
                reparentTargetParent.clause === 'true-case' ? element.whenTrue : element.whenFalse,
              )
            } else {
              throw new Error(
                `Found a ${element.type} at ${EP.toString(
                  reparentTargetParent.intendedParentPath,
                )} instead of a conditional.`,
              )
            }
          },
          targetElement.element,
        )
      }
    }
  },
  getConditionValueFromMetadata(element: ElementInstanceMetadata | null): ConditionValue {
    if (!this.isConditionalFromMetadata(element)) {
      return 'not-a-conditional'
    }
    return element?.conditionValue ?? 'not-a-conditional'
  },
  findLayoutSystemForChildren(
    metadata: ElementInstanceMetadataMap,
    parentPath: ElementPath,
  ): DetectedLayoutSystem {
    const childrenPaths = MetadataUtils.getChildrenPathsUnordered(metadata, parentPath)
    const children = mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      childrenPaths,
    )

    const fallbackLayout =
      MetadataUtils.findElementByElementPath(metadata, parentPath)?.specialSizeMeasurements
        .layoutSystemForChildren ?? 'none'

    const parentLayouts = children.map((c) => c.specialSizeMeasurements.parentLayoutSystem)

    if (parentLayouts.length === 0) {
      return fallbackLayout
    }
    const allEqual = parentLayouts.slice(1).every((x) => x === parentLayouts[0])
    if (!allEqual) {
      return fallbackLayout
    }
    return parentLayouts[0]
  },
  findFlexDirectionForChildren(
    metadata: ElementInstanceMetadataMap,
    parentPath: ElementPath,
  ): FlexDirection | null {
    const childrenPaths = MetadataUtils.getChildrenPathsUnordered(metadata, parentPath)

    const fallbackFlexDirection =
      MetadataUtils.findElementByElementPath(metadata, parentPath)?.specialSizeMeasurements
        .flexDirection ?? null

    const children = mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      childrenPaths,
    )
    const flexDirections = children.map((c) => c.specialSizeMeasurements.parentFlexDirection)
    if (flexDirections.length === 0) {
      return fallbackFlexDirection
    }
    const allEqual = flexDirections.slice(1).every((x) => x === flexDirections[0])
    if (!allEqual) {
      return fallbackFlexDirection
    }
    return flexDirections[0]
  },
  getReparentTargetOfTarget(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): InsertionPath | null {
    const parentElement = this.getParent(metadata, target)
    if (parentElement == null) {
      return null
    } else {
      if (
        isRight(parentElement.element) &&
        isJSXConditionalExpression(parentElement.element.value)
      ) {
        const conditionalExpression: JSXConditionalExpression = parentElement.element.value
        if (getUtopiaID(conditionalExpression.whenTrue) === EP.toUid(target)) {
          return conditionalClauseInsertionPath(parentElement.elementPath, 'true-case')
        } else if (getUtopiaID(conditionalExpression.whenFalse) === EP.toUid(target)) {
          return conditionalClauseInsertionPath(parentElement.elementPath, 'false-case')
        }
      }
      return childInsertionPath(parentElement.elementPath)
    }
  },
}

function fillSpyOnlyMetadata(
  fromSpy: ElementInstanceMetadataMap,
  fromDOM: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
): ElementInstanceMetadataMap {
  const childrenInDomCache: { [pathStr: string]: Array<ElementInstanceMetadata> } = {}

  const conditionalsWithDefaultMetadata = findConditionalsAndCreateMetadata(
    Array.from(new Set([...Object.keys(fromDOM), ...Object.keys(fromSpy)])),
    projectContents,
    nodeModules,
    openFile,
  )

  const findChildrenInDomRecursively = (pathStr: string): Array<ElementInstanceMetadata> => {
    const existing = childrenInDomCache[pathStr]

    if (existing != null) {
      return existing
    }

    const fromSpyAndConditionals = {
      ...conditionalsWithDefaultMetadata,
      ...fromSpy,
    }
    const spyElem = fromSpyAndConditionals[pathStr]

    const { children: childrenFromSpy, unfurledComponents: unfurledComponentsFromSpy } =
      MetadataUtils.getAllChildrenElementsIncludingUnfurledFocusedComponentsUnordered(
        spyElem.elementPath,
        fromSpyAndConditionals,
      )
    const childrenAndUnfurledComponentsFromSpy = [...childrenFromSpy, ...unfurledComponentsFromSpy]

    const { children: childrenFromDom, unfurledComponents: unfurledComponentsFromDom } =
      MetadataUtils.getAllChildrenElementsIncludingUnfurledFocusedComponentsUnordered(
        spyElem.elementPath,
        fromDOM,
      )
    const childrenAndUnfurledComponentsFromDom = [...childrenFromDom, ...unfurledComponentsFromDom]

    const childrenAndUnfurledComponentsNotInDom = childrenAndUnfurledComponentsFromSpy.filter(
      (childNotInDom) =>
        childrenAndUnfurledComponentsFromDom.every(
          (childInDom) => !EP.pathsEqual(childNotInDom.elementPath, childInDom.elementPath),
        ),
    )

    const recursiveChildrenAndUnfurledComponents = childrenAndUnfurledComponentsNotInDom.flatMap(
      (c) => {
        return findChildrenInDomRecursively(EP.toString(c.elementPath))
      },
    )

    const childrenAndUnfurledComponents = [
      ...childrenAndUnfurledComponentsFromDom,
      ...childrenAndUnfurledComponentsNotInDom,
      ...recursiveChildrenAndUnfurledComponents,
    ]

    childrenInDomCache[pathStr] = childrenAndUnfurledComponents

    return childrenAndUnfurledComponents
  }

  const spyElementsWithoutDomMetadata = Object.keys(fromSpy).filter((p) => fromDOM[p] == null)

  const elementsWithoutIntrinsicSize = Object.keys(fromSpy).filter((p) => {
    const globalFrame = fromDOM[p]?.globalFrame
    if (globalFrame == null) {
      return true
    }
    if (isInfinityRectangle(globalFrame)) {
      return false
    }
    return globalFrame.width === 0 || globalFrame.height === 0
  })

  const elementsWithoutDomMetadata = Array.from([
    ...spyElementsWithoutDomMetadata,
    ...Object.keys(conditionalsWithDefaultMetadata),
  ])

  const elementsWithoutParentData = Object.keys(fromSpy).filter((p) => {
    const parentLayoutSystem = fromDOM[p]?.specialSizeMeasurements.parentLayoutSystem
    return parentLayoutSystem == null
  })

  // Sort and then reverse these, so that lower level elements (with longer paths) are handled ahead of their parents
  // Sort and then reverse these, so that lower level elements (with longer paths) are handled ahead of their parents
  // and ancestors. This means that if there are a grandparent and parent which both lack global frames
  // then the parent is fixed ahead of the grandparent, which will be based on the parent.
  elementsWithoutIntrinsicSize.sort()
  elementsWithoutIntrinsicSize.reverse()
  elementsWithoutDomMetadata.sort()
  elementsWithoutDomMetadata.reverse()
  elementsWithoutParentData.sort()
  elementsWithoutParentData.reverse()

  const workingElements: ElementInstanceMetadataMap = {}

  fastForEach([...elementsWithoutDomMetadata, ...elementsWithoutIntrinsicSize], (pathStr) => {
    const spyElem = fromSpy[pathStr] ?? conditionalsWithDefaultMetadata[pathStr]

    const children = findChildrenInDomRecursively(pathStr)
    if (children.length === 0) {
      return
    }

    const childrenFromWorking = children.map((child) => {
      const childPathStr = EP.toString(child.elementPath)
      const fromWorkingElements = workingElements[childPathStr]
      if (fromWorkingElements == null) {
        return child
      } else {
        return fromWorkingElements
      }
    })

    const childrenGlobalFrames = mapDropNulls((c) => c.globalFrame, childrenFromWorking)
    const childrenNonInfinityGlobalFrames = childrenGlobalFrames.filter(isFiniteRectangle)
    const childrenBoundingGlobalFrame =
      childrenNonInfinityGlobalFrames.length === childrenGlobalFrames.length
        ? boundingRectangleArray(childrenNonInfinityGlobalFrames)
        : infinityCanvasRectangle

    const childrenLocalFrames = mapDropNulls((c) => c.localFrame, childrenFromWorking)
    const childrenNonInfinityLocalFrames = childrenLocalFrames.filter(isFiniteRectangle)
    const childrenBoundingLocalFrame =
      childrenNonInfinityLocalFrames.length === childrenLocalFrames.length
        ? boundingRectangleArray(childrenNonInfinityLocalFrames)
        : infinityLocalRectangle

    workingElements[pathStr] = {
      ...spyElem,
      globalFrame: childrenBoundingGlobalFrame,
      localFrame: childrenBoundingLocalFrame,
    }
  })

  fastForEach(elementsWithoutParentData, (pathStr) => {
    const spyElem = fromSpy[pathStr]
    const sameThingFromWorkingElems = workingElements[pathStr]
    const children = findChildrenInDomRecursively(pathStr)
    if (children.length === 0) {
      return
    }

    const childrenFromWorking = children.map((child) => {
      const childPathStr = EP.toString(child.elementPath)
      const fromWorkingElements = workingElements[childPathStr]
      if (fromWorkingElements == null) {
        return child
      } else {
        return fromWorkingElements
      }
    })

    const parentLayoutSystemFromChildren = childrenFromWorking.map(
      (c) => c.specialSizeMeasurements.parentLayoutSystem,
    )
    const parentFlexDirectionFromChildren = childrenFromWorking.map(
      (c) => c.specialSizeMeasurements.parentFlexDirection,
    )
    const immediateParentBoundsFromChildren = childrenFromWorking.map(
      (c) => c.specialSizeMeasurements.immediateParentBounds,
    )
    const positionForChildren = childrenFromWorking.map((c) => c.specialSizeMeasurements.position)

    workingElements[pathStr] = {
      ...spyElem,
      ...sameThingFromWorkingElems,
      specialSizeMeasurements: {
        ...spyElem.specialSizeMeasurements,
        parentLayoutSystem: allElemsEqual(parentLayoutSystemFromChildren)
          ? parentLayoutSystemFromChildren[0]
          : spyElem.specialSizeMeasurements.parentLayoutSystem,
        parentFlexDirection: allElemsEqual(parentFlexDirectionFromChildren)
          ? parentFlexDirectionFromChildren[0]
          : spyElem.specialSizeMeasurements.parentFlexDirection,
        immediateParentBounds: allElemsEqual(immediateParentBoundsFromChildren)
          ? immediateParentBoundsFromChildren[0]
          : spyElem.specialSizeMeasurements.immediateParentBounds,
        position: allElemsEqual(positionForChildren)
          ? positionForChildren[0]
          : spyElem.specialSizeMeasurements.position,
      },
    }
  })

  return workingElements
}

function fillMissingDataFromAncestors(mergedMetadata: ElementInstanceMetadataMap) {
  let workingElements: ElementInstanceMetadataMap = { ...mergedMetadata }

  const elementsWithoutGlobalContentBox = Object.keys(workingElements).filter((p) => {
    return workingElements[p]?.specialSizeMeasurements.globalContentBoxForChildren == null
  })
  // sorted, so that parents are fixed first
  elementsWithoutGlobalContentBox.sort()

  fastForEach(elementsWithoutGlobalContentBox, (pathStr) => {
    const elem = workingElements[pathStr]

    const parentPathStr = EP.toString(EP.parentPath(EP.fromString(pathStr)))

    const parentGlobalContentBoxForChildren =
      workingElements[parentPathStr]?.specialSizeMeasurements.globalContentBoxForChildren ??
      infinityCanvasRectangle

    workingElements[pathStr] = {
      ...elem,
      specialSizeMeasurements: {
        ...elem.specialSizeMeasurements,
        globalContentBoxForChildren: parentGlobalContentBoxForChildren,
      },
    }
  })

  return workingElements
}

function findConditionalsAndCreateMetadata(
  paths: Array<string>,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
): ElementInstanceMetadataMap {
  const allAncestors = paths
    .flatMap((p) => EP.getAncestors(EP.fromString(p)))
    .filter((p) => p.parts.length > 0)
    .map(EP.toString)

  const missingAncestors = allAncestors.filter((a) => paths.indexOf(a) == -1)

  let workingConditionals: ElementInstanceMetadataMap = {}
  missingAncestors.forEach((ancestor: string) => {
    const path = EP.fromString(ancestor)
    return withUnderlyingTarget(
      path,
      projectContents,
      nodeModules,
      openFile,
      null,
      (_, element) => {
        if (isJSXConditionalExpression(element)) {
          // create a default metadata, we can finetune this if necessary
          workingConditionals[ancestor] = elementInstanceMetadata(
            path,
            right(element),
            null,
            null,
            false,
            false,
            emptySpecialSizeMeasurements,
            emptyComputedStyle,
            emptyAttributeMetadatada,
            'Conditional',
            null,
            'not-a-conditional',
          )
        }
      },
    )
  })

  return workingConditionals
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

export function findJSXElementLikeAtPath(
  target: ElementPath | null,
  components: Array<UtopiaJSXComponent>,
): JSXElementLike | null {
  const elem = findElementAtPath(target, components)
  return Utils.optionalMap((e) => {
    if (isJSXElementLike(e)) {
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

// This function creates a fake metadata for the given element
// Useful when metadata is needed before the real on is created.
export function createFakeMetadataForElement(
  path: ElementPath,
  element: JSXElementChild,
  frame: CanvasRectangle,
  metadata: ElementInstanceMetadataMap,
): ElementInstanceMetadata {
  const parentPath = EP.parentPath(path)

  const parentElement = MetadataUtils.findElementByElementPath(metadata, parentPath)

  const isFlex = parentElement != null && MetadataUtils.isFlexLayoutedContainer(parentElement)
  const parentBounds = parentElement != null ? parentElement.globalFrame : null

  const localFrame =
    parentBounds == null || isInfinityRectangle(parentBounds)
      ? localRectangle(frame)
      : getLocalRectangleInNewParentContext(parentBounds, frame)

  const specialSizeMeasurements = { ...emptySpecialSizeMeasurements }
  specialSizeMeasurements.position = isFlex ? 'relative' : 'absolute'
  specialSizeMeasurements.parentLayoutSystem = isFlex ? 'flex' : 'none'
  specialSizeMeasurements.parentFlexDirection =
    parentElement?.specialSizeMeasurements.flexDirection ?? 'row'
  specialSizeMeasurements.immediateParentBounds = nullIfInfinity(parentBounds)
  specialSizeMeasurements.parentPadding =
    parentElement?.specialSizeMeasurements.padding ??
    sides(undefined, undefined, undefined, undefined)
  specialSizeMeasurements.parentFlexGap = parentElement?.specialSizeMeasurements.gap ?? 0

  return elementInstanceMetadata(
    path,
    right(element),
    frame,
    localFrame,
    false,
    false,
    specialSizeMeasurements,
    null,
    null,
    null,
    null,
    'not-a-conditional',
  )
}
