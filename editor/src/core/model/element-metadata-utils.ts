import * as OPI from 'object-path-immutable'
import type { Emphasis, FlexLength, Icon, Sides } from 'utopia-api/core'
import { sides } from 'utopia-api/core'
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
  pluck,
} from '../shared/array-utils'
import {
  intrinsicHTMLElementNamesThatSupportChildren,
  PossibleTextElements,
  TextElements,
  VoidElementsToFilter,
} from '../shared/dom-utils'
import type { Either } from '../shared/either'
import {
  alternativeEither,
  eitherToMaybe,
  flatMapEither,
  foldEither,
  forEachRight,
  isRight,
  right,
  maybeEitherToMaybe,
  isLeft,
} from '../shared/either'
import type {
  ElementInstanceMetadata,
  ElementsByUID,
  JSXAttributes,
  JSXElement,
  JSXElementChild,
  UtopiaJSXComponent,
  JSXElementName,
  ElementInstanceMetadataMap,
  DetectedLayoutSystem,
  JSXConditionalExpression,
  ConditionValue,
  JSXElementLike,
  JSPropertyAccess,
  SpecialSizeMeasurements,
} from '../shared/element-template'
import {
  getJSXElementNameLastPart,
  isJSExpressionMapOrOtherJavaScript,
  isJSXElement,
  isJSXTextBlock,
  getJSXElementNameAsString,
  isIntrinsicElement,
  isIntrinsicHTMLElement,
  emptySpecialSizeMeasurements,
  elementInstanceMetadata,
  isImportedOrigin,
  isJSXFragment,
  isJSXConditionalExpression,
  emptyComputedStyle,
  emptyAttributeMetadata,
  isJSXElementLike,
  isJSExpression,
  hasElementsWithin,
  isJSExpressionOtherJavaScript,
  isJSXMapExpression,
  getJSXAttribute,
  isJSXAttributeValue,
  isJSIdentifier,
  isJSPropertyAccess,
  isJSElementAccess,
} from '../shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attribute-utils'
import type {
  CanvasRectangle,
  LocalRectangle,
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
  Size,
  Rectangle,
  InfinityRectangle,
  CoordinateMarker,
} from '../shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangleToLocalRectangle,
  getLocalRectangleInNewParentContext,
  infinityCanvasRectangle,
  isInfinityRectangle,
  isFiniteRectangle,
  localRectangle,
  zeroCanvasRect,
  zeroRectIfNullOrInfinity,
  nullIfInfinity,
  infinityRectangle,
  infinityLocalRectangle,
} from '../shared/math-utils'
import { optionalMap } from '../shared/optional-utils'
import type { Imports, PropertyPath, ElementPath, NodeModules } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import type { ElementSupportsChildren, HonoursPosition } from './element-template-utils'
import {
  componentHonoursPropsPosition,
  componentHonoursPropsSize,
  componentUsesProperty,
  findJSXElementChildAtPath,
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
  type FilePathMappings,
} from './project-file-utils'
import { assertNever, fastForEach } from '../shared/utils'
import { mapValues, objectMap, objectValues, omit } from '../shared/object-utils'
import { UTOPIA_LABEL_KEY } from './utopia-constants'
import type {
  AllElementProps,
  LockedElements,
  NavigatorEntry,
} from '../../components/editor/store/editor-state'
import {
  isRegularNavigatorEntry,
  withUnderlyingTarget,
} from '../../components/editor/store/editor-state'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { memoize } from '../shared/memoize'
import type { ElementPathTree, ElementPathTrees } from '../shared/element-path-tree'
import {
  buildTree,
  getSubTree,
  getCanvasRoots,
  elementPathTree,
  getElementPathTreeChildren,
  forEachElementPathTreeChild,
  printTree,
} from '../shared/element-path-tree'
import type { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { findUnderlyingTargetComponentImplementationFromImportInfo } from '../../components/custom-code/code-file'
import type {
  Direction,
  FlexDirection,
  ForwardOrReverse,
  SimpleFlexDirection,
} from '../../components/inspector/common/css-utils'
import {
  findFirstNonConditionalAncestor,
  getConditionalActiveCase,
  getConditionalClausePath,
  isTextEditableConditional,
  maybeConditionalActiveBranch,
  maybeConditionalExpression,
  reorderConditionalChildPathTrees,
} from './conditionals'
import { getUtopiaID } from '../shared/uid-utils'
import type { InsertionPath } from '../../components/editor/store/insertion-path'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  isChildInsertionPath,
  replaceWithSingleElement,
} from '../../components/editor/store/insertion-path'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { treatElementAsGroupLikeFromMetadata } from '../../components/canvas/canvas-strategies/strategies/group-helpers'
import type { RemixRoutingTable } from '../../components/editor/store/remix-derived-data'
import { exists, toFirst } from '../shared/optics/optic-utilities'
import { eitherRight, fromField, fromTypeGuard, notNull } from '../shared/optics/optic-creators'
import { getComponentDescriptorForTarget } from '../property-controls/property-controls-utils'
import { treatElementAsFragmentLike } from '../../components/canvas/canvas-strategies/strategies/fragment-like-helpers'

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

// eslint-disable-next-line object-shorthand
export const MetadataUtils = {
  isElementDirectlyGenerated(target: ElementPath): boolean {
    const staticTarget = EP.dynamicPathToStaticPath(target)
    const parentPath = EP.parentPath(target)
    const parentStaticPath = EP.dynamicPathToStaticPath(parentPath)
    return EP.pathsEqual(parentPath, parentStaticPath) && !EP.pathsEqual(target, staticTarget)
  },
  isElementOrAncestorGenerated(target: ElementPath): boolean {
    const staticTarget = EP.dynamicPathToStaticPath(target)
    return !EP.pathsEqual(target, staticTarget)
  },
  findElementByElementPath(
    elementMap: ElementInstanceMetadataMap,
    path: ElementPath | null | undefined,
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
  isImportedComponentFromMetadata(
    element: ElementInstanceMetadata | null,
    importedFrom: string,
    componentName: string | null,
  ): boolean {
    return (
      element != null &&
      element.importInfo != null &&
      isImportedOrigin(element.importInfo) &&
      element.importInfo.filePath === importedFrom &&
      (componentName == null || element.importInfo.exportedName === componentName)
    )
  },
  isProbablySceneFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(element, 'utopia-api', 'Scene')
  },
  isProbablyRemixSceneFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(element, 'utopia-api', 'RemixScene')
  },
  isProbablyRemixOutletFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(element, '@remix-run/react', 'Outlet')
  },
  isProbablyRemixLinkFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(element, '@remix-run/react', 'Link')
  },
  isReactComponentFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(element, 'react', null)
  },
  isImportedComponent(
    jsxMetadata: ElementInstanceMetadataMap,
    path: ElementPath,
    importedFrom: string,
    componentName: string,
  ): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    return MetadataUtils.isImportedComponentFromMetadata(
      elementMetadata,
      componentName,
      importedFrom,
    )
  },
  isProbablyScene(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    return MetadataUtils.isProbablySceneFromMetadata(elementMetadata)
  },
  isProbablyRemixScene(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    return MetadataUtils.isProbablyRemixSceneFromMetadata(elementMetadata)
  },
  isProbablyRemixOutlet(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    return MetadataUtils.isProbablyRemixOutletFromMetadata(elementMetadata)
  },
  isSceneWithOneChild(
    jsxMetadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    path: ElementPath,
  ): boolean {
    return (
      MetadataUtils.isProbablyScene(jsxMetadata, path) &&
      MetadataUtils.getChildrenPathsOrdered(pathTree, path).length === 1
    )
  },
  isContainingComponentRemixSceneOrOutlet(
    jsxMetadata: ElementInstanceMetadataMap,
    path: ElementPath,
  ): boolean {
    const parentComponent = EP.getContainingComponent(path)
    return (
      MetadataUtils.isProbablyRemixOutlet(jsxMetadata, parentComponent) ||
      MetadataUtils.isProbablyRemixScene(jsxMetadata, parentComponent)
    )
  },
  parentIsSceneWithOneChild(
    jsxMetadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    path: ElementPath,
  ): boolean {
    return MetadataUtils.isSceneWithOneChild(jsxMetadata, pathTree, EP.parentPath(path))
  },
  getIndexInParent(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): number {
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, pathTree, target)
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
    pathTree: ElementPathTrees,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    if (target == null) {
      return []
    }
    const parentPath = EP.parentPath(target)
    const siblingPathsOrNull = EP.isRootElementOfInstance(target)
      ? MetadataUtils.getRootViewPathsOrdered(metadata, pathTree, parentPath)
      : MetadataUtils.getChildrenPathsOrdered(pathTree, parentPath)
    const siblingPaths = siblingPathsOrNull ?? []
    return MetadataUtils.findElementsByElementPath(metadata, siblingPaths)
  },
  getSiblingsParticipatingInAutolayoutOrdered(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath | null,
  ): ElementInstanceMetadata[] {
    return MetadataUtils.getSiblingsOrdered(metadata, pathTree, target).filter(
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
  isGridCell(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const parent = MetadataUtils.findElementByElementPath(metadata, EP.parentPath(path))
    return (
      parent != null &&
      isRight(parent.element) &&
      isJSXElement(parent.element.value) &&
      parent.element.value.children.length > 0 &&
      parent.specialSizeMeasurements.containerGridProperties.gridTemplateColumns != null &&
      parent.specialSizeMeasurements.containerGridProperties.gridTemplateRows != null &&
      MetadataUtils.isGridLayoutedContainer(parent)
    )
  },
  isGridCellWithPositioning(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return (
      MetadataUtils.isGridCell(metadata, path) &&
      element != null &&
      !MetadataUtils.hasNoGridCellPositioning(element.specialSizeMeasurements)
    )
  },
  hasNoGridCellPositioning(specialSizeMeasurements: SpecialSizeMeasurements): boolean {
    return (
      specialSizeMeasurements.elementGridPropertiesFromProps.gridColumnStart == null &&
      specialSizeMeasurements.elementGridPropertiesFromProps.gridColumnEnd == null &&
      specialSizeMeasurements.elementGridPropertiesFromProps.gridRowStart == null &&
      specialSizeMeasurements.elementGridPropertiesFromProps.gridRowEnd == null
    )
  },
  isComponentInstanceFromMetadata(
    metadata: ElementInstanceMetadataMap,
    path: ElementPath,
  ): boolean {
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
    return elementMetadata?.componentInstance ?? false
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
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    return MetadataUtils.getChildrenOrdered(elements, pathTree, target).filter(
      MetadataUtils.elementParticipatesInAutoLayout,
    )
  },
  hasStaticChildren(
    elements: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): boolean {
    return MetadataUtils.getChildrenOrdered(elements, pathTree, target).some(
      MetadataUtils.elementParticipatesInAutoLayout,
    )
  },
  isButtonFromMetadata(element: ElementInstanceMetadata | null): boolean {
    if (element == null) {
      return false
    } else {
      if (element.specialSizeMeasurements.htmlElementName.toLowerCase() === 'button') {
        return true
      }

      if (isRight(element.element)) {
        const elem = element.element.value
        const elementName = MetadataUtils.getJSXElementName(elem)
        if (
          elementName != null &&
          PP.depth(elementName.propertyPath) === 0 &&
          elementName.baseVariable === 'button'
        ) {
          return true
        }

        let buttonRoleFound: boolean = false
        if (isJSXElement(elem)) {
          const attrResult = getSimpleAttributeAtPath(right(elem.props), PP.create('role'))
          forEachRight(attrResult, (value) => {
            if (value === 'button') {
              buttonRoleFound = true
            }
          })
        }
        if (buttonRoleFound) {
          return true
        }
      }
      return false
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
  isGeneratedTextFromMetadata(
    target: ElementPath,
    pathTree: ElementPathTrees,
    metadata: ElementInstanceMetadataMap,
  ): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    if (element == null || element.textContent == null || element.textContent.length === 0) {
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
    const childrenElementsFromMetadata = MetadataUtils.getNonExpressionDescendants(
      metadata,
      pathTree,
      target,
    )
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
  isFlexOrGridChild: function (jsxMetadata: ElementInstanceMetadataMap, path: ElementPath) {
    return (
      MetadataUtils.isFlexLayoutedContainer(
        MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path)),
      ) || MetadataUtils.isGridCell(jsxMetadata, path)
    )
  },
  getRelativeAlignJustify: function (
    jsxMetadata: ElementInstanceMetadataMap,
    path: ElementPath,
  ): { align: 'alignSelf' | 'justifySelf'; justify: 'alignSelf' | 'justifySelf' } {
    const direction = MetadataUtils.getSimpleFlexDirection(
      MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path)),
    )
    const align = direction.direction === 'horizontal' ? 'alignSelf' : 'justifySelf'
    const justify = direction.direction === 'horizontal' ? 'justifySelf' : 'alignSelf'
    return { align, justify }
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
  getRootViewPathsOrdered(
    elements: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementPath> {
    const possibleRootElementsOfTarget = mapDropNulls((elementPath) => {
      if (EP.isRootElementOf(elementPath, target)) {
        return elementPath
      } else {
        return null
      }
    }, MetadataUtils.createOrderedElementPathsFromElements(elements, pathTree, [], []).navigatorTargets)
    return possibleRootElementsOfTarget
  },
  getRootViewsOrdered(
    elements: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const paths = MetadataUtils.getRootViewPathsOrdered(elements, pathTree, target)
    return mapDropNulls((path) => elements[EP.toString(path)], paths)
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
  getChildrenPathsOrdered(pathTree: ElementPathTrees, target: ElementPath): Array<ElementPath> {
    const subTree = getSubTree(pathTree, target)
    if (subTree == null) {
      return []
    } else {
      return getElementPathTreeChildren(subTree)
        .map((child) => child.path)
        .filter((path) => !EP.isRootElementOfInstance(path))
    }
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
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const childrenPaths = MetadataUtils.getChildrenPathsOrdered(pathTree, target)
    return mapDropNulls((childPath) => {
      return MetadataUtils.findElementByElementPath(elements, childPath)
    }, childrenPaths)
  },
  getImmediateChildrenPathsOrdered(
    elements: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementPath> {
    const element = MetadataUtils.findElementByElementPath(elements, target)
    if (element == null) {
      return []
    } else {
      const rootPaths = MetadataUtils.getRootViewPathsOrdered(elements, pathTree, target)
      const childrenPaths = MetadataUtils.getChildrenPathsOrdered(pathTree, target)
      return [...rootPaths, ...childrenPaths]
    }
  },
  getImmediateChildrenOrdered(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const roots = MetadataUtils.getRootViewsOrdered(metadata, pathTree, target)
    const children = MetadataUtils.getChildrenOrdered(metadata, pathTree, target)
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
  getScenesMetadata(metadata: ElementInstanceMetadataMap): Array<ElementInstanceMetadata> {
    return Object.values(metadata).filter(
      (metadataEntry) =>
        MetadataUtils.isProbablySceneFromMetadata(metadataEntry) ||
        MetadataUtils.isProbablyRemixSceneFromMetadata(metadataEntry),
    )
  },
  getAllStoryboardChildrenPathsOrdered(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
  ): ElementPath[] {
    const storyboardMetadata = MetadataUtils.getStoryboardMetadata(metadata)
    return storyboardMetadata == null
      ? []
      : MetadataUtils.getImmediateChildrenPathsOrdered(
          metadata,
          pathTree,
          storyboardMetadata.elementPath,
        )
  },
  getAllCanvasSelectablePathsOrdered(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
  ): ElementPath[] {
    // 1) Get the storyboard children
    const allPaths = objectValues(metadata).map((m) => m.elementPath)
    const storyboardChildren = allPaths.filter(EP.isStoryboardChild)

    // 2) Skip over any Scenes with children at this level
    const withScenesSkipped = flatMapArray((path) => {
      if (MetadataUtils.targetIsScene(metadata, path)) {
        const sceneChildren = MetadataUtils.getChildrenPathsOrdered(pathTree, path)
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
        const componentChildren = MetadataUtils.getChildrenPathsOrdered(pathTree, path)
        return [rootPath, ...componentChildren]
      }
    }, withScenesSkipped)

    return withComponentInstancesReplaced
  },
  getAllPaths: memoize(
    (metadata: ElementInstanceMetadataMap, pathTree: ElementPathTrees): ElementPath[] => {
      const projectTree = pathTree

      // This function needs to explicitly return the paths in a depth first manner
      let result: Array<ElementPath> = []
      function recurseElement(tree: ElementPathTree): void {
        result.push(tree.path)
        forEachElementPathTreeChild(tree, (childTree) => {
          recurseElement(childTree)
        })
      }

      const storyboardChildren = MetadataUtils.getAllStoryboardChildrenPathsOrdered(
        metadata,
        pathTree,
      )
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
    pathTree: ElementPathTrees,
  ): ElementPath[] {
    const projectTree = pathTree
    // This function needs to explicitly return the paths in a depth first manner
    let result: Array<ElementPath> = []
    function recurseElement(tree: ElementPathTree | null): void {
      if (tree != null) {
        result.push(tree.path)

        forEachElementPathTreeChild(tree, (childTree) => {
          recurseElement(childTree)
        })
      }
    }

    const rootInstances = this.getAllStoryboardChildrenPathsOrdered(metadata, pathTree)

    fastForEach(rootInstances, (rootInstance) => {
      const element = MetadataUtils.findElementByElementPath(metadata, rootInstance)
      if (element != null) {
        result.push(rootInstance)
        const rootElements = MetadataUtils.getRootViewPathsOrdered(
          metadata,
          pathTree,
          element.elementPath,
        )
        fastForEach(rootElements, (rootPath) => {
          const subTree = getSubTree(projectTree, rootPath)
          recurseElement(subTree)
        })
        const children = MetadataUtils.getChildrenPathsOrdered(pathTree, element.elementPath)
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
  isGroupAgainstImports(instance: ElementInstanceMetadata | null): boolean {
    return instance != null && MetadataUtils.isGivenUtopiaAPIElementFromImports(instance, 'Group')
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
  isHTML(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'html')
  },
  isBody(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'body')
  },
  isHead(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'head')
  },
  isHeading(instance: ElementInstanceMetadata): boolean {
    return (
      this.isElementOfType(instance, 'h1') ||
      this.isElementOfType(instance, 'h2') ||
      this.isElementOfType(instance, 'h3') ||
      this.isElementOfType(instance, 'h4') ||
      this.isElementOfType(instance, 'h5') ||
      this.isElementOfType(instance, 'h6')
    )
  },
  isInput(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'input')
  },
  isAnchorLink(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'a')
  },
  isParagraph(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'p')
  },
  isForm(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'form')
  },
  isReactSuspense(instance: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(instance, 'react', 'Suspense')
  },
  isRemixAwait(instance: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(instance, '@remix-run/react', 'Await')
  },
  isRemixScrollRestoration(instance: ElementInstanceMetadata | null): boolean {
    return MetadataUtils.isImportedComponentFromMetadata(
      instance,
      '@remix-run/react',
      'ScrollRestoration',
    )
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
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    propertyControlsInfo: PropertyControlsInfo,
  ): boolean {
    return (
      this.targetElementSupportsChildrenAlsoText(
        projectContents,
        path,
        metadata,
        pathTree,
        propertyControlsInfo,
      ) === 'supportsChildren'
    )
  },
  targetElementSupportsChildrenAlsoText(
    projectContents: ProjectContentTreeRoot,
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    propertyControlsInfo: PropertyControlsInfo,
  ): ElementSupportsChildren {
    const instance = MetadataUtils.findElementByElementPath(metadata, path)
    if (instance == null) {
      return 'doesNotSupportChildren'
    }
    return foldEither(
      (elementString) => {
        return intrinsicHTMLElementNamesThatSupportChildren.includes(elementString)
          ? 'supportsChildren'
          : 'doesNotSupportChildren'
      },
      (element) => {
        const elementResult = elementChildSupportsChildrenAlsoText(
          element,
          path,
          metadata,
          pathTree,
          projectContents,
          propertyControlsInfo,
        )
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
    target: ElementPath | null,
    pathTree: ElementPathTrees,
    propertyControlsInfo: PropertyControlsInfo,
  ): boolean {
    const targetSupportsChildrenValue = this.targetSupportsChildrenAlsoText(
      projectContents,
      metadata,
      target,
      pathTree,
      propertyControlsInfo,
    )
    return (
      targetSupportsChildrenValue !== 'doesNotSupportChildren' &&
      targetSupportsChildrenValue !== 'conditionalWithText'
    )
  },
  targetSupportsChildrenAlsoText(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
    pathTree: ElementPathTrees,
    propertyControlsInfo: PropertyControlsInfo,
  ): ElementSupportsChildren {
    if (target == null) {
      // Assumed to be reparenting to the canvas root.
      return 'supportsChildren'
    }
    const componentDescriptor = getComponentDescriptorForTarget(
      { propertyControlsInfo, projectContents },
      target,
    )
    if (componentDescriptor != null && !componentDescriptor.supportsChildren) {
      return 'doesNotSupportChildren'
    }
    const instance = MetadataUtils.findElementByElementPath(metadata, target)
    if (instance == null) {
      return withUnderlyingTarget(
        target,
        projectContents,
        'doesNotSupportChildren',
        (_, element) => {
          return (
            elementChildSupportsChildrenAlsoText(
              element,
              target,
              metadata,
              pathTree,
              projectContents,
              propertyControlsInfo,
            ) ?? 'doesNotSupportChildren'
          )
        },
      )
    }
    return MetadataUtils.targetElementSupportsChildrenAlsoText(
      projectContents,
      target,
      metadata,
      pathTree,
      propertyControlsInfo,
    )
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
    }
    if (
      isLeft(metadata.element) ||
      (isRight(metadata.element) && !isJSXElement(metadata.element.value))
    ) {
      return false
    }
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
  },
  targetHonoursPropsPosition(
    projectContents: ProjectContentTreeRoot,
    metadata: ElementInstanceMetadata | null,
  ): HonoursPosition {
    if (metadata == null) {
      return 'does-not-honour'
    } else {
      const underlyingComponent = findUnderlyingTargetComponentImplementationFromImportInfo(
        projectContents,
        metadata.importInfo,
      )
      if (underlyingComponent == null) {
        // Could be an external third party component, assuming true for now.
        return 'absolute-position-and-honours-numeric-props'
      } else {
        return componentHonoursPropsPosition(underlyingComponent)
      }
    }
  },
  intrinsicElementThatSupportsChildren: (element: JSXElementChild) => {
    return (
      isJSXElement(element) &&
      isIntrinsicHTMLElement(element.name) &&
      intrinsicHTMLElementNamesThatSupportChildren.includes(element.name.baseVariable)
    )
  },
  targetTextEditable(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath | null,
  ): boolean {
    if (target == null) {
      return false
    }
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    if (element == null) {
      // this case is necessary for expressions in conditional branches
      // these do not have metadata, but we still want them to be text editable
      return isTextEditableConditional(EP.parentPath(target), metadata, pathTree)
    }
    if (isLeft(element.element)) {
      return false
    }

    const elementValue = element.element.value
    switch (elementValue.type) {
      case 'JS_IDENTIFIER':
      case 'JS_PROPERTY_ACCESS':
      case 'JS_ELEMENT_ACCESS':
        return true
      default:
        break
    }

    if (treatElementAsGroupLikeFromMetadata(element)) {
      return false
    }

    if (
      isJSXElement(elementValue) &&
      isIntrinsicHTMLElement(elementValue.name) &&
      !PossibleTextElements.includes(elementValue.name.baseVariable)
    ) {
      return false
    }
    if (isJSXConditionalExpression(elementValue)) {
      return isTextEditableConditional(target, metadata, pathTree)
    }
    const children = MetadataUtils.getNonExpressionDescendants(metadata, pathTree, target)
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
    pathTree: ElementPathTrees,
    target: ElementPath | null,
  ): boolean {
    if (target == null) {
      return false
    }
    if (!MetadataUtils.targetTextEditable(metadata, pathTree, target)) {
      return false
    }

    const element = MetadataUtils.findElementByElementPath(metadata, target)
    if (element == null) {
      return false
    }

    if (isRight(element.element)) {
      const elementValue = element.element.value
      if (isJSXElement(elementValue)) {
        return (
          elementValue.children.length >= 1 &&
          elementValue.children.some((c) => {
            return (
              isJSXTextBlock(c) ||
              isJSExpressionMapOrOtherJavaScript(c) ||
              isJSIdentifier(c) ||
              isJSElementAccess(c) ||
              isJSPropertyAccess(c)
            )
          })
        )
      }
      if (isJSXConditionalExpression(elementValue)) {
        return isTextEditableConditional(target, metadata, pathTree)
      }
    }
    return false
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
        const componentFrame = MetadataUtils.getLocalFrame(target, metadata)
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
      pathTree: ElementPathTrees,
      collapsedViews: Array<ElementPath>,
      hiddenInNavigator: Array<ElementPath>,
    ): {
      navigatorTargets: Array<ElementPath>
      visibleNavigatorTargets: Array<ElementPath>
    } => {
      const projectTree = pathTree

      // This function exists separately from getAllPaths because the Navigator handles collapsed views
      let navigatorTargets: Array<ElementPath> = []
      let visibleNavigatorTargets: Array<ElementPath> = []

      function walkAndAddKeys(subTree: ElementPathTree | null, collapsedAncestor: boolean): void {
        if (subTree != null) {
          const path = subTree.path
          const isHiddenInNavigator = EP.containsPath(path, hiddenInNavigator)
          navigatorTargets.push(path)
          if (
            !collapsedAncestor &&
            !isHiddenInNavigator &&
            !MetadataUtils.isElementTypeHiddenInNavigator(path, metadata, pathTree)
          ) {
            visibleNavigatorTargets.push(path)
          }

          const isCollapsed = EP.containsPath(path, collapsedViews)
          const newCollapsedAncestor = collapsedAncestor || isCollapsed || isHiddenInNavigator

          let unfurledComponents: Array<ElementPathTree> = []

          forEachElementPathTreeChild(subTree, (child) => {
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

      const canvasRoots = getCanvasRoots(projectTree)
      fastForEach(canvasRoots, (canvasRoot) => {
        const subTree = getSubTree(projectTree, canvasRoot.path)

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
  isElementTypeHiddenInNavigator(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
  ): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    if (element == null) {
      return false
    } else {
      return foldEither(
        (l) => VoidElementsToFilter.includes(l),
        (r) => {
          if (isJSXElement(r)) {
            return VoidElementsToFilter.includes(r.name.baseVariable)
          }
          if (
            // when Data Entries are enabled, we want to show all expressions in the navigator
            !isFeatureEnabled('Condensed Navigator Entries')
          ) {
            if (isJSIdentifier(r) || isJSPropertyAccess(r) || isJSElementAccess(r)) {
              return true
            }
            if (
              isJSExpressionOtherJavaScript(r) &&
              !MetadataUtils.isElementPathConditionalFromMetadata(metadata, EP.parentPath(path))
            ) {
              const children = MetadataUtils.getChildrenOrdered(metadata, pathTree, path)
              // if the expression has children we have to show it in the navigator
              if (children.length > 0) {
                return false
              }
              const parentElement = MetadataUtils.findElementByElementPath(
                metadata,
                EP.parentPath(path),
              )
              // When the expression doesn't have children and the parent has text content, that
              // means this is a text expression, which should not appear in the navigator.
              // The generated text content itself will be the label of the parent.
              if (parentElement?.textContent != null && parentElement?.textContent.length > 0) {
                return true
              }
              // When the expression doesn't have children and the parent has no text content, then
              // the expression does not generate neither elements nor text.
              // In this case the expression doesn't generate anything, but we still want to show it in
              // the navigator, mostly to make sure to map expressions with zero elements are visible.
            }
          }
          return false
        },
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
  getClosestNonFragmentParent(
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
    pathTrees: ElementPathTrees,
    path: ElementPath,
  ): ElementPath {
    let currentPath = path
    while (!EP.isStoryboardPath(currentPath)) {
      if (!treatElementAsFragmentLike(metadata, allElementProps, pathTrees, currentPath)) {
        return currentPath
      }
      currentPath = EP.parentPath(currentPath)
    }
    return currentPath
  },
  getLocalFrame(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): MaybeInfinityLocalRectangle | null {
    function getNonRootParent(parentOf: ElementPath): ElementPath {
      // If the target is the root element of an instance (`a/b/c:root`), then we want to instead
      // find `a/b` as the component instance at `a/b/c` will have an identical globalFrame property.
      // So in that case go up to the grandparent and start checking from there again.
      if (EP.isRootElementOfInstance(parentOf)) {
        const grandParent = EP.parentPath(EP.parentPath(parentOf))
        if (EP.isRootElementOfInstance(grandParent)) {
          return getNonRootParent(grandParent)
        } else {
          return grandParent
        }
      } else {
        return EP.parentPath(parentOf)
      }
    }

    const targetGlobalFrame = MetadataUtils.getFrameInCanvasCoords(path, metadata)
    const parentPath = getNonRootParent(path)
    const parentGlobalFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, metadata)
    if (targetGlobalFrame == null || parentGlobalFrame == null) {
      return null
    } else if (isInfinityRectangle(targetGlobalFrame)) {
      return infinityLocalRectangle
    } else if (isInfinityRectangle(parentGlobalFrame)) {
      return localRectangle({
        x: targetGlobalFrame.x,
        y: targetGlobalFrame.y,
        width: targetGlobalFrame.width,
        height: targetGlobalFrame.height,
      })
    } else {
      return canvasRectangleToLocalRectangle(targetGlobalFrame, parentGlobalFrame)
    }
  },
  getFrameInCanvasCoords(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): MaybeInfinityCanvasRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return Utils.optionalMap((e) => e.globalFrame, element)
  },
  getFrameWithContentInCanvasCoords(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
  ): MaybeInfinityCanvasRectangle | null {
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    return Utils.optionalMap((e) => e.specialSizeMeasurements.globalFrameWithTextContent, element)
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
  getBoundingRectangleOfChildren(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    path: ElementPath,
  ): MaybeInfinityCanvasRectangle | null {
    const aabb = MetadataUtils.getBoundingRectangleInCanvasCoords(
      MetadataUtils.getChildrenPathsOrdered(pathTree, path),
      metadata,
    )
    return aabb
  },
  getFrameOrZeroRect(path: ElementPath, metadata: ElementInstanceMetadataMap): LocalRectangle {
    const frame = MetadataUtils.getLocalFrame(path, metadata)
    return zeroRectIfNullOrInfinity(frame)
  },
  getFrameRelativeTo(
    parent: ElementPath | null,
    metadata: ElementInstanceMetadataMap,
    frame: CanvasRectangle,
  ): LocalRectangle {
    if (parent == null) {
      return Utils.asLocal(frame)
    } else {
      const paths = EP.allPathsForLastPart(parent)
      const parentFrames: Array<MaybeInfinityLocalRectangle> = Utils.stripNulls(
        paths.map((path) => this.getLocalFrame(path, metadata)),
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
    if (parent.specialSizeMeasurements.globalContentBoxForChildren != null) {
      // TODO why is the globalContentBoxForChildren for the canvas root an infinity rectangle that then needs to be converted to zero rect? shouldn't we store a zero rect by default?
      return zeroRectIfNullOrInfinity(parent.specialSizeMeasurements.globalContentBoxForChildren)
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
    pathTree: ElementPathTrees,
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
              // Check for certain elements and check if they have text content within them. Only show the text content if they don't have children elements
              const numberOfChildrenElements = MetadataUtils.getNonExpressionDescendants(
                metadata,
                pathTree,
                element.elementPath,
              ).length
              if (
                // When Data Entries are enabled, we don't want to rename the parent elements based on their text / expression content
                !isFeatureEnabled('Condensed Navigator Entries')
              ) {
                if (numberOfChildrenElements === 0) {
                  if (PossibleTextElements.includes(lastNamePart)) {
                    if (element.textContent != null && element.textContent !== '') {
                      return element.textContent
                    }

                    // fall back to the old way of showing text content – this can probably be deleted now
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
                }
              }
              // With images, take their alt and src properties as possible names first.
              const elementProps = allElementProps[EP.toString(element.elementPath)] ?? {}
              if (lastNamePart === 'img') {
                const getProp = (prop: string): string | null => {
                  const value = elementProps[prop]
                  if (value != null && typeof value === 'string' && value.length > 0) {
                    return value
                  }
                  // Sometimes allElementProps doesn't contain the props yet
                  // This is just a quick fix, the real fix is to fix allElementProps
                  const attr = getJSXAttribute(jsxElement.props, prop)
                  if (attr != null && isJSXAttributeValue(attr) && typeof attr.value === 'string') {
                    return attr.value
                  }
                  return null
                }

                const alt = getProp('alt')
                if (alt != null) {
                  return alt
                }

                const src = getProp('src')
                if (src != null) {
                  if (src.startsWith('data:') && src.includes('base64')) {
                    return '<Base64 data>'
                  }
                  return src
                }
              }

              return lastNamePart
            case 'JSX_TEXT_BLOCK':
              return '(text)'
            case 'JSX_MAP_EXPRESSION':
              return 'List'
            case 'ATTRIBUTE_OTHER_JAVASCRIPT':
              return 'Code'
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
            case 'JS_IDENTIFIER':
              return '(code)'
            case 'JS_ELEMENT_ACCESS':
              return '(code)'
            case 'JS_PROPERTY_ACCESS':
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
    pathTree: ElementPathTrees,
    metadata: ElementInstanceMetadataMap,
    staticName: JSXElementName | null = null,
  ): string {
    const element = this.findElementByElementPath(metadata, path)
    if (element != null) {
      return MetadataUtils.getElementLabelFromMetadata(
        metadata,
        allElementProps,
        pathTree,
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
    return MetadataUtils.getJSXElementFromElementInstanceMetadata(
      MetadataUtils.findElementByElementPath(metadata, path),
    )
  },
  getJSXElementFromElementInstanceMetadata(
    element: ElementInstanceMetadata | null,
  ): JSXElement | null {
    if (element == null) {
      return null
    }
    return foldEither(
      (_) => null,
      (e) => (isJSXElement(e) ? e : null),
      element.element,
    )
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
  getDuplicationParentTargets(targets: ElementPath[]): ElementPath | null {
    return EP.getCommonParent(targets)
  },
  createElementPathTreeFromMetadata(metadata: ElementInstanceMetadataMap): ElementPathTrees {
    return buildTree(metadata)
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
  isLayoutedByFlowAndNotAbsolutePositioned(
    metadata: ElementInstanceMetadataMap,
    view: ElementPath,
  ): boolean {
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
  isAutofocusable(
    metadata: ElementInstanceMetadataMap,
    pathTrees: ElementPathTrees,
    path: ElementPath,
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
  ): boolean {
    const componentDescriptor = getComponentDescriptorForTarget(
      { propertyControlsInfo, projectContents },
      path,
    )
    return (
      componentDescriptor?.focus === 'always' ||
      (EP.isStoryboardDescendant(path) &&
        MetadataUtils.parentIsSceneWithOneChild(metadata, pathTrees, path))
    )
  },
  isAutomaticOrManuallyFocusableComponent(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    autoFocusedPaths: Array<ElementPath>,
    filePathMappings: FilePathMappings,
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
  ): boolean {
    return (
      EP.containsPath(path, autoFocusedPaths) ||
      MetadataUtils.isManuallyFocusableComponent(
        path,
        metadata,
        autoFocusedPaths,
        filePathMappings,
        propertyControlsInfo,
        projectContents,
      )
    )
  },
  isManuallyFocusableComponent(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    autoFocusedPaths: Array<ElementPath>,
    filePathMappings: FilePathMappings,
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
  ): boolean {
    const componentDescriptor = getComponentDescriptorForTarget(
      { propertyControlsInfo, projectContents },
      path,
    )
    if (componentDescriptor != null && componentDescriptor.focus !== 'default') {
      return false
    }
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    const isAnimatedComponent = isAnimatedElement(element)
    if (isAnimatedComponent) {
      return false
    }
    const isImported = isImportedComponent(element, filePathMappings)
    if (isImported) {
      return false
    }
    if (element?.isEmotionOrStyledComponent) {
      return false
    }
    const autoFocusable = EP.containsPath(path, autoFocusedPaths)
    if (autoFocusable) {
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
  getEmphasisOfComponent(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
  ): Emphasis {
    // Look up the emphasis of the component from the property controls.
    const componentDescriptor = getComponentDescriptorForTarget(
      { propertyControlsInfo, projectContents },
      path,
    )
    if (componentDescriptor != null) {
      return componentDescriptor.emphasis
    }

    const element = MetadataUtils.findElementByElementPath(metadata, path)
    // Element with flex or grid get high emphasis.
    if (
      MetadataUtils.isFlexLayoutedContainer(element) ||
      MetadataUtils.isGridLayoutedContainer(element)
    ) {
      return 'emphasized'
    }

    // Divs without styling that contain one or zero elements get low emphasis.
    if (element != null && MetadataUtils.isDiv(element)) {
      const children = MetadataUtils.getChildrenUnordered(metadata, path)
      if (children.length <= 1) {
        const attributes = toFirst(
          fromField<ElementInstanceMetadata, 'element'>('element')
            .compose(eitherRight())
            .compose(fromTypeGuard(isJSXElement))
            .compose(fromField('props')),
          element,
        )
        if (isRight(attributes)) {
          const styleAttributes = getJSXAttribute(attributes.value, 'style')
          if (styleAttributes == null) {
            return 'subdued'
          }
        }
      }
    }

    // Suspense and Await get low emphasis.
    if (
      MetadataUtils.isReactSuspense(element) ||
      MetadataUtils.isRemixAwait(element) ||
      MetadataUtils.isRemixScrollRestoration(element)
    ) {
      return 'subdued'
    }

    // Default to regular.
    return 'regular'
  },
  getIconOfComponent(
    path: ElementPath,
    propertyControlsInfo: PropertyControlsInfo,
    projectContents: ProjectContentTreeRoot,
  ): Icon | null {
    const componentDescriptor = getComponentDescriptorForTarget(
      { propertyControlsInfo, projectContents },
      path,
    )
    return componentDescriptor?.icon ?? null
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
    pathTree: ElementPathTrees,
    targets: Array<ElementPath>,
  ): Array<ElementPath> {
    const allPaths = MetadataUtils.getAllPaths(componentMetadata, pathTree)
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
  isRenderPropsFromMetadata(
    componentMetadata: ElementInstanceMetadataMap,
    elementPath: ElementPath,
  ): boolean {
    const staticElementPath = EP.dynamicPathToStaticPath(elementPath)
    const selectedElementUid = EP.toUid(staticElementPath)
    const parentPath = EP.parentPath(elementPath)
    const element = MetadataUtils.findElementByElementPath(componentMetadata, parentPath)
    if (
      element == null ||
      !isRight(element.element) ||
      element.element.value.type !== 'JSX_ELEMENT'
    ) {
      return false
    }

    const elementInProps = element.element.value.props.some(
      (prop) =>
        prop.type === 'JSX_ATTRIBUTES_ENTRY' &&
        prop.value.type === 'JSX_ELEMENT' &&
        prop.value.uid === selectedElementUid,
    )

    return elementInProps
  },
  isIdentifierFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return exists(
      notNull<ElementInstanceMetadata>()
        .compose(fromField('element'))
        .compose(eitherRight())
        .compose(fromTypeGuard(isJSIdentifier)),
      element,
    )
  },
  isPropertyAccessFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return exists(
      notNull<ElementInstanceMetadata>()
        .compose(fromField('element'))
        .compose(eitherRight())
        .compose(fromTypeGuard(isJSPropertyAccess)),
      element,
    )
  },
  isElementAccessFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return exists(
      notNull<ElementInstanceMetadata>()
        .compose(fromField('element'))
        .compose(eitherRight())
        .compose(fromTypeGuard(isJSElementAccess)),
      element,
    )
  },
  isConditional(target: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    return MetadataUtils.isConditionalFromMetadata(element)
  },
  isExpressionOtherJavascriptFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element?.element != null &&
      isRight(element.element) &&
      isJSExpressionOtherJavaScript(element.element.value)
    )
  },
  isExpressionOtherJavascript(target: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    return MetadataUtils.isExpressionOtherJavascriptFromMetadata(element)
  },
  isJSXMapExpressionFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element?.element != null &&
      isRight(element.element) &&
      isJSXMapExpression(element.element.value)
    )
  },
  isJSXMapExpression(target: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    return MetadataUtils.isJSXMapExpressionFromMetadata(element)
  },
  isJSXElementFromMetadata(element: ElementInstanceMetadata | null): boolean {
    return (
      element?.element != null && isRight(element.element) && isJSXElement(element.element.value)
    )
  },
  isJSXElement(target: ElementPath, metadata: ElementInstanceMetadataMap): boolean {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    return MetadataUtils.isJSXElementFromMetadata(element)
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
    pathTree: ElementPathTrees,
    parentPath: ElementPath,
  ): DetectedLayoutSystem {
    const childrenPaths = MetadataUtils.getChildrenPathsOrdered(pathTree, parentPath)
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
    pathTree: ElementPathTrees,
    parentPath: ElementPath,
  ): FlexDirection | null {
    const childrenPaths = MetadataUtils.getChildrenPathsOrdered(pathTree, parentPath)

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
          return conditionalClauseInsertionPath(
            parentElement.elementPath,
            'true-case',
            replaceWithSingleElement(),
          )
        } else if (getUtopiaID(conditionalExpression.whenFalse) === EP.toUid(target)) {
          return conditionalClauseInsertionPath(
            parentElement.elementPath,
            'false-case',
            replaceWithSingleElement(),
          )
        }
      }
      return childInsertionPath(parentElement.elementPath)
    }
  },
  canInsertElementsToTargetText(
    target: ElementPath,
    metadata: ElementInstanceMetadataMap,
    elementsToInsert: Array<JSXElementName>,
  ): boolean {
    const targetElementName = MetadataUtils.getJSXElementNameFromMetadata(metadata, target)
    const isTextElement =
      targetElementName != null && TextElements.includes(targetElementName.baseVariable)
    if (!isTextElement) {
      return true
    } else {
      return elementsToInsert.every((elementName) => {
        return TextElements.includes(elementName.baseVariable)
      })
    }
  },
  getNonExpressionDescendants(
    metadata: ElementInstanceMetadataMap,
    pathTree: ElementPathTrees,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const childrenInMetadata = MetadataUtils.getChildrenOrdered(metadata, pathTree, target)

    return childrenInMetadata.flatMap((c) =>
      getNonExpressionDescendantsInner(metadata, pathTree, c),
    )
  },
  getJsxElementChildFromMetadata(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): JSXElementChild {
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    if (element == null || isLeft(element.element)) {
      throw new Error(
        `invariant violation: JSXElementChild for ${EP.toString(target)} was not found in metadata`,
      )
    }
    return element.element.value
  },
  isElementDataReference(element: JSXElementChild | null): boolean {
    if (element == null) {
      return false
    }
    switch (element.type) {
      case 'ATTRIBUTE_FUNCTION_CALL':
      case 'ATTRIBUTE_NESTED_ARRAY': // TODO: reconsider nested array and nested object
      case 'ATTRIBUTE_NESTED_OBJECT':
      case 'JSX_ELEMENT':
      case 'JSX_FRAGMENT':
      case 'JSX_MAP_EXPRESSION':
      case 'JSX_CONDITIONAL_EXPRESSION':
        return false
      case 'ATTRIBUTE_OTHER_JAVASCRIPT': {
        // Attribute other javascript is only true if it does not have children entries in the metadata
        return Object.keys(element.elementsWithin).length === 0
      }
      case 'ATTRIBUTE_VALUE':
      case 'JSX_TEXT_BLOCK':
      case 'JS_IDENTIFIER':
      case 'JS_ELEMENT_ACCESS':
      case 'JS_PROPERTY_ACCESS':
        return true
      default:
        assertNever(element)
    }
  },

  findSceneOfTarget(target: ElementPath, metadata: ElementInstanceMetadataMap): ElementPath | null {
    if (
      MetadataUtils.isProbablyScene(metadata, target) ||
      MetadataUtils.isProbablyRemixScene(metadata, target)
    ) {
      return target
    }
    const parent = EP.parentPath(target)
    if (EP.isEmptyPath(parent)) {
      return null
    }
    return MetadataUtils.findSceneOfTarget(parent, metadata)
  },
}

function getNonExpressionDescendantsInner(
  metadata: ElementInstanceMetadataMap,
  pathTree: ElementPathTrees,
  element: ElementInstanceMetadata,
): Array<ElementInstanceMetadata> {
  if (isRight(element.element) && isJSExpression(element.element.value)) {
    const expressionChildren = MetadataUtils.getChildrenOrdered(
      metadata,
      pathTree,
      element.elementPath,
    )
    return expressionChildren.flatMap((exprChild) =>
      getNonExpressionDescendantsInner(metadata, pathTree, exprChild),
    )
  }
  return [element]
}

export function fillMissingDataFromAncestors(mergedMetadata: ElementInstanceMetadataMap) {
  return [
    fillGlobalContentBoxFromAncestors,
    fillConditionalGlobalFrameFromAncestors,
    fillLayoutSystemForChildrenFromAncestors,
  ].reduce((metadata, fill) => fill(metadata), mergedMetadata)
}

function fillGlobalContentBoxFromAncestors(
  metadata: ElementInstanceMetadataMap,
): ElementInstanceMetadataMap {
  const workingElements = { ...metadata }

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

function fillLayoutSystemForChildrenFromAncestors(
  metadata: ElementInstanceMetadataMap,
): ElementInstanceMetadataMap {
  const workingElements = { ...metadata }

  const elementsWithoutLayoutSystemForChildren = Object.keys(workingElements).filter((p) => {
    return workingElements[p]?.specialSizeMeasurements.layoutSystemForChildren == null
  })
  // sorted, so that parents are fixed first
  elementsWithoutLayoutSystemForChildren.sort()

  fastForEach(elementsWithoutLayoutSystemForChildren, (pathStr) => {
    const elem = workingElements[pathStr]

    const parentPathStr = EP.toString(EP.parentPath(EP.fromString(pathStr)))

    const layoutSystemForChildren =
      workingElements[parentPathStr]?.specialSizeMeasurements.layoutSystemForChildren ?? null

    workingElements[pathStr] = {
      ...elem,
      specialSizeMeasurements: {
        ...elem.specialSizeMeasurements,
        layoutSystemForChildren: layoutSystemForChildren,
      },
    }
  })
  return workingElements
}

// There is a case when conditionals should inherit their globalFrame from their parent:
// - when a conditional has no siblings
// - when the active branch is a js expression
// - when that expression has no elementsWithin (so it is a leaf element in the hierarchy)
// We also allow embedded conditionals where the deepest one has the correct active branch and
// none of the conditionals have siblings.
// Why is this useful? Because these expressions don't have globalFrame, so their conditional parent do not have
// a globalFrame neither. But when these conditionals only contain text expressions, and they don't have siblings,
// it make sense to treat the whole parent element of the conditional as a container for the text expression itself.
// Example: In the case `<div>Hello</div>`, we treat the whole div as text, we do not differentiate between the size of
// Hello and the size of the div.
// When we have the element `<div>{true ? 'Hello' : <div />}</div>`, the conditional appears between the <div> parent and
// the Hello text, but we still want to be able to select and text edit the text content.
// Maybe in the future we will treat text expressions as elefants, and they will have a measured size, but until then this is
// a good compromise to give these conditionals/expressions selectability and editability on the canvas.
function fillConditionalGlobalFrameFromAncestors(
  metadata: ElementInstanceMetadataMap,
): ElementInstanceMetadataMap {
  const workingElements = { ...metadata }

  const conditionalsWithNoSiblingsAndExpressionActiveBranch = Object.keys(workingElements).filter(
    (p) => {
      const isConditionalsWithNoSiblingsAndExpressionActiveBranch = (
        element: ElementInstanceMetadata,
      ): boolean => {
        const condElement =
          isRight(element.element) && isJSXConditionalExpression(element.element.value)
            ? element.element.value
            : null

        // filter out non-conditional elements
        if (condElement == null || element.conditionValue === 'not-a-conditional') {
          return false
        }

        const activeBranch = element.conditionValue.active
          ? condElement.whenTrue
          : condElement.whenFalse

        // filter out elements where the active branch is not an expression or a conditional
        if (!(isJSExpression(activeBranch) || isJSXConditionalExpression(activeBranch))) {
          return false
        }

        const parentOfConditionalPath = EP.parentPath(element.elementPath)
        const parentOfConditionalElement = workingElements[EP.toString(parentOfConditionalPath)]

        // filter out elements which have siblings
        if (
          isRight(parentOfConditionalElement.element) &&
          isJSXElementLike(parentOfConditionalElement.element.value) &&
          parentOfConditionalElement.element.value.children.length !== 1
        ) {
          return false
        }

        // when the active branch is a conditional, call the same check recursively on the branch
        if (isJSXConditionalExpression(activeBranch)) {
          const activeBranchMetadata =
            workingElements[
              EP.toString(getConditionalClausePath(element.elementPath, activeBranch))
            ]
          return (
            activeBranchMetadata != null &&
            isConditionalsWithNoSiblingsAndExpressionActiveBranch(activeBranchMetadata)
          )
        }

        // The only remaining question is whether the active branch an expression or not and whether it
        // is leaf element in the hierarchy
        return isJSExpression(activeBranch) && !hasElementsWithin(activeBranch)
      }

      return isConditionalsWithNoSiblingsAndExpressionActiveBranch(workingElements[p])
    },
  )

  // sorted, so that parents are fixed first
  conditionalsWithNoSiblingsAndExpressionActiveBranch.sort()

  fastForEach(conditionalsWithNoSiblingsAndExpressionActiveBranch, (pathStr) => {
    const elem = workingElements[pathStr]

    const condParentPathStr = EP.toString(EP.parentPath(elem.elementPath))
    const condParentGlobalFrame = workingElements[condParentPathStr]?.globalFrame

    const condParentglobalFrameWithTextContent =
      workingElements[condParentPathStr]?.specialSizeMeasurements.globalFrameWithTextContent

    workingElements[pathStr] = {
      ...elem,
      globalFrame: condParentGlobalFrame,
      specialSizeMeasurements: {
        ...elem.specialSizeMeasurements,
        globalFrameWithTextContent: condParentglobalFrameWithTextContent,
      },
    }
  })

  return workingElements
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

export function propertyHasSimpleValue(
  attributes: PropsOrJSXAttributes,
  property: PropertyPath,
  value: string | number | boolean | null | undefined,
): boolean {
  const propertyFromProps = getSimpleAttributeAtPath(attributes, property)
  return foldEither(
    () => false,
    (valueFromProps) => valueFromProps === value,
    propertyFromProps,
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
  specialSizeMeasurements.coordinateSystemBounds = zeroCanvasRect

  return elementInstanceMetadata(
    path,
    right(element),
    frame,
    frame,
    false,
    false,
    specialSizeMeasurements,
    null,
    null,
    null,
    null,
    'not-a-conditional',
    null,
    null,
    null,
  )
}

export function getRootPath(startingMetadata: ElementInstanceMetadataMap): ElementPath | null {
  const storyboard = MetadataUtils.getStoryboardMetadata(startingMetadata)
  if (storyboard == null) {
    return null
  }
  return storyboard.elementPath
}

export function getZIndexOrderedViewsWithoutDirectChildren(
  targets: Array<ElementPath>,
  navigatorTargets: Array<NavigatorEntry>, // TODO could this be instead the ElementPathTree?
): Array<ElementPath> {
  let targetsAndZIndex: Array<{ target: ElementPath; index: number }> = []
  fastForEach(targets, (target) => {
    const index = navigatorTargets.findIndex(
      (entry) => isRegularNavigatorEntry(entry) && EP.pathsEqual(entry.elementPath, target),
    )
    targetsAndZIndex.push({ target: target, index: index })
  })
  targetsAndZIndex.sort((a, b) => a.index - b.index)
  const orderedTargets = pluck(targetsAndZIndex, 'target')

  // keep direct children from reparenting
  let filteredTargets: Array<ElementPath> = []
  fastForEach(orderedTargets, (target) => {
    if (!orderedTargets.some((tp) => EP.pathsEqual(EP.parentPath(target), tp))) {
      filteredTargets.push(target)
    }
  })
  return filteredTargets
}

export function countSetProperties(
  targets: PropertyPath[],
  elementProps: PropsOrJSXAttributes,
): number {
  return targets.filter((curr) => {
    const attr = getSimpleAttributeAtPath(elementProps, curr)
    return isRight(attr) && attr.value != null
  }, 0).length
}
