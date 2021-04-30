import * as OPI from 'object-path-immutable'
import * as R from 'ramda'
import { FlexLength, LayoutSystem, Sides } from 'utopia-api'
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
import { LayoutProp } from '../layout/layout-helpers-new'
import {
  flattenArray,
  mapDropNulls,
  pluck,
  stripNulls,
  flatMapArray,
  uniqBy,
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
} from '../shared/element-template'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attributes'
import {
  CanvasPoint,
  CanvasRectangle,
  canvasRectangle,
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
import { findJSXElementChildAtPath, getUtopiaID } from './element-template-utils'
import {
  isImportedComponent,
  isAnimatedElementAgainstImports,
  isGivenUtopiaAPIElement,
  isSceneAgainstImports,
  isUtopiaAPIComponent,
  isViewAgainstImports,
  getUtopiaJSXComponentsFromSuccess,
} from './project-file-utils'
import { ResizesContentProp } from './scene-utils'
import { fastForEach } from '../shared/utils'
import { omit } from '../shared/object-utils'
import { UTOPIA_LABEL_KEY } from './utopia-constants'
import { withUnderlyingTarget } from '../../components/editor/store/editor-state'
import { ProjectContentTreeRoot } from '../../components/assets'
const ObjectPathImmutable: any = OPI

type MergeCandidate = These<ElementInstanceMetadata, ElementInstanceMetadata>

function fromSpyMergeCandidate(fromSpy: ElementInstanceMetadata): MergeCandidate {
  return makeThis(fromSpy)
}

function fromDOMMergeCandidate(fromDOM: ElementInstanceMetadata): MergeCandidate {
  return makeThat(fromDOM)
}

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
        (success, element, underlyingTarget, underlyingFilePath) => {
          return MetadataUtils.getElementOriginType(
            getUtopiaJSXComponentsFromSuccess(success),
            underlyingTarget,
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
  isSceneTreatedAsGroup(scene: ElementInstanceMetadata | null): boolean {
    if (scene == null) {
      return false
    } else {
      return scene.props[ResizesContentProp] ?? false
    }
  },
  findElements(
    elementMap: ElementInstanceMetadataMap,
    predicate: (element: ElementInstanceMetadata) => boolean,
  ): Array<ElementInstanceMetadata> {
    return Object.values(elementMap).filter(predicate)
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
    const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parentPath)

    const siblingPathsOrNull = EP.isRootElementOfInstance(target)
      ? parentMetadata?.rootElements
      : parentMetadata?.children
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
    element: ElementInstanceMetadata,
  ): boolean {
    return (
      MetadataUtils.isParentYogaLayoutedContainerForElement(element) &&
      !MetadataUtils.isPositionAbsolute(element)
    )
  },
  isParentYogaLayoutedContainerForElement(element: ElementInstanceMetadata): boolean {
    return element.specialSizeMeasurements.parentLayoutSystem === 'flex'
  },
  isGroup(path: ElementPath | null, metadata: ElementInstanceMetadataMap): boolean {
    if (path == null) {
      return false
    } else {
      const instance = MetadataUtils.findElementByElementPath(metadata, path)
      if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
        return (
          LayoutHelpers.getLayoutSystemFromProps(right(instance.element.value.props)) ===
          LayoutSystem.Group
        )
      } else {
        return false
      }
    }
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
  isButton(
    target: ElementPath,
    components: Array<UtopiaJSXComponent>,
    metadata: ElementInstanceMetadataMap,
  ): boolean {
    const elementName = MetadataUtils.getJSXElementName(target, components)
    if (
      elementName != null &&
      PP.depth(elementName.propertyPath) === 0 &&
      elementName.baseVariable === 'button'
    ) {
      return true
    }
    const instance = MetadataUtils.findElementByElementPath(metadata, target)
    if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
      const buttonRoleFound = instance.element.value.props.some(
        (attribute) =>
          attribute.key === 'role' &&
          eitherToMaybe(jsxSimpleAttributeToValue(attribute.value)) === 'button',
      )
      if (buttonRoleFound) {
        return true
      }
    }
    return instance?.specialSizeMeasurements.htmlElementName.toLowerCase() === 'button'
  },
  getYogaSizeProps(
    target: ElementPath,
    metadata: ElementInstanceMetadataMap,
    components: Array<UtopiaJSXComponent>,
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
        const element = findJSXElementChildAtPath(components, staticTarget)
        if (element != null && isJSXElement(element)) {
          const widthLookupAxis: LayoutProp = flexDirection === 'horizontal' ? 'flexBasis' : 'Width'
          const heightLookupAxis: LayoutProp = flexDirection === 'vertical' ? 'flexBasis' : 'Height'
          let result: Partial<Size> = {}
          const width: Either<string, FlexLength> = alternativeEither(
            getLayoutProperty(widthLookupAxis, right(element.props)),
            getLayoutProperty('Width', right(element.props)),
          )
          const height: Either<string, FlexLength> = alternativeEither(
            getLayoutProperty(heightLookupAxis, right(element.props)),
            getLayoutProperty('Height', right(element.props)),
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
  getFlexWrap: function (
    instance: ElementInstanceMetadata | null,
  ): 'wrap' | 'wrap-reverse' | 'nowrap' {
    if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
      return FlexLayoutHelpers.getFlexWrap(instance.element.value.props)
    } else {
      return 'nowrap' // TODO read this value from spy
    }
  },
  isAutoSizingView(element: ElementInstanceMetadata | null): boolean {
    if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
      // TODO NEW Property Path
      const isAutoSizing =
        LayoutHelpers.getLayoutSystemFromProps(right(element.element.value.props)) === 'group'
      const isYogaLayoutedContainer = MetadataUtils.isFlexLayoutedContainer(element)
      const hasChildren = element.children.length > 0
      const parentIsYoga = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        element,
      )
      return isAutoSizing && !isYogaLayoutedContainer && hasChildren && !parentIsYoga
    } else {
      return false
    }
  },
  isAutoSizingViewFromComponents(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath | null,
  ): boolean {
    if (target == null) {
      return false
    }
    const instance = MetadataUtils.findElementByElementPath(metadata, target)
    return MetadataUtils.isAutoSizingView(instance)
  },
  isAutoSizingText(imports: Imports, instance: ElementInstanceMetadata): boolean {
    return (
      MetadataUtils.isTextAgainstImports(imports, instance) && instance.props.textSizing === 'auto'
    )
  },
  findNonGroupParent(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): ElementPath | null {
    const parentPath = EP.parentPath(target)

    if (parentPath == null) {
      return null
    } else if (EP.isStoryboardChild(parentPath)) {
      // we've reached the top
      return parentPath
    } else {
      const parent = MetadataUtils.findElementByElementPath(metadata, parentPath)
      if (MetadataUtils.isAutoSizingView(parent)) {
        return MetadataUtils.findNonGroupParent(metadata, parentPath)
      } else {
        return parentPath
      }
    }
  },
  shiftGroupFrame(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
    originalFrame: CanvasRectangle | null,
    addOn: boolean,
  ): CanvasRectangle | null {
    if (originalFrame == null) {
      // if the originalFrame is null, we have nothing to shift
      return null
    }
    if (EP.isStoryboardChild(target)) {
      // If it's a scene we don't need to shift
      return originalFrame
    }

    const shiftMultiplier = addOn ? 1 : -1
    let workingFrame: CanvasRectangle = originalFrame
    // If this is held within a group, then we need to add on the frames of the parent groups.
    let ancestorPath = EP.parentPath(target)
    while (!EP.isStoryboardChild(ancestorPath) && !EP.isEmptyPath(ancestorPath)) {
      const ancestorElement = MetadataUtils.findElementByElementPath(metadata, ancestorPath)

      const ancestorParentPath = EP.parentPath(ancestorPath)
      if (ancestorElement == null) {
        break
      } else {
        if (MetadataUtils.isAutoSizingView(ancestorElement) && ancestorElement.localFrame != null) {
          // if the ancestorElement is a group, it better have a measurable frame, too,
          // TODO check with Sean if there are implications of this nullcheck
          workingFrame = Utils.offsetRect(workingFrame, {
            x: shiftMultiplier * ancestorElement.localFrame.x,
            y: shiftMultiplier * ancestorElement.localFrame.y,
          } as CanvasPoint)
        }
      }

      ancestorPath = ancestorParentPath
    }

    return workingFrame
  },
  setPropertyDirectlyIntoMetadata(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
    property: PropertyPath,
    value: any,
  ): ElementInstanceMetadataMap {
    return this.transformAtPathOptionally(metadata, target, (element) => {
      return {
        ...element,
        props: ObjectPathImmutable.set(element.props, PP.getElements(property), value),
      }
    })
  },
  unsetPropertyDirectlyIntoMetadata(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
    property: PropertyPath,
  ): ElementInstanceMetadataMap {
    return this.transformAtPathOptionally(metadata, target, (element) => {
      return {
        ...element,
        props: ObjectPathImmutable.del(element.props, PP.getElements(property)),
      }
    })
  },
  getRootViewPaths(elements: ElementInstanceMetadataMap, target: ElementPath): Array<ElementPath> {
    const element = MetadataUtils.findElementByElementPath(elements, target)
    return element?.rootElements ?? []
  },
  getRootViews(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const rootPaths = MetadataUtils.getRootViewPaths(elements, target)
    return MetadataUtils.findElementsByElementPath(elements, rootPaths ?? [])
  },
  getChildrenPaths(elements: ElementInstanceMetadataMap, target: ElementPath): Array<ElementPath> {
    const element = MetadataUtils.findElementByElementPath(elements, target)
    return element?.children ?? []
  },
  getChildren(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const childrenPaths = MetadataUtils.getChildrenPaths(elements, target)
    return MetadataUtils.findElementsByElementPath(elements, childrenPaths ?? [])
  },
  getImmediateChildrenPaths(
    elements: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementPath> {
    const element = MetadataUtils.findElementByElementPath(elements, target)
    return element == null ? [] : [...element.rootElements, ...element.children]
  },
  getImmediateChildren(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): Array<ElementInstanceMetadata> {
    const childrenPaths = MetadataUtils.getImmediateChildrenPaths(metadata, target)
    return MetadataUtils.findElementsByElementPath(metadata, childrenPaths ?? [])
  },
  getChildrenHandlingGroups(
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
    includeGroups: boolean,
  ): Array<ElementInstanceMetadata> {
    const immediateChildren = MetadataUtils.getImmediateChildren(metadata, target)

    const getChildrenInner = (
      childInstance: ElementInstanceMetadata,
    ): Array<ElementInstanceMetadata> => {
      // autoSizing views are the new groups
      if (this.isAutoSizingViewFromComponents(metadata, childInstance.elementPath)) {
        const rawChildren = MetadataUtils.findElementsByElementPath(
          metadata,
          childInstance.children,
        )
        const children = Utils.flatMapArray(getChildrenInner, rawChildren)
        if (includeGroups) {
          return [childInstance, ...children]
        } else {
          return children
        }
      } else {
        return [childInstance]
      }
    }

    return Utils.flatMapArray(getChildrenInner, immediateChildren)
  },
  getStoryboardMetadata(metadata: ElementInstanceMetadataMap): ElementInstanceMetadata | null {
    return Object.values(metadata).find((e) => EP.isStoryboardPath(e.elementPath)) ?? null
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
      if (root.rootElements.length > 0) {
        return root.rootElements
      } else {
        return [root.elementPath]
      }
    }, rootScenesAndElements)
  },
  getAllPaths(metadata: ElementInstanceMetadataMap): ElementPath[] {
    // This function needs to explicitly return the paths in a depth first manner
    let result: Array<ElementPath> = []
    function recurseElement(elementPath: ElementPath): void {
      result.push(elementPath)
      const descendants = MetadataUtils.getImmediateChildrenPaths(metadata, elementPath)
      fastForEach(descendants, recurseElement)
    }

    const storyboardChildren = this.getAllStoryboardChildrenPaths(metadata)
    fastForEach(storyboardChildren, recurseElement)

    return uniqBy<ElementPath>(result, EP.pathsEqual)
  },
  getAllPathsIncludingUnfurledFocusedComponents(
    metadata: ElementInstanceMetadataMap,
  ): ElementPath[] {
    // This function needs to explicitly return the paths in a depth first manner
    let result: Array<ElementPath> = []
    function recurseElement(elementPath: ElementPath): void {
      result.push(elementPath)
      const {
        children,
        unfurledComponents,
      } = MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(elementPath, metadata)
      const childrenIncludingUnfurledComponents = [...children, ...unfurledComponents]
      fastForEach(childrenIncludingUnfurledComponents, recurseElement)
    }

    const rootInstances = this.getAllStoryboardChildrenPaths(metadata)

    fastForEach(rootInstances, (rootInstance) => {
      const element = MetadataUtils.findElementByElementPath(metadata, rootInstance)
      if (element != null) {
        result.push(rootInstance)
        element.rootElements.forEach(recurseElement)
        element.children.forEach(recurseElement)
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
    imports: Imports,
    instance: ElementInstanceMetadata,
    elementType: string,
  ): boolean {
    // KILLME Replace with isGivenUtopiaAPIElementFromName from project-file-utils.ts
    return foldEither(
      (_) => false,
      (element) => isGivenUtopiaAPIElement(element, imports, elementType),
      instance.element,
    )
  },
  isViewAgainstImports(imports: Imports, instance: ElementInstanceMetadata | null): boolean {
    return (
      instance != null &&
      MetadataUtils.isGivenUtopiaAPIElementFromImports(imports, instance, 'View')
    )
  },
  isImg(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'img')
  },
  isTextAgainstImports(imports: Imports, instance: ElementInstanceMetadata | null): boolean {
    return (
      instance != null &&
      MetadataUtils.isGivenUtopiaAPIElementFromImports(imports, instance, 'Text')
    )
  },
  isDiv(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'div')
  },
  isSpan(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'span')
  },
  overflows(instance: ElementInstanceMetadata | null): boolean {
    if (instance != null) {
      const overflow = Utils.propOr('visible', 'overflow', instance.props.style)
      return overflow !== 'hidden' && overflow !== 'clip'
    } else {
      return false
    }
  },
  targetElementSupportsChildren(imports: Imports, instance: ElementInstanceMetadata): boolean {
    // FIXME Replace with a property controls check
    const elementEither = instance.element

    if (isLeft(elementEither)) {
      return intrinsicHTMLElementNamesThatSupportChildren.includes(elementEither.value)
    } else {
      const element = elementEither.value
      if (isJSXElement(element) && isUtopiaAPIComponent(element.name, imports)) {
        // Explicitly prevent components / elements that we *know* don't support children
        return (
          isViewAgainstImports(element.name, imports) || isSceneAgainstImports(element, imports)
        )
      } else {
        // We don't know at this stage
        return true
      }
    }
  },
  targetSupportsChildren(
    imports: Imports,
    metadata: ElementInstanceMetadataMap,
    target: ElementPath,
  ): boolean {
    const instance = MetadataUtils.findElementByElementPath(metadata, target)
    return instance == null ? false : MetadataUtils.targetElementSupportsChildren(imports, instance)
  },
  // TODO update this to work with the natural width / height
  getImageMultiplier(
    metadata: ElementInstanceMetadataMap,
    targets: Array<ElementPath>,
  ): number | null {
    const multipliers: Set<number> = Utils.emptySet()
    Utils.fastForEach(targets, (target) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, target)
      if (instance != null && this.isImg(instance)) {
        const componentFrame = instance.localFrame
        if (componentFrame != null) {
          const imageSize = getImageSize(instance)
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
  createOrderedElementPathsFromElements(
    metadata: ElementInstanceMetadataMap,
    collapsedViews: Array<ElementPath>,
  ): {
    navigatorTargets: Array<ElementPath>
    visibleNavigatorTargets: Array<ElementPath>
  } {
    // This function exists separately from getAllPaths because the Navigator has a specific
    // ordering for the paths, which arguably should go in the bin
    let navigatorTargets: Array<ElementPath> = []
    let visibleNavigatorTargets: Array<ElementPath> = []

    function walkAndAddKeys(path: ElementPath, collapsedAncestor: boolean): void {
      navigatorTargets.push(path)
      if (!collapsedAncestor) {
        visibleNavigatorTargets.push(path)
      }

      const {
        children,
        unfurledComponents,
      } = MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(path, metadata)
      const childrenIncludingFocusedElements = [...children, ...unfurledComponents]
      const reversedChildren = R.reverse(childrenIncludingFocusedElements)

      const isCollapsed = EP.containsPath(path, collapsedViews)
      fastForEach(reversedChildren, (childElement) => {
        walkAndAddKeys(childElement, collapsedAncestor || isCollapsed)
      })
    }

    const reverseCanvasRoots = MetadataUtils.getAllStoryboardChildrenPaths(metadata).reverse()
    fastForEach(reverseCanvasRoots, (childElement) => {
      walkAndAddKeys(childElement, false)
    })

    return {
      navigatorTargets: navigatorTargets,
      visibleNavigatorTargets: visibleNavigatorTargets,
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
      return R.reduce(
        (working, next) => {
          return Utils.offsetRect(working, {
            x: -next.x,
            y: -next.y,
          } as LocalRectangle)
        },
        Utils.asLocal(frame),
        parentFrames,
      )
    }
  },
  getElementLabelFromProps(element: ElementInstanceMetadata): string | null {
    const dataLabelProp = element.props[UTOPIA_LABEL_KEY]
    if (dataLabelProp != null && typeof dataLabelProp === 'string' && dataLabelProp.length > 0) {
      return dataLabelProp
    } else {
      return null
    }
  },
  getElementLabel(
    path: ElementPath,
    metadata: ElementInstanceMetadataMap,
    staticName: JSXElementName | null = null,
  ): string {
    const element = this.findElementByElementPath(metadata, path)
    if (element != null) {
      const sceneLabel = element.label // KILLME?
      const dataLabelProp = MetadataUtils.getElementLabelFromProps(element)
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
                if (lastNamePart === 'img') {
                  const alt = element.props['alt']
                  if (alt != null && typeof alt === 'string' && alt.length > 0) {
                    return alt
                  }
                  const src = element.props['src']
                  if (src != null && typeof src === 'string' && src.length > 0) {
                    return src
                  }
                }
                // For Text elements, use their text property if it exists.
                if (lastNamePart === 'Text') {
                  const text = element.props['text']
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
    }

    // Default catch all name, will probably avoid some odd cases in the future.
    return 'Element'
  },
  getJSXElementName(
    path: ElementPath,
    components: Array<UtopiaJSXComponent>,
  ): JSXElementName | null {
    const jsxElement = findElementAtPath(path, components)
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
    fromDOM: Array<ElementInstanceMetadata>,
  ): ElementInstanceMetadataMap {
    // This logic effectively puts everything from the spy first,
    // then anything missed out from the DOM right after it.
    // Ideally this would function like a VCS diff inserting runs of new elements
    // inbetween matching metadata, so it may be necessary to implement something
    // like that in the future. But for now this is likely "good enough" that it
    // wont make any difference.
    let workingElements: ElementInstanceMetadataMap = { ...fromSpy }
    let newlyFoundElements: Array<ElementPath> = []
    fastForEach(fromDOM, (domElem) => {
      const spyElem = MetadataUtils.findElementByElementPath(fromSpy, domElem.elementPath)

      // Checking if our elements support children should prevent us from ending up with the
      // internals of draft-js showing up underneath Text elements.
      const shouldNotTraverse = Utils.path(['props', 'data-utopia-do-not-traverse'], fromDOM)
      let children: Array<ElementPath>
      let rootElements: Array<ElementPath>
      if (shouldNotTraverse) {
        children = []
        rootElements = []
      } else {
        children = EP.addPathsIfMissing(spyElem?.children ?? [], domElem.children)
        rootElements = EP.addPathsIfMissing(spyElem?.rootElements ?? [], domElem.rootElements)
      }

      if (spyElem == null) {
        const elem =
          children === domElem.children && rootElements === domElem.rootElements
            ? domElem
            : {
                ...domElem,
                children: children,
                rootElements: rootElements,
              }
        workingElements[EP.toString(domElem.elementPath)] = elem
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
          props: spyElem.props,
          element: jsxElement,
          children: children,
          rootElements: rootElements,
          componentInstance: componentInstance,
          isEmotionOrStyledComponent: spyElem.isEmotionOrStyledComponent,
          label: spyElem.label,
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
    const parentPath = EP.parentPath(target)
    let remainingElements: ElementInstanceMetadataMap = omit([EP.toString(target)], metadata)
    if (Object.keys(remainingElements).length === Object.keys(metadata).length) {
      // Nothing was removed
      return metadata
    }

    const updatedElements = MetadataUtils.transformAtPathOptionally(
      remainingElements,
      parentPath,
      (elem) => {
        const updatedChildren = elem.children.filter((child) => !EP.pathsEqual(child, target))
        if (updatedChildren.length === elem.children.length) {
          return elem
        } else {
          return {
            ...elem,
            children: updatedChildren,
          }
        }
      },
    )
    return updatedElements
  },
  updateParentWithNewChildPath(
    targetParent: ElementPath | null,
    childPath: ElementPath,
    elements: ElementInstanceMetadataMap,
    indexPosition: IndexPosition | null,
  ): ElementInstanceMetadataMap {
    const makeE = () => {
      // TODO delete me
      throw new Error('Should not attempt to create empty elements.')
    }
    if (targetParent == null) {
      // TODO Scene Implementation
      return elements
    } else {
      return this.transformAtPathOptionally(elements, targetParent, (parentElement) => {
        let updatedChildren: Array<ElementPath>
        if (indexPosition == null) {
          updatedChildren = parentElement.children.concat(childPath)
        } else {
          updatedChildren = Utils.addToArrayWithFill(
            childPath,
            parentElement.children,
            indexPosition,
            makeE,
          )
        }
        return {
          ...parentElement,
          children: updatedChildren,
        }
      })
    }
  },
  insertElementMetadataChild(
    targetParent: ElementPath | null,
    elementToInsert: ElementInstanceMetadata,
    metadata: ElementInstanceMetadataMap,
    indexPosition: IndexPosition | null,
  ): ElementInstanceMetadataMap {
    // Insert into the map
    const withNewElement: ElementInstanceMetadataMap = {
      ...metadata,
      [EP.toString(elementToInsert.elementPath)]: elementToInsert,
    }

    // Update the parent
    const updatedElements = this.updateParentWithNewChildPath(
      targetParent,
      elementToInsert.elementPath,
      withNewElement,
      indexPosition,
    )
    return updatedElements
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
        children: [], // all descendants have new UID-s
        rootElements: [], // all descendants have new UID-s
      }

      workingElements[EP.toString(newElementPath)] = newElementMetadata
      return newElementPath
    }

    // Everything about this feels wrong
    const originalMetadata = MetadataUtils.findElementByElementPath(metadata, oldPath)
    if (originalMetadata == null) {
      return metadata
    } else {
      const duplicatedElementPath = duplicateElementMetadata(
        originalMetadata,
        oldPath,
        newPath,
        newElement,
      )
      const updatedElements = this.updateParentWithNewChildPath(
        EP.parentPath(duplicatedElementPath),
        duplicatedElementPath,
        workingElements,
        {
          type: 'back',
        },
      )

      return updatedElements
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
            children: updateChildren(existing.children),
            rootElements: updateChildren(existing.rootElements),
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
    return elementName != null && !isIntrinsicElement(elementName)
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
    const specialSizeMeasurements = MetadataUtils.findElementByElementPath(elementMap, path)
      ?.specialSizeMeasurements
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
    const specialSizeMeasurements = MetadataUtils.findElementByElementPath(elementMap, path)
      ?.specialSizeMeasurements
    const parentSizeMeasurements = MetadataUtils.findElementByElementPath(elementMap, parentPath)
      ?.specialSizeMeasurements
    if (parentPath == null || specialSizeMeasurements == null || parentSizeMeasurements == null) {
      return null
    }
    if (specialSizeMeasurements.flexDirection !== parentSizeMeasurements.flexDirection) {
      return parentPath
    } else {
      return this.findNearestAncestorFlexDirectionChange(elementMap, parentPath)
    }
  },
  isFocusableComponent(
    path: ElementPath,
    components: UtopiaJSXComponent[],
    metadata: ElementInstanceMetadataMap,
    imports: Imports,
  ): boolean {
    const elementName = MetadataUtils.getJSXElementName(path, components)
    const element = MetadataUtils.findElementByElementPath(metadata, path)
    if (element?.isEmotionOrStyledComponent) {
      return false
    }
    const isAnimatedComponent =
      elementName != null && isAnimatedElementAgainstImports(elementName, imports)
    if (isAnimatedComponent) {
      return false
    }
    const isImported = elementName != null && isImportedComponent(elementName, imports)
    if (isImported) {
      return false
    }
    const isComponent = elementName != null && !isIntrinsicHTMLElement(elementName)
    if (isComponent) {
      return true
    } else {
      return false
    }
  },
  isFocusableLeafComponent(
    path: ElementPath,
    components: UtopiaJSXComponent[],
    metadata: ElementInstanceMetadataMap,
    imports: Imports,
  ): boolean {
    return (
      MetadataUtils.getChildrenPaths(metadata, path).length === 0 &&
      MetadataUtils.isFocusableComponent(path, components, metadata, imports)
    )
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
