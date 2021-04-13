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
  id,
  Imports,
  InstancePath,
  PropertyPath,
  ScenePath,
  StaticInstancePath,
  StaticTemplatePath,
  TemplatePath,
} from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import * as TP from '../shared/template-path'
import { findJSXElementChildAtPath, getUtopiaID } from './element-template-utils'
import {
  isAnimatedElementAgainstImports,
  isGivenUtopiaAPIElement,
  isImportedComponent,
  isUtopiaAPIComponent,
} from './project-file-utils'
import { EmptyScenePathForStoryboard, ResizesContentProp } from './scene-utils'
import { fastForEach } from '../shared/utils'
import { omit } from '../shared/object-utils'
const ObjectPathImmutable: any = OPI

type MergeCandidate = These<ElementInstanceMetadata, ElementInstanceMetadata>

function fromSpyMergeCandidate(fromSpy: ElementInstanceMetadata): MergeCandidate {
  return makeThis(fromSpy)
}

function fromDOMMergeCandidate(fromDOM: ElementInstanceMetadata): MergeCandidate {
  return makeThat(fromDOM)
}

export const getChildrenOfCollapsedViews = (
  templatePaths: TemplatePath[],
  collapsedViews: Array<TemplatePath>,
): Array<TemplatePath> => {
  return Utils.flatMapArray((view) => {
    return Utils.stripNulls(
      templatePaths.map((childPath) => {
        return TP.isAncestorOf(childPath, view) && !TP.pathsEqual(view, childPath)
          ? childPath
          : null
      }),
    )
  }, collapsedViews)
}

const ElementsToDrillIntoForTextContent = ['div', 'span']

export const MetadataUtils = {
  getElementOriginType(
    elements: Array<UtopiaJSXComponent>,
    target: TemplatePath,
  ): ElementOriginType {
    if (TP.isScenePath(target)) {
      return 'scene'
    } else {
      const staticTarget = this.dynamicPathToStaticPath(target)
      if (staticTarget == null) {
        return 'unknown-element'
      } else {
        if (TP.pathsEqual(target, staticTarget)) {
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
    }
  },
  anyUnknownOrGeneratedElements(
    elements: Array<UtopiaJSXComponent>,
    targets: Array<TemplatePath>,
  ): boolean {
    return targets.some((target) => {
      const originType = this.getElementOriginType(elements, target)
      return (
        originType === 'unknown-element' || originType === 'generated-static-definition-present'
      )
    })
  },
  findElementByTemplatePath(
    elementMap: ElementInstanceMetadataMap,
    path: TemplatePath | null,
  ): ElementInstanceMetadata | null {
    if (path == null) {
      return null
    } else {
      const targetPath = TP.instancePathForElementAtPath(path)
      return elementMap[TP.toString(targetPath)] ?? null
    }
  },
  getElementByInstancePathMaybe(
    elementMap: ElementInstanceMetadataMap,
    path: InstancePath | null,
  ): ElementInstanceMetadata | null {
    if (path == null) {
      return null
    } else {
      return elementMap[TP.toString(path)] ?? null
    }
  },
  getElementsByInstancePath(
    elementMap: ElementInstanceMetadataMap,
    paths: Array<InstancePath>,
  ): Array<ElementInstanceMetadata> {
    return stripNulls(
      paths.map((path) => MetadataUtils.getElementByInstancePathMaybe(elementMap, path)),
    )
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
  getViewZIndexFromMetadata(metadata: ElementInstanceMetadataMap, target: TemplatePath): number {
    const siblings = MetadataUtils.getSiblings(metadata, target)
    return siblings.findIndex((child) => {
      return getUtopiaID(child) === TP.toUid(target)
    })
  },
  getParent(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath | null,
  ): ElementInstanceMetadata | null {
    if (target == null) {
      return null
    }
    const parentPath = TP.parentPath(target)
    if (parentPath == null || TP.isScenePath(parentPath)) {
      // TODO Scene Implementation
      return null
    } else {
      return this.getElementByInstancePathMaybe(metadata, parentPath)
    }
  },
  getSiblings(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath | null,
  ): ElementInstanceMetadata[] {
    if (target == null) {
      return []
    }

    const parentPath = TP.parentPath(target)
    if (parentPath == null) {
      return []
    } else if (TP.isScenePath(parentPath)) {
      const parentMetadata = MetadataUtils.findElementByTemplatePath(metadata, parentPath)
      const rootElementPaths = parentMetadata?.rootElements ?? []
      return MetadataUtils.getElementsByInstancePath(metadata, rootElementPaths)
    } else {
      const parent = metadata[TP.toString(parentPath)]
      return parent == null
        ? []
        : MetadataUtils.getElementsByInstancePath(metadata, parent.children)
    }
  },
  isParentYogaLayoutedContainerAndElementParticipatesInLayout(
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
  ): boolean {
    if (TP.isScenePath(path)) {
      // TODO Scene Implementation
      return false
    } else {
      const instance = this.getElementByInstancePathMaybe(metadata, path)
      return (
        optionalMap(
          MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout,
          instance,
        ) ?? false
      )
    }
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
  isGroup(path: TemplatePath | null, metadata: ElementInstanceMetadataMap): boolean {
    if (path == null) {
      return false
    } else if (TP.isScenePath(path)) {
      // TODO Scene Implementation
      return false
    } else {
      const instance = this.getElementByInstancePathMaybe(metadata, path)
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
    target: TemplatePath,
    components: Array<UtopiaJSXComponent>,
    metadata: ElementInstanceMetadataMap,
  ): boolean {
    const elementName = MetadataUtils.getJSXElementName(target, components, metadata)
    if (
      elementName != null &&
      PP.depth(elementName.propertyPath) === 0 &&
      elementName.baseVariable === 'button'
    ) {
      return true
    }
    const instance = TP.isInstancePath(target)
      ? this.getElementByInstancePathMaybe(metadata, target)
      : null
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
    target: TemplatePath,
    metadata: ElementInstanceMetadataMap,
    components: Array<UtopiaJSXComponent>,
  ): Partial<Size> {
    const parentInstance = this.getParent(metadata, target)
    if (parentInstance == null) {
      return {}
    } else {
      const flexDirection = getReorderDirection(this.getFlexDirection(parentInstance))

      if (TP.isInstancePath(target)) {
        const staticTarget = this.dynamicPathToStaticPath(target)
        if (staticTarget == null) {
          return {}
        } else {
          const element = findJSXElementChildAtPath(components, staticTarget)
          if (element != null && isJSXElement(element)) {
            const widthLookupAxis: LayoutProp =
              flexDirection === 'horizontal' ? 'flexBasis' : 'Width'
            const heightLookupAxis: LayoutProp =
              flexDirection === 'vertical' ? 'flexBasis' : 'Height'
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
      } else {
        return {}
      }
    }
  },
  getElementMargin(
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
  ): Partial<Sides> | null {
    if (TP.isInstancePath(path)) {
      const instance = MetadataUtils.getElementByInstancePathMaybe(metadata, path)
      if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
        return instance.specialSizeMeasurements.margin
      } else {
        return null
      }
    } else {
      return null
    }
  },
  getElementPadding(
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
  ): Partial<Sides> | null {
    if (TP.isInstancePath(path)) {
      const instance = MetadataUtils.getElementByInstancePathMaybe(metadata, path)
      if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
        return instance.specialSizeMeasurements.padding
      } else {
        return null
      }
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
      const isYogaLayoutedContainer = this.isFlexLayoutedContainer(element)
      const hasChildren = element.children.length > 0
      const parentIsYoga = this.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        element,
      )
      return isAutoSizing && !isYogaLayoutedContainer && hasChildren && !parentIsYoga
    } else {
      return false
    }
  },
  isAutoSizingViewFromComponents(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath | null,
  ): boolean {
    if (target == null || TP.isScenePath(target)) {
      return false
    }
    const instance = this.getElementByInstancePathMaybe(metadata, target)
    return this.isAutoSizingView(instance)
  },
  isAutoSizingText(imports: Imports, instance: ElementInstanceMetadata): boolean {
    return this.isTextAgainstImports(imports, instance) && instance.props.textSizing === 'auto'
  },
  findNonGroupParent(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath,
  ): TemplatePath | null {
    const parentPath = TP.parentPath(target)

    if (parentPath == null) {
      return null
    } else if (TP.isScenePath(parentPath)) {
      // we've reached the top
      return parentPath
    } else {
      const parent = MetadataUtils.getElementByInstancePathMaybe(metadata, parentPath)
      if (MetadataUtils.isAutoSizingView(parent)) {
        return MetadataUtils.findNonGroupParent(metadata, parentPath)
      } else {
        return parentPath
      }
    }
  },
  templatePathToStaticTemplatePath(path: TemplatePath | null): StaticTemplatePath | null {
    if (path == null) {
      return path
    } else {
      return TP.dynamicPathToStaticPath(path)
    }
  },
  dynamicPathToStaticPath(path: InstancePath): StaticInstancePath {
    return TP.dynamicPathToStaticPath(path)
  },
  shiftGroupFrame(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath,
    originalFrame: CanvasRectangle | null,
    addOn: boolean,
  ): CanvasRectangle | null {
    if (originalFrame == null) {
      // if the originalFrame is null, we have nothing to shift
      return null
    }
    if (TP.isScenePath(target)) {
      // If it's a scene we don't need to shift
      return originalFrame
    }

    const shiftMultiplier = addOn ? 1 : -1
    let workingFrame: CanvasRectangle = originalFrame
    // If this is held within a group, then we need to add on the frames of the parent groups.
    let ancestorPath = TP.instancePathParent(target)
    while (TP.isInstancePath(ancestorPath)) {
      const ancestorElement = MetadataUtils.getElementByInstancePathMaybe(metadata, ancestorPath)

      const ancestorParentPath = TP.instancePathParent(ancestorPath)
      if (ancestorElement == null) {
        break
      } else if (TP.isInstancePath(ancestorParentPath)) {
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
    target: InstancePath,
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
    target: InstancePath,
    property: PropertyPath,
  ): ElementInstanceMetadataMap {
    return this.transformAtPathOptionally(metadata, target, (element) => {
      return {
        ...element,
        props: ObjectPathImmutable.del(element.props, PP.getElements(property)),
      }
    })
  },
  getRootViews(elements: ElementInstanceMetadataMap, target: TemplatePath): Array<InstancePath> {
    const element = MetadataUtils.findElementByTemplatePath(elements, target)
    return element?.rootElements ?? []
  },
  getImmediateChildrenPaths(
    elements: ElementInstanceMetadataMap,
    target: TemplatePath,
  ): Array<InstancePath> {
    const element = MetadataUtils.findElementByTemplatePath(elements, target)
    return element == null ? [] : [...element.rootElements, ...element.children]
  },
  getImmediateChildren(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath,
  ): Array<ElementInstanceMetadata> {
    const childrenPaths = MetadataUtils.getImmediateChildrenPaths(metadata, target)
    return MetadataUtils.getElementsByInstancePath(metadata, childrenPaths ?? [])
  },
  getChildrenHandlingGroups(
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath,
    includeGroups: boolean,
  ): Array<ElementInstanceMetadata> {
    const immediateChildren = MetadataUtils.getImmediateChildren(metadata, target)

    const getChildrenInner = (
      childInstance: ElementInstanceMetadata,
    ): Array<ElementInstanceMetadata> => {
      // autoSizing views are the new groups
      if (this.isAutoSizingViewFromComponents(metadata, childInstance.templatePath)) {
        const rawChildren = MetadataUtils.getElementsByInstancePath(
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
    return Object.values(metadata).find((e) => TP.isStoryboardPath(e.templatePath)) ?? null
  },
  getAllStoryboardChildren(metadata: ElementInstanceMetadataMap): ElementInstanceMetadata[] {
    const storyboardMetadata = MetadataUtils.getStoryboardMetadata(metadata)
    return storyboardMetadata == null
      ? []
      : MetadataUtils.getImmediateChildren(metadata, storyboardMetadata.templatePath)
  },
  getAllStoryboardChildrenPaths(metadata: ElementInstanceMetadataMap): InstancePath[] {
    const storyboardMetadata = MetadataUtils.getStoryboardMetadata(metadata)
    return storyboardMetadata == null
      ? []
      : MetadataUtils.getImmediateChildrenPaths(metadata, storyboardMetadata.templatePath)
  },
  getAllStoryboardChildrenPathsScenesOnly(metadata: ElementInstanceMetadataMap): ScenePath[] {
    // FIXME Use the instance path after we separate Scene from the component it renders
    const children = MetadataUtils.getAllStoryboardChildren(metadata)
    return mapDropNulls(
      (e) =>
        MetadataUtils.elementIsScene(e)
          ? TP.scenePathForElementAtInstancePath(e.templatePath)
          : null,
      children,
    )
  },
  getAllCanvasRootPaths(metadata: ElementInstanceMetadataMap): TemplatePath[] {
    const rootScenesAndElements = MetadataUtils.getAllStoryboardChildren(metadata)
    return flatMapArray<ElementInstanceMetadata, TemplatePath>((root) => {
      if (root.rootElements.length > 0) {
        return root.rootElements
      } else {
        return [root.templatePath]
      }
    }, rootScenesAndElements)
  },
  getAllPaths(metadata: ElementInstanceMetadataMap): InstancePath[] {
    // This function needs to explicitly return the paths in a depth first manner
    let result: Array<InstancePath> = []
    function recurseElement(elementPath: InstancePath): void {
      result.push(elementPath)
      const element = MetadataUtils.getElementByInstancePathMaybe(metadata, elementPath)
      fastForEach(element?.children ?? [], recurseElement)
    }

    const rootInstances = this.getAllStoryboardChildrenPaths(metadata)

    fastForEach(rootInstances, (rootInstance) => {
      const element = MetadataUtils.findElementByTemplatePath(metadata, rootInstance)
      if (element != null) {
        result.push(rootInstance)
        element.rootElements.forEach(recurseElement)
        element.children.forEach(recurseElement)
      }
    })

    return uniqBy<InstancePath>(result, TP.pathsEqual)
  },
  getAllPathsIncludingUnfurledFocusedComponents(
    metadata: ElementInstanceMetadataMap,
    focusedElementPath: ScenePath | null,
  ): InstancePath[] {
    // This function needs to explicitly return the paths in a depth first manner
    let result: Array<InstancePath> = []
    function recurseElement(elementPath: InstancePath): void {
      result.push(elementPath)
      const {
        children,
        unfurledComponents,
      } = MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(
        elementPath,
        metadata,
        focusedElementPath,
      )
      const childrenIncludingUnfurledComponents = [...children, ...unfurledComponents]
      fastForEach(childrenIncludingUnfurledComponents, recurseElement)
    }

    const rootInstances = this.getAllStoryboardChildrenPaths(metadata)

    fastForEach(rootInstances, (rootInstance) => {
      const element = MetadataUtils.findElementByTemplatePath(metadata, rootInstance)
      if (element != null) {
        result.push(rootInstance)
        element.rootElements.forEach(recurseElement)
        element.children.forEach(recurseElement)
      }
    })

    return uniqBy<InstancePath>(result, TP.pathsEqual)
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
  isLayoutWrapperAgainstImports(
    imports: Imports,
    instance: ElementInstanceMetadata | null,
  ): boolean {
    return (
      instance != null &&
      (MetadataUtils.isGivenUtopiaAPIElementFromImports(imports, instance, 'Layoutable') ||
        MetadataUtils.isGivenUtopiaAPIElementFromImports(imports, instance, 'Positionable') ||
        MetadataUtils.isGivenUtopiaAPIElementFromImports(imports, instance, 'Resizeable'))
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
    // Explicitly prevent components / elements that we *know* don't support children
    if (this.isUtopiaAPIElementFromImports(imports, instance)) {
      return this.isViewAgainstImports(imports, instance)
    } else if (isLeft(instance.element)) {
      return intrinsicHTMLElementNamesThatSupportChildren.includes(instance.element.value)
    } else {
      return true
    }
  },
  targetSupportsChildren(
    imports: Imports,
    metadata: ElementInstanceMetadataMap,
    target: TemplatePath,
  ): boolean {
    if (TP.isScenePath(target)) {
      return true
    } else {
      const instance = this.getElementByInstancePathMaybe(metadata, target)
      return instance == null ? false : this.targetElementSupportsChildren(imports, instance)
    }
  },
  // TODO update this to work with the natural width / height
  getImageMultiplier(
    imports: Imports,
    metadata: ElementInstanceMetadataMap,
    targets: Array<TemplatePath>,
  ): number | null {
    const multipliers: Set<number> = Utils.emptySet()
    Utils.fastForEach(targets, (target) => {
      if (TP.isScenePath(target)) {
        return
      }
      const instance = this.getElementByInstancePathMaybe(metadata, target)
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
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
    focusedElementPath: ScenePath | null,
  ): { children: Array<InstancePath>; unfurledComponents: Array<InstancePath> } {
    const allPaths = Object.values(metadata).map((element) => element.templatePath)
    const children = MetadataUtils.getImmediateChildrenPaths(metadata, path)

    const matchingFocusPath =
      focusedElementPath == null
        ? null
        : TP.scenePathUpToElementPath(
            focusedElementPath,
            TP.elementPathForPath(path),
            'dynamic-scene-path',
          )
    const focusedRootElementPaths =
      matchingFocusPath == null
        ? []
        : allPaths.filter(
            (p) =>
              TP.depth(p) === 2 && // TODO this is actually pretty silly, TP.depth returns depth + 1 for legacy reasons
              TP.scenePathsEqual(TP.scenePathPartOfTemplatePath(p), matchingFocusPath),
          )

    return { children: children, unfurledComponents: focusedRootElementPaths }
  },
  getAllChildrenElementsIncludingUnfurledFocusedComponents(
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
    focusedElementPath: ScenePath | null,
  ): {
    children: Array<ElementInstanceMetadata>
    unfurledComponents: Array<ElementInstanceMetadata>
  } {
    const { children, unfurledComponents } = this.getAllChildrenIncludingUnfurledFocusedComponents(
      path,
      metadata,
      focusedElementPath,
    )

    return {
      children: mapDropNulls((childPath) => {
        return this.getElementByInstancePathMaybe(metadata, childPath)
      }, children),
      unfurledComponents: mapDropNulls((childPath) => {
        return this.getElementByInstancePathMaybe(metadata, childPath)
      }, unfurledComponents),
    }
  },
  createOrderedTemplatePathsFromElements(
    metadata: ElementInstanceMetadataMap,
    collapsedViews: Array<TemplatePath>,
    focusedElementPath: ScenePath | null,
  ): { navigatorTargets: Array<TemplatePath>; visibleNavigatorTargets: Array<TemplatePath> } {
    let navigatorTargets: Array<TemplatePath> = []
    let visibleNavigatorTargets: Array<TemplatePath> = []

    function walkAndAddKeys(path: InstancePath, collapsedAncestor: boolean): void {
      const {
        children,
        unfurledComponents,
      } = MetadataUtils.getAllChildrenIncludingUnfurledFocusedComponents(
        path,
        metadata,
        focusedElementPath,
      )
      const childrenIncludingFocusedElements = [...children, ...unfurledComponents]
      const reversedChildren = R.reverse(childrenIncludingFocusedElements)
      const isCollapsed = TP.containsPath(path, collapsedViews)
      navigatorTargets.push(path)
      if (!collapsedAncestor) {
        visibleNavigatorTargets.push(path)
      }

      fastForEach(reversedChildren, (childElement) => {
        walkAndAddKeys(childElement, collapsedAncestor || isCollapsed)
      })
    }

    const reverseCanvasRoots = MetadataUtils.getAllStoryboardChildren(metadata).reverse()
    fastForEach(reverseCanvasRoots, (root) => {
      if (MetadataUtils.elementIsScene(root)) {
        const rootScenePath = TP.scenePathForElementAtInstancePath(root.templatePath)
        const isCollapsed = TP.containsPath(rootScenePath, collapsedViews)
        navigatorTargets.push(rootScenePath)
        visibleNavigatorTargets.push(rootScenePath)
        fastForEach(root.rootElements, (rootElement) => {
          walkAndAddKeys(rootElement, isCollapsed)
        })
      } else {
        return walkAndAddKeys(root.templatePath, false)
      }
    })

    return {
      navigatorTargets: navigatorTargets,
      visibleNavigatorTargets: visibleNavigatorTargets,
    }
  },
  transformAtPathOptionally(
    elementMap: ElementInstanceMetadataMap,
    path: InstancePath,
    transform: (element: ElementInstanceMetadata) => ElementInstanceMetadata,
  ): ElementInstanceMetadataMap {
    const existing = MetadataUtils.getElementByInstancePathMaybe(elementMap, path)
    if (existing == null) {
      return elementMap
    } else {
      const transformed = transform(existing)
      if (transformed === existing) {
        return elementMap
      } else {
        return {
          ...elementMap,
          [TP.toString(path)]: transformed,
        }
      }
    }
  },
  getFrameInCanvasCoords(
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
  ): CanvasRectangle | null {
    const element = MetadataUtils.findElementByTemplatePath(metadata, path)
    return Utils.optionalMap((e) => e.globalFrame, element)
  },
  getFrame(path: TemplatePath, metadata: ElementInstanceMetadataMap): LocalRectangle | null {
    const element = MetadataUtils.findElementByTemplatePath(metadata, path)
    return Utils.optionalMap((e) => e.localFrame, element)
  },
  getFrameRelativeTo: function (
    parent: TemplatePath | null,
    metadata: ElementInstanceMetadataMap,
    frame: CanvasRectangle,
  ): LocalRectangle {
    if (parent == null) {
      return Utils.asLocal(frame)
    } else {
      const paths = TP.allPaths(parent)
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
  getElementLabel(
    path: TemplatePath,
    metadata: ElementInstanceMetadataMap,
    staticName: JSXElementName | null = null,
  ): string {
    const element = this.findElementByTemplatePath(metadata, path)
    if (element != null) {
      const sceneLabelOrComponentName = element.label ?? element.componentName
      const dataLabelProp = element.props['data-label']
      if (dataLabelProp != null && typeof dataLabelProp === 'string' && dataLabelProp.length > 0) {
        return dataLabelProp
      } else if (sceneLabelOrComponentName != null) {
        return sceneLabelOrComponentName
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
    path: TemplatePath,
    components: Array<UtopiaJSXComponent>,
    metadata: ElementInstanceMetadataMap,
  ): JSXElementName | null {
    if (TP.isScenePath(path)) {
      const scene = MetadataUtils.findElementByTemplatePath(metadata, path)
      if (scene != null && scene.componentName != null) {
        return jsxElementName(scene.componentName, [])
      } else {
        return null
      }
    } else {
      const jsxElement = findElementAtPath(path, components)
      if (jsxElement != null) {
        if (isJSXElement(jsxElement)) {
          return jsxElement.name
        } else {
          return null
        }
      }
      return null
    }
  },
  getJSXElementBaseName(
    path: TemplatePath,
    components: Array<UtopiaJSXComponent>,
    metadata: ElementInstanceMetadataMap,
  ): string | null {
    if (TP.isScenePath(path)) {
      const scene = MetadataUtils.findElementByTemplatePath(metadata, path)
      if (scene != null) {
        return scene.componentName
      } else {
        return null
      }
    } else {
      const jsxElement = findElementAtPath(path, components)
      if (jsxElement != null) {
        if (isJSXElement(jsxElement)) {
          return jsxElement.name.baseVariable
        } else {
          return null
        }
      }
      return null
    }
  },
  getJSXElementTagName(
    path: TemplatePath,
    components: Array<UtopiaJSXComponent>,
    metadata: ElementInstanceMetadataMap,
  ): string | null {
    if (TP.isScenePath(path)) {
      const scene = MetadataUtils.findElementByTemplatePath(metadata, path)
      if (scene != null) {
        return scene.componentName
      } else {
        return null
      }
    } else {
      const jsxElement = findElementAtPath(path, components)
      if (jsxElement != null) {
        if (isJSXElement(jsxElement)) {
          return getJSXElementNameAsString(jsxElement.name)
        } else {
          return null
        }
      }
      return null
    }
  },
  getTargetParentForPaste: function (
    imports: Imports,
    selectedViews: Array<TemplatePath>,
    metadata: ElementInstanceMetadataMap,
    pasteTargetsToIgnore: TemplatePath[],
  ): TemplatePath | null {
    if (selectedViews.length > 0) {
      const parentTarget = TP.getCommonParent(selectedViews, true)
      if (parentTarget == null) {
        return null
      } else {
        // we should not paste the source into itself
        const insertingSourceIntoItself = TP.containsPath(parentTarget, pasteTargetsToIgnore)

        if (TP.isScenePath(parentTarget)) {
          return insertingSourceIntoItself ? null : parentTarget
        } else if (
          this.targetSupportsChildren(imports, metadata, parentTarget) &&
          !insertingSourceIntoItself
        ) {
          return parentTarget
        } else {
          const parentOfSelected = TP.instancePathParent(parentTarget)
          if (TP.isScenePath(parentOfSelected)) {
            return parentOfSelected
          } else {
            if (this.targetSupportsChildren(imports, metadata, parentOfSelected)) {
              return parentOfSelected
            } else {
              return null
            }
          }
        }
      }
    } else {
      return null
    }
  },
  getDuplicationParentTargets(targets: TemplatePath[]): TemplatePath | null {
    return TP.getCommonParent(targets)
  },
  elementIsScene(element: ElementInstanceMetadata): boolean {
    return isLeft(element.element) && element.element.value === 'Scene'
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
    let newlyFoundElements: Array<InstancePath> = []
    fastForEach(fromDOM, (domElem) => {
      const spyElem = MetadataUtils.getElementByInstancePathMaybe(fromSpy, domElem.templatePath)

      // Checking if our elements support children should prevent us from ending up with the
      // internals of draft-js showing up underneath Text elements.
      const shouldNotTraverse = Utils.path(['props', 'data-utopia-do-not-traverse'], fromDOM)
      let children: Array<InstancePath>
      let rootElements: Array<InstancePath>
      if (shouldNotTraverse) {
        children = []
        rootElements = []
      } else {
        children = TP.addPathsIfMissing(spyElem?.children ?? [], domElem.children)
        rootElements = TP.addPathsIfMissing(spyElem?.rootElements ?? [], domElem.rootElements)
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
        workingElements[TP.toString(domElem.templatePath)] = elem
        newlyFoundElements.push(domElem.templatePath)
      } else {
        let componentInstance = spyElem.componentInstance || domElem.componentInstance
        let jsxElement = alternativeEither(spyElem.element, domElem.element)
        if (MetadataUtils.elementIsScene(spyElem)) {
          // We have some weird special casing for Scenes (see https://github.com/concrete-utopia/utopia/pull/671)
          jsxElement = spyElem.element
        } else {
          const elemUID: string | null = TP.toStaticUid(domElem.templatePath)
          const possibleElement = elementsByUID[elemUID]
          if (possibleElement != null) {
            if (!isIntrinsicElement(possibleElement.name)) {
              componentInstance = true
              jsxElement = right(possibleElement)
            }
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
          componentName: spyElem.componentName,
          label: spyElem.label,
        }
        workingElements[TP.toString(domElem.templatePath)] = elem
      }
    })

    return workingElements
  },
  staticElementsOnly(
    elements: Array<UtopiaJSXComponent>,
    targets: Array<TemplatePath>,
  ): Array<TemplatePath> {
    return targets.filter((target) => {
      const originType = this.getElementOriginType(elements, target)
      return originType === 'statically-defined' || originType === 'scene'
    })
  },
  removeElementMetadataChild(
    target: InstancePath,
    metadata: ElementInstanceMetadataMap,
  ): ElementInstanceMetadataMap {
    const parentPath = TP.parentPath(target)
    const targetID = TP.toTemplateId(target)
    // Remove it from where it used to be.
    if (TP.isScenePath(parentPath)) {
      // TODO Scene Implementation
      return metadata
    } else {
      let remainingElements: ElementInstanceMetadataMap = omit([TP.toString(target)], metadata)
      if (Object.keys(remainingElements).length === Object.keys(metadata).length) {
        // Nothing was removed
        return metadata
      }

      const updatedElements = MetadataUtils.transformAtPathOptionally(
        remainingElements,
        parentPath,
        (elem) => {
          const updatedChildren = elem.children.filter((child) => !TP.pathsEqual(child, target))
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
    }
  },
  updateParentWithNewChildPath(
    targetParent: TemplatePath | null,
    childPath: InstancePath,
    elements: ElementInstanceMetadataMap,
    indexPosition: IndexPosition | null,
  ): ElementInstanceMetadataMap {
    const makeE = () => {
      // TODO delete me
      throw new Error('Should not attempt to create empty elements.')
    }
    if (targetParent == null || TP.isScenePath(targetParent)) {
      // TODO Scene Implementation
      return elements
    } else {
      return this.transformAtPathOptionally(elements, targetParent, (parentElement) => {
        let updatedChildren: Array<InstancePath>
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
    targetParent: TemplatePath | null,
    elementToInsert: ElementInstanceMetadata,
    metadata: ElementInstanceMetadataMap,
    indexPosition: IndexPosition | null,
  ): ElementInstanceMetadataMap {
    // Insert into the map
    const withNewElement: ElementInstanceMetadataMap = {
      ...metadata,
      [TP.toString(elementToInsert.templatePath)]: elementToInsert,
    }

    // Update the parent
    const updatedElements = this.updateParentWithNewChildPath(
      targetParent,
      elementToInsert.templatePath,
      withNewElement,
      indexPosition,
    )
    return updatedElements
  },
  duplicateElementMetadataAtPath(
    oldPath: TemplatePath,
    newPath: TemplatePath,
    newElement: Either<string, JSXElementChild>,
    metadata: ElementInstanceMetadataMap,
  ): ElementInstanceMetadataMap {
    let workingElements = { ...metadata }

    function duplicateElementMetadata(
      element: ElementInstanceMetadata,
      pathToReplace: InstancePath,
      pathToReplaceWith: InstancePath,
      newElementInner: Either<string, JSXElementChild>,
    ): InstancePath {
      const newTemplatePath = TP.replaceIfAncestor(
        element.templatePath,
        pathToReplace,
        pathToReplaceWith,
      )

      const newElementMetadata: ElementInstanceMetadata = {
        ...element,
        templatePath: newTemplatePath,
        element: newElementInner,
        children: [], // all descendants have new UID-s
        rootElements: [], // all descendants have new UID-s
      }

      workingElements[TP.toString(newTemplatePath)] = newElementMetadata
      return newTemplatePath
    }

    // Everything about this feels wrong
    const originalMetadata = MetadataUtils.findElementByTemplatePath(metadata, oldPath)
    if (originalMetadata == null) {
      return metadata
    } else {
      const duplicatedElementPath = duplicateElementMetadata(
        originalMetadata,
        TP.instancePathForElementAtPath(oldPath),
        TP.instancePathForElementAtPath(newPath),
        newElement,
      )
      const updatedElements = this.updateParentWithNewChildPath(
        TP.parentPath(duplicatedElementPath),
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
    replaceSearch: TemplatePath,
    replaceWith: TemplatePath | null,
  ): ElementInstanceMetadataMap {
    let updatedElements: ElementInstanceMetadataMap = { ...metadata }

    const allPathsWithReplacements = Object.values(metadata)
      .map((e) => e.templatePath)
      .filter((path) => TP.isAncestorOf(path, replaceSearch, true))
      .map((path) => {
        const replacement = TP.replaceOrDefault(path, replaceSearch, replaceWith)
        return {
          path: path,
          replacement: replacement,
          pathString: TP.toString(path),
          replacementString: TP.toString(replacement),
        }
      })

    function updateChildren(children: InstancePath[]): InstancePath[] {
      let childWasUpdated = false
      const updatedChildren = children.map((child) => {
        const replacementChild = allPathsWithReplacements.find((pathWithReplacement) =>
          TP.pathsEqual(pathWithReplacement.path, child),
        )
        childWasUpdated = childWasUpdated && replacementChild != null
        return replacementChild == null ? child : replacementChild.replacement
      })

      return childWasUpdated ? updatedChildren : children
    }

    fastForEach(
      allPathsWithReplacements,
      ({ path, replacement, pathString, replacementString }) => {
        const existing = MetadataUtils.getElementByInstancePathMaybe(updatedElements, path)
        if (existing != null) {
          delete updatedElements[pathString]
          updatedElements[replacementString] = {
            ...existing,
            templatePath: replacement,
            children: updateChildren(existing.children),
            rootElements: updateChildren(existing.rootElements),
          }
        }
      },
    )

    return updatedElements
  },
  findElementMetadata(
    target: TemplatePath,
    elements: ReadonlyArray<ElementInstanceMetadata>,
  ): ElementInstanceMetadata | null {
    const pathToUse = TP.isScenePath(target) ? TP.instancePathForElementAtScenePath(target) : target
    return elements.find((elem) => TP.pathsEqual(pathToUse, elem.templatePath)) ?? null
  },
  getStaticElementName(
    path: TemplatePath,
    rootElements: Array<UtopiaJSXComponent>,
  ): JSXElementName | null {
    if (TP.isScenePath(path)) {
      return null
    } else {
      // TODO remove dependency on metadata from here
      const staticPath = MetadataUtils.dynamicPathToStaticPath(path)
      const jsxElement = optionalMap((p) => findJSXElementChildAtPath(rootElements, p), staticPath)
      return optionalMap((element) => (isJSXElement(element) ? element.name : null), jsxElement)
    }
  },
  isComponentInstance(
    path: TemplatePath,
    rootElements: Array<UtopiaJSXComponent>,
    metadata: ElementInstanceMetadataMap,
    imports: Imports,
  ): boolean {
    if (TP.isScenePath(path)) {
      return false
    } else {
      const elementName = MetadataUtils.getStaticElementName(path, rootElements)
      const instanceMetadata = MetadataUtils.getElementByInstancePathMaybe(metadata, path)
      return (
        elementName != null &&
        instanceMetadata != null &&
        !MetadataUtils.isGivenUtopiaAPIElementFromImports(
          imports,
          instanceMetadata,
          getJSXElementNameLastPart(elementName),
        ) &&
        !isIntrinsicElement(elementName)
      )
    }
  },
  isPinnedAndNotAbsolutePositioned(
    metadata: ElementInstanceMetadataMap,
    view: TemplatePath,
  ): boolean {
    // Disable snapping and guidelines for pinned elements marked with relative positioning:
    if (TP.isInstancePath(view)) {
      const elementMetadata = MetadataUtils.getElementByInstancePathMaybe(metadata, view)
      if (
        elementMetadata != null &&
        elementMetadata.specialSizeMeasurements.parentLayoutSystem === 'flow' &&
        !MetadataUtils.isPositionAbsolute(elementMetadata)
      ) {
        return true
      }
    }
    return false
  },
  walkMetadata(
    metadata: ElementInstanceMetadataMap,
    withEachElement: (
      element: ElementInstanceMetadata,
      parentMetadata: ElementInstanceMetadata | null,
    ) => void,
  ): void {
    fastForEach(Object.values(metadata), (elem) => {
      const parentPath = TP.parentPath(elem.templatePath)
      const parent = TP.isInstancePath(parentPath)
        ? this.getElementByInstancePathMaybe(metadata, parentPath)
        : null
      withEachElement(elem, parent)
    })
  },
  findContainingBlock(
    elementMap: ElementInstanceMetadataMap,
    path: TemplatePath,
  ): TemplatePath | null {
    const specialSizeMeasurements = TP.isInstancePath(path)
      ? this.getElementByInstancePathMaybe(elementMap, path)?.specialSizeMeasurements
      : null
    const parentPath = TP.parentPath(path)
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
    path: TemplatePath,
  ): TemplatePath | null {
    const parentPath = TP.parentPath(path)
    const specialSizeMeasurements = TP.isInstancePath(path)
      ? this.getElementByInstancePathMaybe(elementMap, path)?.specialSizeMeasurements
      : null
    const parentSizeMeasurements =
      parentPath != null && TP.isInstancePath(parentPath)
        ? this.getElementByInstancePathMaybe(elementMap, parentPath)?.specialSizeMeasurements
        : null
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
    path: TemplatePath,
    components: UtopiaJSXComponent[],
    metadata: ElementInstanceMetadataMap,
    imports: Imports,
  ): boolean {
    const elementName = MetadataUtils.getJSXElementName(path, components, metadata)
    const elementInstancePath = TP.isInstancePath(path)
      ? path
      : TP.instancePathForElementAtScenePath(path)
    const element = MetadataUtils.getElementByInstancePathMaybe(metadata, elementInstancePath)
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
}

export function findElementAtPath(
  target: TemplatePath | null,
  components: Array<UtopiaJSXComponent>,
): JSXElementChild | null {
  if (target == null) {
    return null
  } else {
    if (TP.isScenePath(target)) {
      return null
    } else {
      const staticTarget = MetadataUtils.dynamicPathToStaticPath(target)
      if (staticTarget == null) {
        return null
      } else {
        return findJSXElementChildAtPath(components, staticTarget)
      }
    }
  }
}

export function findJSXElementAtPath(
  target: TemplatePath | null,
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
  target: TemplatePath,
  metadata: ElementInstanceMetadataMap,
): PropsOrJSXAttributes | null {
  const targetMetadata = MetadataUtils.findElementByTemplatePath(metadata, target)
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
