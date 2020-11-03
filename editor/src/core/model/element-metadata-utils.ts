import * as OPI from 'object-path-immutable'
import * as R from 'ramda'
import { FlexLength, LayoutSystem, Sides } from 'utopia-api'
import { getReorderDirection } from '../../components/canvas/controls/select-mode/yoga-utils'
import { getImageSize, scaleImageDimensions } from '../../components/images'
import { foldThese, makeThat, makeThis, mergeThese, setThat, These } from '../../utils/these'
import Utils, { IndexPosition } from '../../utils/utils'
import { getLayoutProperty } from '../layout/getLayoutProperty'
import { FlexLayoutHelpers, LayoutHelpers } from '../layout/layout-helpers'
import { LayoutProp } from '../layout/layout-helpers-new'
import { flattenArray, mapDropNulls, pluck, stripNulls, flatMapArray } from '../shared/array-utils'
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
  ComponentMetadata,
  ComponentMetadataWithoutRootElements,
  ElementInstanceMetadata,
  ElementsByUID,
  getJSXElementNameLastPart,
  getJSXElementNameNoPathName,
  isComponentMetadata,
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXTextBlock,
  JSXAttributes,
  JSXElement,
  JSXElementChild,
  MetadataWithoutChildren,
  UtopiaJSXComponent,
  JSXElementName,
  getJSXElementNameAsString,
  isIntrinsicElement,
  jsxElementName,
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
import { isGivenUtopiaAPIElement, isUtopiaAPIComponent } from './project-file-utils'
import { EmptyScenePathForStoryboard } from './scene-utils'
import { fastForEach } from '../shared/utils'
import { UTOPIA_ORIGINAL_ID_KEY, UTOPIA_UID_KEY } from './utopia-constants'
import { extractOriginalUidFromIndexedUid } from '../shared/uid-utils'
const ObjectPathImmutable: any = OPI

type MergeCandidate = These<ElementInstanceMetadata, ElementInstanceMetadata>

function fromSpyMergeCandidate(fromSpy: ElementInstanceMetadata): MergeCandidate {
  return makeThis(fromSpy)
}

function fromDOMMergeCandidate(fromDOM: ElementInstanceMetadata): MergeCandidate {
  return makeThat(fromDOM)
}

function produceMetadataFromMergeCandidate(
  elementsByUID: ElementsByUID,
  mergeCandidate: MergeCandidate,
): ElementInstanceMetadata {
  const mergedMetadata = mergeThese((fromSpy, fromDOM) => {
    // Checking if our elements support children should prevent us from ending up with the
    // internals of draft-js showing up underneath Text elements.
    const shouldNotTraverse = Utils.path(['props', 'data-utopia-do-not-traverse'], fromDOM)
    let children: Array<ElementInstanceMetadata>
    if (shouldNotTraverse) {
      children = []
    } else {
      children = mergeElementMetadata(elementsByUID, fromSpy.children, fromDOM.children)
    }

    // The actual merge case.
    return {
      ...fromDOM,
      props: fromSpy.props,
      element: alternativeEither(fromSpy.element, fromDOM.element),
      children: children,
      componentInstance: fromSpy.componentInstance || fromDOM.componentInstance,
    }
  }, mergeCandidate)

  const possibleUID: string | null | undefined = Utils.defaultIfNull(
    mergedMetadata.props[UTOPIA_UID_KEY],
    mergedMetadata.props[UTOPIA_ORIGINAL_ID_KEY],
  )
  if (possibleUID == null) {
    return mergedMetadata
  } else {
    const possibleElement = elementsByUID[possibleUID]
    if (possibleElement == null) {
      return mergedMetadata
    } else {
      if (isIntrinsicElement(possibleElement.name)) {
        return mergedMetadata
      } else {
        return {
          ...mergedMetadata,
          componentInstance: true,
          element: right(possibleElement),
        }
      }
    }
  }
}

function mergeElementMetadata(
  elementsByUID: ElementsByUID,
  fromSpy: Array<ElementInstanceMetadata>,
  fromDOM: Array<ElementInstanceMetadata>,
): Array<ElementInstanceMetadata> {
  // This logic effectively puts everything from the spy first,
  // then anything missed out from the DOM right after it.
  // Ideally this would function like a VCS diff inserting runs of new elements
  // inbetween matching metadata, so it may be necessary to implement something
  // like that in the future. But for now this is likely "good enough" that it
  // wont make any difference.
  let mergeCandidates: Array<MergeCandidate> = fromSpy.map(fromSpyMergeCandidate)
  Utils.fastForEach(fromDOM, (fromDOMElement) => {
    const potentialMergeCandidateIndex = mergeCandidates.findIndex((candidate) => {
      return foldThese(
        (thisValue) => TP.pathsEqual(thisValue.templatePath, fromDOMElement.templatePath),
        (_) => false,
        (thisValue, _) => TP.pathsEqual(thisValue.templatePath, fromDOMElement.templatePath),
        candidate,
      )
    })
    if (potentialMergeCandidateIndex === -1) {
      mergeCandidates.push(fromDOMMergeCandidate(fromDOMElement))
    } else {
      const candidate = mergeCandidates[potentialMergeCandidateIndex]
      mergeCandidates[potentialMergeCandidateIndex] = setThat(fromDOMElement, candidate)
    }
  })
  return mergeCandidates.map((candidate) => {
    return produceMetadataFromMergeCandidate(elementsByUID, candidate)
  })
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
  elementInstanceMetadataGetChildren(
    element: ElementInstanceMetadata,
  ): Array<ElementInstanceMetadata> | null {
    return element.children
  },
  getElementByInstancePathMaybe(
    scenes: ComponentMetadata[],
    path: InstancePath | null,
  ): ElementInstanceMetadata | null {
    if (path == null) {
      return null
    }

    const scene = MetadataUtils.findSceneByTemplatePath(scenes, path)
    if (scene == null) {
      return null
    } else {
      const rootElementPaths = scene.rootElements
      return TP.findAtElementPath(
        rootElementPaths,
        path.element,
        MetadataUtils.elementInstanceMetadataGetChildren,
        getUtopiaID,
      )
    }
  },
  findSceneByTemplatePath(
    scenes: ComponentMetadata[],
    path: TemplatePath,
  ): ComponentMetadata | null {
    const scenePath = TP.scenePathForPath(path)
    return scenes.find((s) => TP.pathsEqual(s.scenePath, scenePath)) ?? null
  },
  isSceneTreatedAsGroup(scene: ComponentMetadata | null): boolean {
    return scene?.sceneResizesContent ?? false
  },
  getElementInstanceMetadataAlongPath(
    components: Array<ComponentMetadata>,
    target: InstancePath,
  ): Array<ElementInstanceMetadata> | null {
    const scene = MetadataUtils.findSceneByTemplatePath(components, target)
    if (scene == null || scene.rootElements == null) {
      return null
    } else {
      let result: Array<ElementInstanceMetadata> = []
      function findElement(element: ElementInstanceMetadata): 'DONE' | 'BAIL' | 'CONTINUE' {
        if (TP.pathsEqual(element.templatePath, target)) {
          result.push(element)
          return 'DONE'
        } else if (TP.isAncestorOf(target, element.templatePath)) {
          result.push(element)
          for (const child of element.children) {
            const childResult = findElement(child)
            if (childResult === 'CONTINUE') {
              continue
            } else {
              return childResult
            }
          }
          return 'BAIL'
        } else {
          return 'CONTINUE'
        }
      }
      for (const rootElement of scene.rootElements) {
        const findResult = findElement(rootElement)
        if (findResult !== 'CONTINUE') {
          break
        }
      }
      return result
    }
  },
  findElements(
    rootElements: ReadonlyArray<ComponentMetadata>,
    predicate: (element: ElementInstanceMetadata) => boolean,
  ): Array<ElementInstanceMetadata> {
    let elementsToCheck = flatMapArray((e) => e.rootElements, rootElements)
    let elements: Array<ElementInstanceMetadata> = []
    let element: ElementInstanceMetadata | undefined = undefined
    while ((element = elementsToCheck.shift()) != null) {
      if (predicate(element)) {
        elements.push(element)
      }
      const children = MetadataUtils.elementInstanceMetadataGetChildren(element)
      if (children != null) {
        elementsToCheck.push(...children)
      }
    }
    return elements
  },
  getViewZIndexFromMetadata(scenes: Array<ComponentMetadata>, target: TemplatePath): number {
    if (TP.isScenePath(target)) {
      return scenes.findIndex((s) => TP.pathsEqual(s.scenePath, target))
    } else {
      const siblings = MetadataUtils.getSiblings(scenes, target)
      return siblings.findIndex((child) => {
        return getUtopiaID(child) === TP.toTemplateId(target)
      })
    }
  },
  getParent(
    scenes: ComponentMetadata[],
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
      return this.getElementByInstancePathMaybe(scenes, parentPath)
    }
  },
  getSiblings(scenes: ComponentMetadata[], target: TemplatePath | null): ElementInstanceMetadata[] {
    if (target == null) {
      return []
    }

    const parentPath = TP.parentPath(target)
    if (parentPath == null) {
      return []
    } else if (TP.isScenePath(parentPath)) {
      // really this is overkill, since the only "sibling" is itself, but I'm keeping this here so TS
      // can flag it when we support multiple root elements on a component
      const rootElementPaths =
        MetadataUtils.findSceneByTemplatePath(scenes, target)?.rootElements ?? []
      return rootElementPaths
    } else {
      const parent = MetadataUtils.getElementByInstancePathMaybe(scenes, parentPath)
      return parent == null ? [] : parent.children
    }
  },
  isParentYogaLayoutedContainerAndElementParticipatesInLayout(
    path: TemplatePath,
    scenes: ComponentMetadata[],
  ): boolean {
    if (TP.isScenePath(path)) {
      // TODO Scene Implementation
      return false
    } else {
      const instance = this.getElementByInstancePathMaybe(scenes, path)
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
  isGroup(path: TemplatePath | null, scenes: ComponentMetadata[]): boolean {
    if (path == null) {
      return false
    } else if (TP.isScenePath(path)) {
      // TODO Scene Implementation
      return false
    } else {
      const instance = this.getElementByInstancePathMaybe(scenes, path)
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
  isPositionAbsolute(instance: ElementInstanceMetadata | null): boolean {
    return instance?.specialSizeMeasurements.position === 'absolute'
  },
  getYogaSizeProps(
    target: TemplatePath,
    scenes: Array<ComponentMetadata>,
    components: Array<UtopiaJSXComponent>,
  ): Partial<Size> {
    const parentInstance = this.getParent(scenes, target)
    if (parentInstance == null) {
      return {}
    } else {
      const flexDirection = getReorderDirection(this.getYogaDirection(parentInstance))

      if (TP.isInstancePath(target)) {
        const staticTarget = this.dynamicPathToStaticPath(target)
        if (staticTarget == null) {
          return {}
        } else {
          const element = findJSXElementChildAtPath(components, staticTarget)
          if (element != null && isJSXElement(element)) {
            const widthLookupAxis: LayoutProp =
              flexDirection === 'horizontal' ? 'FlexFlexBasis' : 'FlexCrossBasis'
            const heightLookupAxis: LayoutProp =
              flexDirection === 'vertical' ? 'FlexFlexBasis' : 'FlexCrossBasis'
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
  getElementMargin(path: TemplatePath, components: ComponentMetadata[]): Partial<Sides> | null {
    if (TP.isInstancePath(path)) {
      const instance = MetadataUtils.getElementByInstancePathMaybe(components, path)
      if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
        return instance.specialSizeMeasurements.margin
      } else {
        return null
      }
    } else {
      return null
    }
  },
  getElementPadding(path: TemplatePath, components: ComponentMetadata[]): Partial<Sides> | null {
    if (TP.isInstancePath(path)) {
      const instance = MetadataUtils.getElementByInstancePathMaybe(components, path)
      if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
        return instance.specialSizeMeasurements.padding
      } else {
        return null
      }
    } else {
      return null
    }
  },
  getYogaDirection: function (
    instance: ElementInstanceMetadata | null,
  ): 'row' | 'row-reverse' | 'column' | 'column-reverse' {
    if (instance != null && isRight(instance.element) && isJSXElement(instance.element.value)) {
      return FlexLayoutHelpers.getFlexDirectionFromProps(instance.element.value.props)
    } else {
      return 'row' // TODO read this value from spy
    }
  },
  getYogaDirectionAtPath(
    path: TemplatePath | null,
    scenes: ComponentMetadata[],
  ): 'row' | 'row-reverse' | 'column' | 'column-reverse' {
    // TODO Scene Implementation
    const instance =
      path == null || TP.isScenePath(path) ? null : this.getElementByInstancePathMaybe(scenes, path)
    return this.getYogaDirection(instance)
  },
  getYogaWrap: function (
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
    components: ComponentMetadata[],
    target: TemplatePath | null,
  ): boolean {
    if (target == null || TP.isScenePath(target)) {
      return false
    }
    const instance = this.getElementByInstancePathMaybe(components, target)
    return this.isAutoSizingView(instance)
  },
  isAutoSizingText(imports: Imports, instance: ElementInstanceMetadata): boolean {
    return this.isTextAgainstImports(imports, instance) && instance.props.textSizing === 'auto'
  },
  findNonGroupParent(
    componentsMetadata: Array<ComponentMetadata>,
    target: TemplatePath,
  ): TemplatePath | null {
    const parentPath = TP.parentPath(target)

    if (parentPath == null) {
      return null
    } else if (TP.isScenePath(parentPath)) {
      // we've reached the top
      return parentPath
    } else {
      const parent = MetadataUtils.getElementByInstancePathMaybe(componentsMetadata, parentPath)
      if (MetadataUtils.isAutoSizingView(parent)) {
        return MetadataUtils.findNonGroupParent(componentsMetadata, parentPath)
      } else {
        return parentPath
      }
    }
  },
  templatePathToStaticTemplatePath(path: TemplatePath | null): StaticTemplatePath | null {
    if (path == null || TP.isScenePath(path)) {
      return path
    } else {
      return this.dynamicPathToStaticPath(path)
    }
  },
  dynamicPathToStaticPath(path: InstancePath): StaticInstancePath | null {
    return TP.staticInstancePath(path.scene, path.element.map(extractOriginalUidFromIndexedUid))
  },
  shiftGroupFrame(
    componentsMetadata: Array<ComponentMetadata>,
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
      const ancestorElement = MetadataUtils.getElementByInstancePathMaybe(
        componentsMetadata,
        ancestorPath,
      )

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
    scenes: Array<ComponentMetadata>,
    target: InstancePath,
    property: PropertyPath,
    value: any,
  ): Array<ComponentMetadata> {
    return this.transformAtPathOptionally(scenes, target, (element) => {
      return {
        ...element,
        props: ObjectPathImmutable.set(element.props, PP.getElements(property), value),
      }
    }).elements
  },
  unsetPropertyDirectlyIntoMetadata(
    scenes: Array<ComponentMetadata>,
    target: InstancePath,
    property: PropertyPath,
  ): Array<ComponentMetadata> {
    return this.transformAtPathOptionally(scenes, target, (element) => {
      return {
        ...element,
        props: ObjectPathImmutable.del(element.props, PP.getElements(property)),
      }
    }).elements
  },
  getImmediateChildren(
    scenes: ComponentMetadata[],
    target: TemplatePath,
  ): Array<ElementInstanceMetadata> {
    if (TP.isScenePath(target)) {
      const scene = MetadataUtils.findSceneByTemplatePath(scenes, target)
      return scene?.rootElements ?? []
    } else {
      const element = MetadataUtils.getElementByInstancePathMaybe(scenes, target)
      return element?.children ?? []
    }
  },
  getChildrenHandlingGroups(
    scenes: ComponentMetadata[],
    target: TemplatePath,
    includeGroups: boolean,
  ): Array<ElementInstanceMetadata> {
    const immediateChildren = MetadataUtils.getImmediateChildren(scenes, target)

    const getChildrenInner = (
      childInstance: ElementInstanceMetadata,
    ): Array<ElementInstanceMetadata> => {
      // autoSizing views are the new groups
      if (this.isAutoSizingViewFromComponents(scenes, childInstance.templatePath)) {
        const children = Utils.flatMapArray(getChildrenInner, childInstance.children)
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
  getAllScenePaths(scenes: ComponentMetadata[]): ScenePath[] {
    return scenes
      .map((s) => s.scenePath)
      .filter((s) => !TP.pathsEqual(s, EmptyScenePathForStoryboard))
  },
  getCanvasRootScenesAndElements(
    scenes: ComponentMetadata[],
  ): Array<ComponentMetadata | ElementInstanceMetadata> {
    // The spy metadata has a new special scene (its scene path is _empty array_ 🤯)
    // which represents the "Storyboard" element (nee Canvas Metadata).
    // We want to show the scene- and non-scene-children of Storyboard as the root elements
    // Scenes are already the roots in this `scenes` array, we want to take the non-scene
    // children of Storyboard and put them right next to the scene roots as siblings.
    // We want to filter out the Storyboard element itself, and hide the "Storyboard Scene".
    // We also want to show the scene and non-scene siblings in their original order, not the order determined by the spy

    const storyboardRoot = this.findStoryboardRoot(scenes)
    if (storyboardRoot == null) {
      return []
    } else {
      const rootChildrenOfStoryboard = flatMapArray((e) => e.children, storyboardRoot.rootElements)
      return rootChildrenOfStoryboard.map((child) => {
        if (isLeft(child.element) && child.element.value === 'Scene') {
          const foundScenePath = TP.scenePath(child.templatePath.element)
          const realSceneRooot = scenes.find((scene) =>
            TP.pathsEqual(scene.scenePath, foundScenePath),
          )
          if (realSceneRooot != null) {
            return realSceneRooot
          }
        }
        return child
      })
    }
  },
  getAllCanvasRootPaths(scenes: ComponentMetadata[]): TemplatePath[] {
    const rootScenesAndElements = this.getCanvasRootScenesAndElements(scenes)
    return flatMapArray<ElementInstanceMetadata | ComponentMetadata, TemplatePath>((root) => {
      if (isComponentMetadata(root)) {
        if (root.rootElements != null) {
          return root.rootElements.map((e) => e.templatePath)
        } else {
          return [root.scenePath]
        }
      } else {
        return [root.templatePath]
      }
    }, rootScenesAndElements)
  },
  getAllPaths(scenes: ComponentMetadata[]): TemplatePath[] {
    let result: Array<TemplatePath> = []
    function recurseElement(element: ElementInstanceMetadata): void {
      result.push(element.templatePath)
      fastForEach(element.children, recurseElement)
    }

    const scenePaths = this.getAllScenePaths(scenes)
    fastForEach(scenePaths, (scenePath) => {
      const scene = scenes.find((s) => TP.pathsEqual(scenePath, s.scenePath))
      if (scene != null) {
        result.push(scenePath)
        scene.rootElements.forEach(recurseElement)
      }
    })

    return result
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
  isButton(instance: ElementInstanceMetadata): boolean {
    return this.isElementOfType(instance, 'button')
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
    scenes: ComponentMetadata[],
    target: TemplatePath,
  ): boolean {
    if (TP.isScenePath(target)) {
      return true
    } else {
      const instance = this.getElementByInstancePathMaybe(scenes, target)
      return instance == null ? false : this.targetElementSupportsChildren(imports, instance)
    }
  },
  // TODO update this to work with the natural width / height
  getImageMultiplier(
    imports: Imports,
    componentMetadata: ComponentMetadata[],
    targets: Array<TemplatePath>,
  ): number | null {
    const multipliers: Set<number> = Utils.emptySet()
    Utils.fastForEach(targets, (target) => {
      if (TP.isScenePath(target)) {
        return
      }
      const instance = this.getElementByInstancePathMaybe(componentMetadata, target)
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
  findStoryboardRoot(roots: Array<ComponentMetadata>): ComponentMetadata | null {
    return roots.find((root) => TP.pathsEqual(root.scenePath, EmptyScenePathForStoryboard)) ?? null
  },
  createOrderedTemplatePathsFromElements(scenes: Array<ComponentMetadata>): Array<TemplatePath> {
    function getKeysOfElementAndDescendants(element: ElementInstanceMetadata): Array<TemplatePath> {
      const path = element.templatePath
      return [
        path,
        ...Utils.flatMapArray<ElementInstanceMetadata, TemplatePath>((elem) => {
          return getKeysOfElementAndDescendants(elem)
        }, R.reverse(element.children)),
      ]
    }
    // TODO possibly replace this with a reduceRight
    return flattenArray(
      this.getCanvasRootScenesAndElements(scenes)
        .reverse()
        .map((root) => {
          if (isComponentMetadata(root)) {
            const childrenPathsOfRoot = flatMapArray(
              (e) => getKeysOfElementAndDescendants(e),
              root.rootElements,
            )
            return [root.scenePath, ...childrenPathsOfRoot]
          } else {
            return getKeysOfElementAndDescendants(root)
          }
        }),
    )
  },
  transformAtPathOptionally(
    components: Array<ComponentMetadata>,
    path: InstancePath,
    transform: (element: ElementInstanceMetadata) => ElementInstanceMetadata,
  ): TP.ElementsTransformResult<ComponentMetadata> {
    const scenePath = TP.scenePathForPath(path)
    const sceneIndex = components.findIndex((c) => TP.pathsEqual(c.scenePath, scenePath))
    const scene = components[sceneIndex]
    const transformResult = TP.findAndTransformAtPath(
      scene.rootElements,
      TP.elementPathForPath(path),
      MetadataUtils.elementInstanceMetadataGetChildren,
      getUtopiaID,
      transform,
    )

    const updatedScene: ComponentMetadata = {
      ...scene,
      rootElements: transformResult.elements,
    }

    return {
      elements: R.update(sceneIndex, updatedScene, components),
      transformedElement: updatedScene,
    }
  },
  getSceneFrame(path: ScenePath, scenes: Array<ComponentMetadata>): CanvasRectangle | null {
    const maybeScene = MetadataUtils.findSceneByTemplatePath(scenes, path)
    return maybeScene?.globalFrame ?? null
  },
  getFrameInCanvasCoords(
    path: TemplatePath,
    metadata: Array<ComponentMetadata>,
  ): CanvasRectangle | null {
    if (TP.isScenePath(path)) {
      return this.getSceneFrame(path, metadata)
    } else {
      const element = MetadataUtils.getElementByInstancePathMaybe(metadata, path)
      return Utils.optionalMap((e) => e.globalFrame, element)
    }
  },
  getFrame(path: TemplatePath, metadata: Array<ComponentMetadata>): LocalRectangle | null {
    if (TP.isScenePath(path)) {
      const frame = this.getSceneFrame(path, metadata)
      return localRectangle(frame)
    } else {
      const element = MetadataUtils.getElementByInstancePathMaybe(metadata, path)
      return Utils.optionalMap((e) => e.localFrame, element)
    }
  },
  getFrameRelativeTo: function (
    parent: TemplatePath | null,
    metadata: Array<ComponentMetadata>,
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
    metadata: Array<ComponentMetadata>,
    staticName: JSXElementName | null = null,
  ): string {
    if (TP.isScenePath(path)) {
      const scene = this.findSceneByTemplatePath(metadata, path)
      if (scene != null) {
        return scene.label ?? scene.component ?? 'Scene'
      }
    } else {
      // Try to get something from the metadata.
      const element = this.getElementByInstancePathMaybe(metadata, path)
      if (element != null) {
        const dataLabelProp = element.props['data-label']
        if (
          dataLabelProp != null &&
          typeof dataLabelProp === 'string' &&
          dataLabelProp.length > 0
        ) {
          return dataLabelProp
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
    }

    // Default catch all name, will probably avoid some odd cases in the future.
    return 'Element'
  },
  getJSXElementName(
    path: TemplatePath,
    components: Array<UtopiaJSXComponent>,
    metadata: ComponentMetadata[], // TODO maybe we could remove metadata as a dependency from here if we change findSceneByTemplatePath
  ): JSXElementName | null {
    if (TP.isScenePath(path)) {
      const scene = MetadataUtils.findSceneByTemplatePath(metadata, path)
      if (scene != null && scene.component != null) {
        return jsxElementName(scene.component, [])
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
    metadata: ComponentMetadata[], // TODO BEFORE MERGE maybe we could remove metadata as a dependency from here
  ): string | null {
    if (TP.isScenePath(path)) {
      const scene = MetadataUtils.findSceneByTemplatePath(metadata, path)
      if (scene != null) {
        return scene.component
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
    metadata: ComponentMetadata[], // TODO BEFORE MERGE maybe we could remove metadata as a dependency from here
  ): string | null {
    if (TP.isScenePath(path)) {
      const scene = MetadataUtils.findSceneByTemplatePath(metadata, path)
      if (scene != null) {
        return scene.component
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
    scenes: Array<ComponentMetadata>,
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
          this.targetSupportsChildren(imports, scenes, parentTarget) &&
          !insertingSourceIntoItself
        ) {
          return parentTarget
        } else {
          const parentOfSelected = TP.instancePathParent(parentTarget)
          if (TP.isScenePath(parentOfSelected)) {
            return parentOfSelected
          } else {
            if (this.targetSupportsChildren(imports, scenes, parentOfSelected)) {
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
  mergeComponentMetadata(
    elementsByUID: ElementsByUID,
    fromSpy: Array<ComponentMetadata>,
    fromDOM: Array<ElementInstanceMetadata>,
  ): Array<ComponentMetadata> {
    const rootElements = flatMapArray((s) => s.rootElements, fromSpy)
    const mergedInstanceMetadata = mergeElementMetadata(elementsByUID, rootElements, fromDOM)

    return fromSpy.map((scene) => {
      const newRootElements = mergedInstanceMetadata.filter((m) =>
        TP.isChildOf(m.templatePath, scene.scenePath),
      )
      const sceneMetadata = mergedInstanceMetadata.find((m) =>
        TP.pathsEqual(m.templatePath, scene.templatePath),
      )
      return {
        ...scene,
        globalFrame: sceneMetadata?.globalFrame ?? null,
        rootElements: newRootElements,
      }
    })
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
    scenes: Array<ComponentMetadata>,
  ): Array<ComponentMetadata> {
    const parentPath = TP.parentPath(target)
    const targetID = TP.toTemplateId(target)
    // Remove it from where it used to be.
    if (TP.isScenePath(parentPath)) {
      // TODO Scene Implementation
      return scenes
    } else {
      return this.transformAtPathOptionally(scenes, parentPath, (parentElement) => {
        return {
          ...parentElement,
          children: parentElement.children.filter((child) => getUtopiaID(child) != targetID),
        }
      }).elements
    }
  },
  insertElementMetadataChild(
    targetParent: TemplatePath | null,
    elementToInsert: ElementInstanceMetadata,
    components: Array<ComponentMetadata>,
    indexPosition: IndexPosition | null,
  ): Array<ComponentMetadata> {
    const makeE = () => {
      // TODO delete me
      throw new Error('Should not attempt to create empty elements.')
    }
    if (targetParent == null || TP.isScenePath(targetParent)) {
      // TODO Scene Implementation
      return components
    } else {
      const transformResult = this.transformAtPathOptionally(
        components,
        targetParent,
        (parentElement) => {
          let updatedChildren: Array<ElementInstanceMetadata>
          if (indexPosition == null) {
            updatedChildren = parentElement.children.concat(elementToInsert)
          } else {
            updatedChildren = Utils.addToArrayWithFill(
              elementToInsert,
              parentElement.children,
              indexPosition,
              makeE,
            )
          }
          return {
            ...parentElement,
            children: updatedChildren,
          }
        },
      )
      return transformResult.elements
    }
  },
  duplicateElementMetadata(
    element: ElementInstanceMetadata,
    pathToReplace: InstancePath,
    pathToReplaceWith: InstancePath,
    newElement: Either<string, JSXElementChild>,
    scenes: Array<ComponentMetadata>,
  ): ElementInstanceMetadata {
    // Everything about this feels wrong
    const duplicatedChildren = element.children.map((child) => {
      const childsElement = child.element
      let duplicatedElement: Either<string, JSXElementChild>
      if (isLeft(childsElement) || isLeft(newElement)) {
        duplicatedElement = childsElement
      } else {
        const childElementUID = getUtopiaID(childsElement.value)
        duplicatedElement = isJSXElement(newElement.value)
          ? right(newElement.value.children.find((c) => getUtopiaID(c) === childElementUID)!)
          : childsElement
      }
      return this.duplicateElementMetadata(
        child,
        pathToReplace,
        pathToReplaceWith,
        duplicatedElement,
        scenes,
      )
    })
    const newTemplatePath = TP.replaceIfAncestor(
      element.templatePath,
      pathToReplace,
      pathToReplaceWith,
    )
    return {
      ...element,
      templatePath: newTemplatePath,
      element: newElement,
      children: duplicatedChildren,
    }
  },
  duplicateElementMetadataAtPath(
    oldPath: TemplatePath,
    newPath: TemplatePath,
    newElement: Either<string, JSXElementChild>,
    scenes: Array<ComponentMetadata>,
  ): Array<ComponentMetadata> {
    // Everything about this feels wrong
    const originalMetadata = getSceneMetadataOrElementInstanceMetadata(oldPath, scenes)
    if (originalMetadata == null) {
      return scenes
    } else if (isLeft(originalMetadata) && TP.isScenePath(newPath)) {
      const componentMetadata = originalMetadata.value
      return [
        ...scenes,
        {
          ...componentMetadata,
          scenePath: newPath,
        },
      ]
    } else if (
      isRight(originalMetadata) &&
      TP.isInstancePath(oldPath) &&
      TP.isInstancePath(newPath)
    ) {
      const duplicatedMetadata = this.duplicateElementMetadata(
        originalMetadata.value,
        oldPath,
        newPath,
        newElement,
        scenes,
      )
      return this.insertElementMetadataChild(TP.parentPath(newPath), duplicatedMetadata, scenes, {
        type: 'back',
      })
    } else {
      return scenes
    }
  },
  transformAllMetadata(
    scenes: Array<ComponentMetadata>,
    transform: (metadata: ElementInstanceMetadata) => ElementInstanceMetadata,
  ): Array<ComponentMetadata> {
    function innerTransform(metadata: ElementInstanceMetadata): ElementInstanceMetadata {
      const updatedChildren = metadata.children.map(innerTransform)
      return transform({
        ...metadata,
        children: updatedChildren,
      })
    }

    return scenes.map((scene) => {
      return {
        ...scene,
        rootElements: scene.rootElements.map(innerTransform),
      }
    })
  },
  removeElementMetadata(
    target: TemplatePath,
    rootElements: Array<ElementInstanceMetadata>,
  ): Array<ElementInstanceMetadata> {
    const parentPath = TP.parentPath(target) ?? target
    const transform = (t: ElementInstanceMetadata): ElementInstanceMetadata => {
      return {
        ...t,
        children: [],
      }
    }
    if (TP.isScenePath(parentPath)) {
      const parentInstancePath = TP.instancePath([], TP.elementPathForPath(parentPath))
      return rootElements.filter((elem) => !TP.pathsEqual(parentInstancePath, elem.templatePath))
    } else {
      return TP.findAndTransformAtPath(
        rootElements,
        TP.elementPathForPath(parentPath),
        MetadataUtils.elementInstanceMetadataGetChildren,
        getUtopiaID,
        transform,
      ).elements
    }
  },
  findElementMetadata(
    target: TemplatePath,
    rootElements: Array<ElementInstanceMetadata>,
  ): ElementInstanceMetadata | null {
    if (TP.isScenePath(target)) {
      const sceneInstancePath = TP.instancePath([], TP.elementPathForPath(target))
      return (
        rootElements.find((elem) => TP.pathsEqual(sceneInstancePath, elem.templatePath)) ?? null
      )
    } else {
      return TP.findAtElementPath(
        rootElements,
        target.element,
        MetadataUtils.elementInstanceMetadataGetChildren,
        getUtopiaID,
      )
    }
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
    metadata: ComponentMetadata[],
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
    metadata: Array<ComponentMetadata>,
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
  walkElementMetadata(
    elementMetadata: ElementInstanceMetadata,
    parentMetadata: ElementInstanceMetadata | null,
    withEachElement: (
      metadata: ElementInstanceMetadata,
      parentMetadataWith: ElementInstanceMetadata | null,
    ) => void,
  ): void {
    withEachElement(elementMetadata, parentMetadata)
    for (const child of elementMetadata.children) {
      this.walkElementMetadata(child, elementMetadata, withEachElement)
    }
  },
  walkMetadata(
    rootMetadata: Array<ComponentMetadata>,
    withEachElement: (
      metadata: ElementInstanceMetadata,
      parentMetadata: ElementInstanceMetadata | null,
    ) => void,
  ): void {
    for (const componentMetadata of rootMetadata) {
      for (const rootElement of componentMetadata.rootElements) {
        this.walkElementMetadata(rootElement, null, withEachElement)
      }
    }
  },
}

export function convertMetadataMap(
  metadataMap: {
    [templatePath: string]: MetadataWithoutChildren
  },
  scenes: { [templatePath: string]: ComponentMetadataWithoutRootElements },
): ComponentMetadata[] {
  function convertMetadata(rootMetadata: MetadataWithoutChildren): ElementInstanceMetadata {
    return {
      ...rootMetadata,
      children: mapDropNulls((childPath) => {
        const child = metadataMap[TP.toString(childPath)]
        return optionalMap(convertMetadata, child)
      }, rootMetadata.childrenTemplatePaths),
    }
  }

  const metadatas = Object.values(metadataMap)
  let result: ComponentMetadata[] = []
  Utils.fastForEach(Object.keys(scenes), (sceneIdString) => {
    const scene = scenes[sceneIdString]
    const sceneId = scene.scenePath
    const rootElements = metadatas.filter((m) => TP.isChildOf(m.templatePath, sceneId))

    result.push({
      ...scene,
      globalFrame: null,
      rootElements: rootElements.map(convertMetadata),
    })
  })
  return result
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

export function getSceneMetadataOrElementInstanceMetadata(
  target: TemplatePath,
  componentMetadata: Array<ComponentMetadata>,
): Either<ComponentMetadata, ElementInstanceMetadata> | null {
  if (TP.isScenePath(target)) {
    const sceneMetadata = MetadataUtils.findSceneByTemplatePath(componentMetadata, target)
    return optionalMap((m) => left<ComponentMetadata, ElementInstanceMetadata>(m), sceneMetadata)
  } else {
    const elementMetadata = MetadataUtils.getElementByInstancePathMaybe(componentMetadata, target)
    return optionalMap((m) => right<ComponentMetadata, ElementInstanceMetadata>(m), elementMetadata)
  }
}

export function getScenePropsOrElementAttributes(
  target: TemplatePath,
  componentMetadata: Array<ComponentMetadata>,
): PropsOrJSXAttributes | null {
  const metadata = getSceneMetadataOrElementInstanceMetadata(target, componentMetadata)
  if (metadata == null) {
    return null
  } else {
    return foldEither(
      (sceneMetadata) => left(sceneMetadata.container),
      (elementMetadata) =>
        foldEither(
          () => null,
          (element) => {
            if (isJSXElement(element)) {
              return right(element.props)
            } else {
              return null
            }
          },
          elementMetadata.element,
        ),
      metadata,
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
