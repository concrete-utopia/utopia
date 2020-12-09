import Utils, { IndexPosition } from '../../utils/utils'
import {
  ElementInstanceMetadata,
  ElementsWithin,
  getJSXElementNameLastPart,
  isJSXArbitraryBlock,
  isJSXAttributeValue,
  isJSXElement,
  isJSXTextBlock,
  isUtopiaJSXComponent,
  JSXArbitraryBlock,
  JSXElement,
  JSXElementChild,
  JSXTextBlock,
  TopLevelElement,
  UtopiaJSXComponent,
  isJSXFragment,
} from '../shared/element-template'
import {
  Imports,
  ScenePath,
  StaticElementPath,
  StaticInstancePath,
  StaticTemplatePath,
} from '../shared/project-file-types'
import * as TP from '../shared/template-path'
import {
  fixUtopiaElement,
  generateUID,
  getUtopiaIDFromJSXElement,
  setUtopiaIDOnJSXElement,
} from '../shared/uid-utils'
import { fastForEach } from '../shared/utils'
import { isUtopiaAPIComponent, getComponentsFromTopLevelElements } from './project-file-utils'
import { getStoryboardTemplatePath } from '../../components/editor/store/editor-state'

function getAllUniqueUidsInner(
  components: Array<UtopiaJSXComponent>,
  throwErrorWithSuspiciousActions?: string,
): Array<string> {
  let uniqueIDs: Set<string> = Utils.emptySet()

  function extractUid(element: JSXElementChild): void {
    if (isJSXElement(element)) {
      fastForEach(element.children, extractUid)
      if (isJSXAttributeValue(element.props['data-uid'])) {
        const uid = element.props['data-uid'].value
        if (throwErrorWithSuspiciousActions) {
          if (uniqueIDs.has(uid)) {
            throw new Error(
              `Found duplicate UID: '${uid}'. Suspicious action(s): ${throwErrorWithSuspiciousActions}`,
            )
          }
        }
        uniqueIDs.add(uid)
      } else {
        if (throwErrorWithSuspiciousActions) {
          throw new Error(
            `Found JSXElement with missing UID. Suspicious action(s): ${throwErrorWithSuspiciousActions}`,
          )
        }
      }
    }
  }

  fastForEach(components, (tle) => {
    extractUid(tle.rootElement)
  })

  return Array.from(uniqueIDs)
}

export const getAllUniqueUids = Utils.memoize(getAllUniqueUidsInner)

export function generateUidWithExistingComponents(components: Array<UtopiaJSXComponent>): string {
  const existingUIDS = getAllUniqueUids(components)
  return generateUID(existingUIDS)
}

export function guaranteeUniqueUids(
  elements: Array<JSXElementChild>,
  existingIDs: Array<string>,
): Array<JSXElementChild> {
  return elements.map((element) => fixUtopiaElement(element, existingIDs))
}

export function getValidTemplatePaths(
  topLevelElement: TopLevelElement,
  scenePath: ScenePath,
): Array<StaticInstancePath> {
  if (isUtopiaJSXComponent(topLevelElement)) {
    return getValidTemplatePathsFromElement(topLevelElement.rootElement, scenePath)
  } else {
    return []
  }
}

export function getValidTemplatePathsFromElement(
  element: JSXElementChild,
  parentPath: StaticTemplatePath,
): Array<StaticInstancePath> {
  if (isJSXElement(element)) {
    const uid = getUtopiaID(element)
    const path = TP.appendToPath(parentPath, uid)
    let paths = [path]
    fastForEach(element.children, (c) => paths.push(...getValidTemplatePathsFromElement(c, path)))
    return paths
  } else if (isJSXArbitraryBlock(element)) {
    let paths: Array<StaticInstancePath> = []
    fastForEach(Object.values(element.elementsWithin), (e) =>
      paths.push(...getValidTemplatePathsFromElement(e, parentPath)),
    )
    return paths
  } else if (isJSXFragment(element)) {
    let paths: Array<StaticInstancePath> = []
    fastForEach(Object.values(element.children), (e) =>
      paths.push(...getValidTemplatePathsFromElement(e, parentPath)),
    )
    return paths
  } else {
    return []
  }
}

// THIS IS SUPER UGLY, DO NOT USE OUTSIDE OF FILE
function isUtopiaJSXElement(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXElement {
  return isJSXElement(element as any)
}
function isUtopiaJSXArbitraryBlock(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXArbitraryBlock {
  return isJSXArbitraryBlock(element as any)
}
function isUtopiaJSXTextBlock(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXTextBlock {
  return isJSXTextBlock(element as any)
}
function isElementInstanceMetadata(
  element: JSXElementChild | ElementInstanceMetadata,
): element is ElementInstanceMetadata {
  return (element as any).templatePath != null
}

export function setUtopiaID(element: JSXElementChild, uid: string): JSXElementChild {
  if (isUtopiaJSXElement(element)) {
    return setUtopiaIDOnJSXElement(element, uid)
  } else {
    throw new Error(`Unable to set utopia id on ${element.type}`)
  }
}

export function getUtopiaID(element: JSXElementChild | ElementInstanceMetadata): string {
  if (isUtopiaJSXElement(element)) {
    return getUtopiaIDFromJSXElement(element)
  } else if (isUtopiaJSXArbitraryBlock(element)) {
    return element.uniqueID
  } else if (isUtopiaJSXTextBlock(element)) {
    return element.uniqueID
  } else if (isElementInstanceMetadata(element)) {
    return TP.toTemplateId(element.templatePath)
  } else if (isJSXFragment(element)) {
    return element.uniqueID
  }
  throw new Error(`Cannot recognize element ${JSON.stringify(element)}`)
}

export function elementSupportsChildren(imports: Imports, element: JSXElementChild): boolean {
  if (isJSXElement(element)) {
    if (isUtopiaAPIComponent(element.name, imports)) {
      return getJSXElementNameLastPart(element.name) === 'View'
    } else {
      // Be permissive about HTML elements.
      return true
    }
  } else {
    return false
  }
}

export function transformJSXComponentAtPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticInstancePath,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  return transformJSXComponentAtElementPath(components, TP.elementPathForPath(path), transform)
}

export function transformJSXComponentAtElementPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPath,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  const transformResult = transformAtPathOptionally(components, path, transform)

  if (transformResult.transformedElement == null) {
    throw new Error(`Did not find element to transform ${TP.elementPathToString(path)}`)
  } else {
    return transformResult.elements
  }
}

function transformAtPathOptionally(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPath,
  transform: (elem: JSXElement) => JSXElement,
): TP.ElementsTransformResult<UtopiaJSXComponent> {
  function findAndTransformAtPathInner(
    element: JSXElementChild,
    workingPath: string[],
  ): JSXElementChild | null {
    const [firstUIDOrIndex, ...tailPath] = workingPath
    if (isJSXElement(element)) {
      if (getUtopiaID(element) === firstUIDOrIndex) {
        // transform
        if (tailPath.length === 0) {
          return transform(element)
        } else {
          // we will want to transform one of our children
          let childrenUpdated: boolean = false
          const updatedChildren = element.children.map((child) => {
            const possibleUpdate = findAndTransformAtPathInner(child, tailPath)
            if (possibleUpdate != null) {
              childrenUpdated = true
            }
            return Utils.defaultIfNull(child, possibleUpdate)
          })
          if (childrenUpdated) {
            return {
              ...element,
              children: updatedChildren,
            }
          }
        }
      }
    } else if (isJSXArbitraryBlock(element)) {
      if (firstUIDOrIndex in element.elementsWithin) {
        const updated = findAndTransformAtPathInner(
          element.elementsWithin[firstUIDOrIndex],
          workingPath,
        )
        if (updated != null && isJSXElement(updated)) {
          const newElementsWithin: ElementsWithin = {
            ...element.elementsWithin,
            [firstUIDOrIndex]: updated,
          }
          return {
            ...element,
            elementsWithin: newElementsWithin,
          }
        }
      }
    } else if (isJSXFragment(element)) {
      let childrenUpdated: boolean = false
      const updatedChildren = element.children.map((child) => {
        const possibleUpdate = findAndTransformAtPathInner(child, workingPath)
        if (possibleUpdate != null) {
          childrenUpdated = true
        }
        return Utils.defaultIfNull(child, possibleUpdate)
      })
      if (childrenUpdated) {
        return {
          ...element,
          children: updatedChildren,
        }
      }
    }
    return null
  }

  let transformedElement: UtopiaJSXComponent | null = null
  const transformedElements = components.map((component) => {
    const updatedElement = findAndTransformAtPathInner(component.rootElement, path)
    if (updatedElement == null) {
      return component
    } else {
      const newComponent: UtopiaJSXComponent = {
        ...component,
        rootElement: updatedElement,
      }
      transformedElement = newComponent
      return newComponent
    }
  })

  return {
    elements: transformedElements,
    transformedElement: transformedElement,
  }
}

export function findJSXElementChildAtPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticInstancePath,
): JSXElementChild | null {
  function findAtPathInner(
    element: JSXElementChild,
    workingPath: Array<string>,
  ): JSXElementChild | null {
    const firstUIDOrIndex = workingPath[0]
    if (isJSXElement(element)) {
      const uid = getUtopiaID(element)
      if (uid === firstUIDOrIndex) {
        const tailPath = workingPath.slice(1)
        if (tailPath.length === 0) {
          // this is the element we want
          return element
        } else {
          // we will want to delve into the children
          const children = element.children
          for (const child of children) {
            const childResult = findAtPathInner(child, tailPath)
            if (childResult != null) {
              return childResult
            }
          }
        }
      }
    } else if (isJSXArbitraryBlock(element)) {
      if (firstUIDOrIndex in element.elementsWithin) {
        const elementWithin = element.elementsWithin[firstUIDOrIndex]
        const withinResult = findAtPathInner(elementWithin, workingPath)
        if (withinResult != null) {
          return withinResult
        }
      }
    } else if (isJSXFragment(element)) {
      const children = element.children
      for (const child of children) {
        const childResult = findAtPathInner(child, workingPath)
        if (childResult != null) {
          return childResult
        }
      }
    }
    return null
  }

  const pathElements = TP.elementPathForPath(path)
  for (const component of components) {
    const topLevelResult = findAtPathInner(component.rootElement, pathElements)
    if (topLevelResult != null) {
      return topLevelResult
    }
  }

  return null
}

export function findJSXElementAtStaticPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticInstancePath,
): JSXElement | null {
  const foundElement = findJSXElementChildAtPath(components, path)
  if (foundElement != null && isJSXElement(foundElement)) {
    return foundElement
  } else {
    return null
  }
}

export function removeJSXElementChild(
  target: StaticTemplatePath,
  rootElements: Array<UtopiaJSXComponent>,
): Array<UtopiaJSXComponent> {
  if (TP.isScenePath(target)) {
    // TODO Scene Implementation
    return rootElements
  }

  const parentPath = TP.parentPath(target)
  const targetID = TP.toTemplateId(target)
  // Remove it from where it used to be.
  if (TP.isScenePath(parentPath)) {
    // TODO Scene Implementation
    return rootElements
  } else {
    function removeRelevantChild<T extends JSXElementChild>(
      parentElement: T,
      descendIntoElements: boolean,
    ): T {
      if (isJSXElement(parentElement) && descendIntoElements) {
        let updatedChildren = parentElement.children.filter((child) => {
          return getUtopiaID(child) != targetID
        })
        updatedChildren = updatedChildren.map((child) => {
          return removeRelevantChild(child, false)
        })
        return {
          ...parentElement,
          children: updatedChildren,
        }
      } else if (isJSXFragment(parentElement)) {
        let updatedChildren = parentElement.children.filter((child) => {
          return getUtopiaID(child) != targetID
        })
        updatedChildren = updatedChildren.map((child) => removeRelevantChild(child, false))
        return {
          ...parentElement,
          children: updatedChildren,
        }
      } else {
        return parentElement
      }
    }
    return transformAtPathOptionally(
      rootElements,
      TP.elementPathForPath(parentPath),
      (parentElement: JSXElement) => {
        return removeRelevantChild(parentElement, true)
      },
    ).elements
  }
}

export function insertJSXElementChild(
  targetParent: StaticTemplatePath | null,
  elementToInsert: JSXElementChild,
  components: Array<UtopiaJSXComponent>,
  indexPosition: IndexPosition | null,
): Array<UtopiaJSXComponent> {
  const makeE = () => {
    // TODO delete me
    throw new Error('Should not attempt to create empty elements.')
  }
  const targetParentIncludingStoryboardRoot = targetParent ?? getStoryboardTemplatePath(components)
  if (targetParentIncludingStoryboardRoot == null) {
    return components
  } else if (TP.isScenePath(targetParentIncludingStoryboardRoot)) {
    // TODO Scene Implementation
    return components
  } else {
    return transformJSXComponentAtPath(
      components,
      targetParentIncludingStoryboardRoot,
      (parentElement) => {
        if (isJSXElement(parentElement)) {
          let updatedChildren: Array<JSXElementChild>
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
        } else {
          return parentElement
        }
      },
    )
  }
}

export function getZIndexOfElement(
  topLevelElements: Array<TopLevelElement>,
  target: StaticInstancePath,
): number {
  const parentPath = TP.instancePathParent(target)
  if (parentPath != null) {
    if (!TP.isScenePath(parentPath)) {
      const parentElement = findJSXElementAtStaticPath(
        getComponentsFromTopLevelElements(topLevelElements),
        parentPath,
      )
      if (parentElement != null) {
        const elementUID = TP.toUid(target)
        return parentElement.children.findIndex((child) => {
          return isJSXElement(child) && getUtopiaID(child) === elementUID
        })
      }
    }
  }
  return -1
}

export function transformAllElements(
  components: Array<UtopiaJSXComponent>,
  transform: (element: JSXElementChild) => JSXElementChild,
): Array<UtopiaJSXComponent> {
  function innerTransform(element: JSXElementChild): JSXElementChild {
    if (isJSXElement(element) || isJSXFragment(element)) {
      const updatedChildren = element.children.map(innerTransform)
      return transform({
        ...element,
        children: updatedChildren,
      })
    } else {
      return transform(element)
    }
  }
  return components.map((component) => {
    return {
      ...component,
      rootElement: innerTransform(component.rootElement),
    }
  })
}
