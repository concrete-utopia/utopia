import {
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  walkContentsTree,
  walkContentsTreeForParseSuccess,
} from '../../components/assets'
import { importedFromWhere } from '../../components/editor/import-utils'
import Utils, { IndexPosition } from '../../utils/utils'
import { Either, isRight } from '../shared/either'
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
  getJSXAttribute,
  getJSXElementNameAsString,
  Param,
  JSXAttributes,
  JSXAttributesPart,
  JSXAttribute,
  JSXAttributesSpread,
  JSXArrayElement,
  JSXProperty,
} from '../shared/element-template'
import {
  Imports,
  isParseSuccess,
  isTextFile,
  StaticElementPathPart,
  StaticElementPath,
  ElementPath,
} from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import {
  fixUtopiaElement,
  generateUID,
  getUtopiaIDFromJSXElement,
  setUtopiaIDOnJSXElement,
} from '../shared/uid-utils'
import { fastForEach } from '../shared/utils'
import {
  isUtopiaAPIComponent,
  getComponentsFromTopLevelElements,
  isGivenUtopiaAPIElement,
  isSceneAgainstImports,
} from './project-file-utils'
import { getStoryboardElementPath } from './scene-utils'
import { TransientFilesState } from '../../components/editor/store/editor-state'

function getAllUniqueUidsInner(
  projectContents: ProjectContentTreeRoot,
  throwErrorWithSuspiciousActions?: string,
): Array<string> {
  let uniqueIDs: Set<string> = Utils.emptySet()

  function extractUid(element: JSXElementChild): void {
    if (isJSXElement(element)) {
      fastForEach(element.children, extractUid)
      const uidProp = getJSXAttribute(element.props, 'data-uid')
      if (uidProp != null && isJSXAttributeValue(uidProp)) {
        const uid = uidProp.value
        if (throwErrorWithSuspiciousActions != null) {
          if (uniqueIDs.has(uid)) {
            throw new Error(
              `Found duplicate UID: '${uid}'. Suspicious action(s): ${throwErrorWithSuspiciousActions}`,
            )
          }
        }
        uniqueIDs.add(uid)
      } else {
        if (throwErrorWithSuspiciousActions != null) {
          throw new Error(
            `Found JSXElement with missing UID. Suspicious action(s): ${throwErrorWithSuspiciousActions}`,
          )
        }
      }
    }
  }

  walkContentsTreeForParseSuccess(projectContents, (fullPath, parseSuccess) => {
    fastForEach(parseSuccess.topLevelElements, (tle) => {
      if (isUtopiaJSXComponent(tle)) {
        extractUid(tle.rootElement)
      }
    })
  })

  return Array.from(uniqueIDs)
}

export const getAllUniqueUids = Utils.memoize(getAllUniqueUidsInner)

let MOCK_NEXT_GENERATED_UID: string | null = null
export function FOR_TESTS_setNextGeneratedUid(nextUid: string): void {
  MOCK_NEXT_GENERATED_UID = nextUid
}

export function generateUidWithExistingComponents(projectContents: ProjectContentTreeRoot): string {
  if (MOCK_NEXT_GENERATED_UID != null) {
    const toReturn = MOCK_NEXT_GENERATED_UID
    MOCK_NEXT_GENERATED_UID = null
    return toReturn
  }
  const existingUIDS = getAllUniqueUids(projectContents)
  return generateUID(existingUIDS)
}

export function guaranteeUniqueUids(
  elements: Array<JSXElementChild>,
  existingIDs: Array<string>,
): Array<JSXElementChild> {
  return elements.map((element) => fixUtopiaElement(element, existingIDs))
}

export function isSceneElement(
  element: JSXElementChild,
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): boolean {
  const file = getContentsTreeFileFromString(projectContents, filePath)
  if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return isSceneAgainstImports(element, file.fileContents.parsed.imports)
  } else {
    return false
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
  return (element as any).elementPath != null
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
    return EP.toUid(element.elementPath)
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
  path: StaticElementPath,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  const lastElementPathPart = EP.lastElementPathForPath(path)
  return lastElementPathPart == null
    ? components
    : transformJSXComponentAtElementPath(components, lastElementPathPart, transform)
}

export function transformJSXComponentAtElementPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPathPart,
  transform: (elem: JSXElement) => JSXElement,
): Array<UtopiaJSXComponent> {
  const transformResult = transformAtPathOptionally(components, path, transform)

  if (transformResult.transformedElement == null) {
    throw new Error(`Did not find element to transform ${EP.elementPathPartToString(path)}`)
  } else {
    return transformResult.elements
  }
}

function transformAtPathOptionally(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPathPart,
  transform: (elem: JSXElement) => JSXElement,
): EP.ElementsTransformResult<UtopiaJSXComponent> {
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
  path: StaticElementPath,
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

  const pathElements = EP.lastElementPathForPath(path)
  for (const component of components) {
    const topLevelResult =
      pathElements == null ? null : findAtPathInner(component.rootElement, pathElements)
    if (topLevelResult != null) {
      return topLevelResult
    }
  }

  return null
}

export function findJSXElementAtStaticPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPath,
): JSXElement | null {
  const foundElement = findJSXElementChildAtPath(components, path)
  if (foundElement != null && isJSXElement(foundElement)) {
    return foundElement
  } else {
    return null
  }
}

export function removeJSXElementChild(
  target: StaticElementPath,
  rootElements: Array<UtopiaJSXComponent>,
): Array<UtopiaJSXComponent> {
  const parentPath = EP.parentPath(target)
  const targetID = EP.toUid(target)
  // Remove it from where it used to be.

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

  const lastElementPathPart = EP.lastElementPathForPath(parentPath)
  return lastElementPathPart == null
    ? rootElements
    : transformAtPathOptionally(rootElements, lastElementPathPart, (parentElement: JSXElement) => {
        return removeRelevantChild(parentElement, true)
      }).elements
}

export function insertJSXElementChild(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  targetParent: StaticElementPath | null,
  elementToInsert: JSXElementChild,
  components: Array<UtopiaJSXComponent>,
  indexPosition: IndexPosition | null,
): Array<UtopiaJSXComponent> {
  const makeE = () => {
    // TODO delete me
    throw new Error('Should not attempt to create empty elements.')
  }
  const targetParentIncludingStoryboardRoot =
    targetParent ?? getStoryboardElementPath(projectContents, openFile)
  if (targetParentIncludingStoryboardRoot == null) {
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
  target: StaticElementPath,
): number {
  const parentPath = EP.parentPath(target)
  const parentElement = findJSXElementAtStaticPath(
    getComponentsFromTopLevelElements(topLevelElements),
    parentPath,
  )
  if (parentElement != null) {
    const elementUID = EP.toUid(target)
    return parentElement.children.findIndex((child) => {
      return isJSXElement(child) && getUtopiaID(child) === elementUID
    })
  } else {
    return -1
  }
}

export function elementOnlyHasSingleTextChild(jsxElement: JSXElement): boolean {
  return jsxElement.children.length === 1 && isJSXTextBlock(jsxElement.children[0])
}

export function codeUsesProperty(javascript: string, propsParam: Param, property: string): boolean {
  switch (propsParam.boundParam.type) {
    case 'REGULAR_PARAM':
      return javascript.includes(`${propsParam.boundParam.paramName}.${property}`)
    case 'DESTRUCTURED_OBJECT':
      return propsParam.boundParam.parts.some((part) => {
        const partBoundParam = part.param.boundParam
        if (partBoundParam.type === 'REGULAR_PARAM') {
          // This handles the aliasing that may be applied to the destructured field.
          const propertyToCheck = part.propertyName ?? partBoundParam.paramName
          if (propertyToCheck === property) {
            // This is the aliased name or if there's no alias the field name.
            const propertyToLookFor = partBoundParam.paramName
            return javascript.includes(propertyToLookFor)
          }
        }
        return false
      })
    case 'DESTRUCTURED_ARRAY':
      return false
    default:
      const _exhaustiveCheck: never = propsParam.boundParam
      throw new Error(`Unhandled param type: ${JSON.stringify(propsParam.boundParam)}`)
  }
}

export function componentUsesProperty(component: UtopiaJSXComponent, property: string): boolean {
  if (component.param == null) {
    return false
  } else {
    return elementUsesProperty(component.rootElement, component.param, property)
  }
}

export function elementUsesProperty(
  element: JSXElementChild,
  propsParam: Param,
  property: string,
): boolean {
  switch (element.type) {
    case 'JSX_ELEMENT':
      const fromChildren = element.children.some((child) => {
        return elementUsesProperty(child, propsParam, property)
      })
      const fromAttributes = attributesUseProperty(element.props, propsParam, property)
      return fromChildren || fromAttributes
    case 'JSX_ARBITRARY_BLOCK':
      return codeUsesProperty(element.originalJavascript, propsParam, property)
    case 'JSX_TEXT_BLOCK':
      return false
    case 'JSX_FRAGMENT':
      return element.children.some((child) => {
        return elementUsesProperty(child, propsParam, property)
      })
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled element type: ${JSON.stringify(element)}`)
  }
}

export function attributesUseProperty(
  attributes: JSXAttributes,
  propsParam: Param,
  property: string,
): boolean {
  return attributes.some((part) => attributePartUsesProperty(part, propsParam, property))
}

export function arrayElementUsesProperty(
  arrayElement: JSXArrayElement,
  propsParam: Param,
  property: string,
): boolean {
  return attributeUsesProperty(arrayElement.value, propsParam, property)
}

export function jsxPropertyUsesProperty(
  jsxProperty: JSXProperty,
  propsParam: Param,
  property: string,
): boolean {
  return attributeUsesProperty(jsxProperty.value, propsParam, property)
}

export function attributeUsesProperty(
  attribute: JSXAttribute,
  propsParam: Param,
  property: string,
): boolean {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return false
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return codeUsesProperty(attribute.javascript, propsParam, property)
    case 'ATTRIBUTE_NESTED_ARRAY':
      return attribute.content.some((elem) => {
        return arrayElementUsesProperty(elem, propsParam, property)
      })
    case 'ATTRIBUTE_NESTED_OBJECT':
      return attribute.content.some((elem) => {
        return jsxPropertyUsesProperty(elem, propsParam, property)
      })
    case 'ATTRIBUTE_FUNCTION_CALL':
      return attribute.parameters.some((parameter) => {
        return attributeUsesProperty(parameter, propsParam, property)
      })
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute type: ${JSON.stringify(attribute)}`)
  }
}

export function attributePartUsesProperty(
  attributesPart: JSXAttributesPart,
  propsParam: Param,
  property: string,
): boolean {
  switch (attributesPart.type) {
    case 'JSX_ATTRIBUTES_ENTRY':
      return attributeUsesProperty(attributesPart.value, propsParam, property)
    case 'JSX_ATTRIBUTES_SPREAD':
      return attributeUsesProperty(attributesPart.spreadValue, propsParam, property)
    default:
      const _exhaustiveCheck: never = attributesPart
      throw new Error(`Unhandled attribute part: ${JSON.stringify(attributesPart)}`)
  }
}
