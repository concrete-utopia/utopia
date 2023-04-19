import {
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  walkContentsTreeForParseSuccess,
} from '../../components/assets'
import Utils, { IndexPosition } from '../../utils/utils'
import {
  ElementsWithin,
  isJSExpressionOtherJavaScript,
  isJSXAttributeValue,
  isJSXElement,
  isJSXTextBlock,
  isUtopiaJSXComponent,
  JSXElement,
  JSXElementChild,
  JSXTextBlock,
  TopLevelElement,
  UtopiaJSXComponent,
  isJSXFragment,
  getJSXAttribute,
  Param,
  JSXAttributes,
  JSXAttributesPart,
  JSExpression,
  JSXArrayElement,
  JSXProperty,
  isSpreadAssignment,
  modifiableAttributeIsAttributeOtherJavaScript,
  jsxElementName,
  jsxElementNameEquals,
  isJSXElementLike,
  isJSXConditionalExpression,
  emptyComments,
  jsExpressionValue,
  isIntrinsicElement,
  jsxFragment,
  JSXConditionalExpression,
} from '../shared/element-template'
import {
  isParseSuccess,
  isTextFile,
  StaticElementPathPart,
  StaticElementPath,
  ElementPath,
} from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import {
  fixUtopiaElement,
  generateMockNextGeneratedUID,
  generateUID,
  getUtopiaID,
} from '../shared/uid-utils'
import { assertNever, fastForEach } from '../shared/utils'
import { getComponentsFromTopLevelElements, isSceneAgainstImports } from './project-file-utils'
import { getStoryboardElementPath } from './scene-utils'
import { getJSXAttributeAtPath, GetJSXAttributeResult } from '../shared/jsx-attributes'
import { forceNotNull } from '../shared/optional-utils'
import {
  ConditionalCase,
  conditionalWhenFalseOptic,
  conditionalWhenTrueOptic,
  getConditionalClausePath,
  maybeBranchConditionalCase,
} from './conditionals'
import { modify } from '../shared/optics/optic-utilities'
import {
  getElementPathFromInsertionPath,
  InsertionPath,
  insertionPathIsArray,
  insertionPathIsConditionalClause,
  insertionPathIsSlot,
} from '../../components/editor/store/reparent-target'
import { intrinsicHTMLElementNamesThatSupportChildren } from '../shared/dom-utils'
import { isNullJSXAttributeValue } from '../shared/element-template'

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
    } else if (isJSXFragment(element)) {
      fastForEach(element.children, extractUid)
      uniqueIDs.add(element.uid)
    } else if (isJSXConditionalExpression(element)) {
      uniqueIDs.add(element.uid)
      extractUid(element.whenTrue)
      extractUid(element.whenFalse)
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

export function generateUidWithExistingComponents(projectContents: ProjectContentTreeRoot): string {
  const mockUID = generateMockNextGeneratedUID()
  if (mockUID == null) {
    const existingUIDS = getAllUniqueUids(projectContents)
    return generateUID(existingUIDS)
  } else {
    return mockUID
  }
}

export function generateUidWithExistingComponentsAndExtraUids(
  projectContents: ProjectContentTreeRoot,
  additionalUids: Array<string>,
): string {
  const mockUID = generateMockNextGeneratedUID()
  if (mockUID == null) {
    const existingUIDSFromProject = getAllUniqueUids(projectContents)
    return generateUID([...existingUIDSFromProject, ...additionalUids])
  } else {
    return mockUID
  }
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
  if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return isSceneAgainstImports(element, file.fileContents.parsed.imports)
  } else {
    return false
  }
}

export function transformJSXComponentAtPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPath,
  transform: (elem: JSXElementChild) => JSXElementChild,
): Array<UtopiaJSXComponent> {
  const lastElementPathPart = EP.lastElementPathForPath(path)
  return lastElementPathPart == null
    ? components
    : transformJSXComponentAtElementPath(components, lastElementPathPart, transform)
}

export function transformJSXComponentAtElementPath(
  components: Array<UtopiaJSXComponent>,
  path: StaticElementPathPart,
  transform: (elem: JSXElementChild) => JSXElementChild,
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
  transform: (elem: JSXElementChild) => JSXElementChild,
): EP.ElementsTransformResult<UtopiaJSXComponent> {
  function findAndTransformAtPathInner(
    element: JSXElementChild,
    workingPath: string[],
  ): JSXElementChild | null {
    const [firstUIDOrIndex, ...tailPath] = workingPath
    if (isJSXElementLike(element)) {
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
    } else if (isJSExpressionOtherJavaScript(element)) {
      if (getUtopiaID(element) === firstUIDOrIndex) {
        let childrenUpdated: boolean = false
        const updatedChildren = Object.values(element.elementsWithin).reduce(
          (acc, child): ElementsWithin => {
            const updated = findAndTransformAtPathInner(child, tailPath)
            if (updated != null && isJSXElement(updated)) {
              childrenUpdated = true
              return {
                ...acc,
                [child.uid]: updated,
              }
            }
            return acc
          },
          element.elementsWithin,
        )
        if (childrenUpdated) {
          return {
            ...element,
            elementsWithin: updatedChildren,
          }
        }
      }
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
    } else if (isJSXConditionalExpression(element)) {
      if (getUtopiaID(element) === firstUIDOrIndex) {
        const updatedWhenTrue = findAndTransformAtPathInner(element.whenTrue, tailPath)
        const updatedWhenFalse = findAndTransformAtPathInner(element.whenFalse, tailPath)
        if (updatedWhenTrue != null) {
          return {
            ...element,
            whenTrue: updatedWhenTrue,
          }
        }
        if (updatedWhenFalse != null) {
          return {
            ...element,
            whenFalse: updatedWhenFalse,
          }
        }
        return transform(element) // if no branch matches, transform the conditional itself
      }
      if (getUtopiaID(element.whenTrue) === firstUIDOrIndex) {
        const updated = findAndTransformAtPathInner(element.whenTrue, workingPath)
        if (updated != null && isJSXElement(updated)) {
          return {
            ...element,
            whenTrue: updated,
          }
        }
      }
      if (getUtopiaID(element.whenFalse) === firstUIDOrIndex) {
        const updated = findAndTransformAtPathInner(element.whenFalse, workingPath)
        if (updated != null && isJSXElement(updated)) {
          return {
            ...element,
            whenFalse: updated,
          }
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
    if (isJSExpressionOtherJavaScript(element) && firstUIDOrIndex in element.elementsWithin) {
      const elementWithin = element.elementsWithin[firstUIDOrIndex]
      const withinResult = findAtPathInner(elementWithin, workingPath)
      if (withinResult != null) {
        return withinResult
      }
    } else if (isJSXConditionalExpression(element) && getUtopiaID(element) === firstUIDOrIndex) {
      const tailPath = workingPath.slice(1)
      if (tailPath.length === 0) {
        // this is the element we want
        return element
      } else {
        return (
          findAtPathInner(element.whenTrue, tailPath) ??
          findAtPathInner(element.whenFalse, tailPath)
        )
      }
    } else if (getUtopiaID(element) === firstUIDOrIndex) {
      const tailPath = workingPath.slice(1)
      if (tailPath.length === 0) {
        // this is the element we want
        return element
      } else {
        if (isJSXElementLike(element)) {
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

export function rearrangeJsxChildren(
  target: StaticElementPath,
  rearrangedChildPaths: Array<StaticElementPath>,
  rootElements: Array<UtopiaJSXComponent>,
): Array<UtopiaJSXComponent> {
  const lastElementPathPart = EP.lastElementPathForPath(target)
  return lastElementPathPart == null
    ? rootElements
    : transformAtPathOptionally(
        rootElements,
        lastElementPathPart,
        (parentElement: JSXElementChild) => {
          if (isJSXElementLike(parentElement)) {
            const originalChildren = parentElement.children
            if (originalChildren.length !== rearrangedChildPaths.length) {
              throw new Error(
                `rearrangeJsxChildren error: target parent's children count (${originalChildren.length}) does not match input array length (${rearrangedChildPaths.length})`,
              )
            }

            const rearrangedChildren = rearrangedChildPaths.map((path) => {
              const targetUid = EP.toUid(path)
              return forceNotNull(
                `rearrangeJsxChildren did not find child with uid ${targetUid}`,
                originalChildren.find((c) => getUtopiaID(c) === targetUid),
              )
            })
            return { ...parentElement, children: rearrangedChildren }
          } else {
            return parentElement
          }
        },
      ).elements
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
    } else if (isJSXConditionalExpression(parentElement)) {
      const trueCasePath = getConditionalClausePath(parentPath, parentElement.whenTrue)
      const falseCasePath = getConditionalClausePath(parentPath, parentElement.whenFalse)

      const nullAttribute = jsExpressionValue(null, emptyComments)

      return {
        ...parentElement,
        whenTrue: EP.pathsEqual(trueCasePath, target) ? nullAttribute : parentElement.whenTrue,
        whenFalse: EP.pathsEqual(falseCasePath, target) ? nullAttribute : parentElement.whenFalse,
      }
    } else {
      return parentElement
    }
  }

  const lastElementPathPart = EP.lastElementPathForPath(parentPath)
  return lastElementPathPart == null
    ? rootElements
    : transformAtPathOptionally(
        rootElements,
        lastElementPathPart,
        (parentElement: JSXElementChild) => {
          return removeRelevantChild(parentElement, true)
        },
      ).elements
}

export interface InsertChildAndDetails {
  components: Array<UtopiaJSXComponent>
  insertionDetails: string | null
}

export function insertChildAndDetails(
  components: Array<UtopiaJSXComponent>,
  insertionDetails: string | null = null,
): InsertChildAndDetails {
  return {
    components: components,
    insertionDetails: insertionDetails,
  }
}

export function insertJSXElementChild(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  targetParent: InsertionPath | null,
  elementToInsert: JSXElementChild,
  components: Array<UtopiaJSXComponent>,
  indexPosition: IndexPosition | null,
): InsertChildAndDetails {
  const makeE = () => {
    // TODO delete me
    throw new Error('Should not attempt to create empty elements.')
  }
  const storyboardPath = getStoryboardElementPath(projectContents, openFile)

  // TODO the caller could should provde the storyboard root path if they want to insert to the storyboard root
  const targetParentIncludingStoryboardRoot: InsertionPath | null =
    targetParent ??
    // TODO this is the ugliest code I wrote today
    (storyboardPath == null
      ? null
      : {
          type: 'ARRAY_INSERTION',
          propName: 'children',
          elementPath: storyboardPath,
          indexPosition: null,
        })

  if (targetParentIncludingStoryboardRoot == null) {
    return insertChildAndDetails(components)
  } else {
    const parentPath = getElementPathFromInsertionPath(targetParentIncludingStoryboardRoot)
    let details: string | null = null
    const updatedComponents = transformJSXComponentAtPath(
      components,
      parentPath,
      (parentElement) => {
        if (targetParent == null) {
          return parentElement
        }
        if (insertionPathIsArray(targetParent)) {
          if (!isJSXElementLike(parentElement)) {
            return parentElement
          }
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
            [targetParentIncludingStoryboardRoot.propName]: updatedChildren,
          }
        } else if (insertionPathIsConditionalClause(targetParent)) {
          return elementToInsert
        } else {
          return parentElement
        }
      },
    )
    return insertChildAndDetails(updatedComponents, details)
  }
}

export function getIndexInParent(
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
      return getUtopiaID(child) === elementUID
    })
  } else {
    return -1
  }
}

export function elementOnlyHasSingleTextChild(jsxElement: JSXElement): boolean {
  return jsxElement.children.length === 1 && isJSXTextBlock(jsxElement.children[0])
}

function textBlockIsNonEmpty(textBlock: JSXTextBlock): boolean {
  return textBlock.text.trim().length > 0
}

function allElementsAndChildrenAreText(elements: Array<JSXElementChild>): boolean {
  return (
    elements.length > 0 &&
    elements.every((element) => {
      switch (element.type) {
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        case 'JSX_CONDITIONAL_EXPRESSION': // TODO: maybe if it is true for the current branch?
          return false // We can't possibly know at this point
        case 'JSX_ELEMENT':
          return jsxElementNameEquals(element.name, jsxElementName('br', []))
        case 'JSX_FRAGMENT':
          return allElementsAndChildrenAreText(element.children)
        case 'JSX_TEXT_BLOCK':
          return textBlockIsNonEmpty(element)
        case 'ATTRIBUTE_VALUE':
          return typeof element.value === 'string'
        case 'ATTRIBUTE_NESTED_ARRAY':
          return element.content.every((contentElement) => {
            return elementOnlyHasTextChildren(contentElement.value)
          })
        case 'ATTRIBUTE_NESTED_OBJECT':
          return element.content.every((contentElement) => {
            return elementOnlyHasTextChildren(contentElement.value)
          })
        case 'ATTRIBUTE_FUNCTION_CALL':
          return allElementsAndChildrenAreText(element.parameters)
        default:
          assertNever(element)
      }
    })
  )
}

export function elementOnlyHasTextChildren(element: JSXElementChild): boolean {
  switch (element.type) {
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JSX_CONDITIONAL_EXPRESSION': // TODO: maybe we the current branch only includes text children???
      return false // We can't possibly know at this point
    case 'JSX_ELEMENT':
      return (
        jsxElementNameEquals(element.name, jsxElementName('br', [])) ||
        allElementsAndChildrenAreText(element.children)
      )
    case 'JSX_FRAGMENT':
      return allElementsAndChildrenAreText(element.children)
    case 'JSX_TEXT_BLOCK':
      return textBlockIsNonEmpty(element)
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return false
    default:
      assertNever(element)
  }
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

export function componentHonoursPropsPosition(component: UtopiaJSXComponent): boolean {
  if (component.param == null) {
    return false
  } else {
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const leftStyleAttr = getJSXAttributeAtPath(rootElement.props, PP.create('style', 'left'))
      const topStyleAttr = getJSXAttributeAtPath(rootElement.props, PP.create('style', 'top'))
      const rightStyleAttr = getJSXAttributeAtPath(rootElement.props, PP.create('style', 'right'))
      const bottomStyleAttr = getJSXAttributeAtPath(rootElement.props, PP.create('style', 'bottom'))
      return (
        ((propertyComesFromPropsStyle(component.param, leftStyleAttr, 'left') ||
          propertyComesFromPropsStyle(component.param, rightStyleAttr, 'right')) &&
          (propertyComesFromPropsStyle(component.param, topStyleAttr, 'top') ||
            propertyComesFromPropsStyle(component.param, bottomStyleAttr, 'bottom'))) ||
        propsStyleIsSpreadInto(component.param, rootElement.props)
      )
    } else {
      return false
    }
  }
}

export function componentHonoursPropsSize(component: UtopiaJSXComponent): boolean {
  if (component.param == null) {
    return false
  } else {
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const widthStyleAttr = getJSXAttributeAtPath(rootElement.props, PP.create('style', 'width'))
      const heightStyleAttr = getJSXAttributeAtPath(rootElement.props, PP.create('style', 'height'))
      return (
        (propertyComesFromPropsStyle(component.param, widthStyleAttr, 'width') &&
          propertyComesFromPropsStyle(component.param, heightStyleAttr, 'height')) ||
        propsStyleIsSpreadInto(component.param, rootElement.props)
      )
    } else {
      return false
    }
  }
}

export function propsStyleIsSpreadInto(propsParam: Param, attributes: JSXAttributes): boolean {
  const boundParam = propsParam.boundParam
  switch (boundParam.type) {
    case 'REGULAR_PARAM': {
      const styleProp = getJSXAttributeAtPath(attributes, PP.create('style'))
      const styleAttribute = styleProp.attribute
      switch (styleAttribute.type) {
        case 'ATTRIBUTE_NOT_FOUND':
          return false
        case 'ATTRIBUTE_VALUE':
          return false
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
          return false
        case 'ATTRIBUTE_NESTED_ARRAY':
          return false
        case 'ATTRIBUTE_NESTED_OBJECT':
          return styleAttribute.content.some((attributePart) => {
            if (isSpreadAssignment(attributePart)) {
              const spreadPart = attributePart.value
              if (modifiableAttributeIsAttributeOtherJavaScript(spreadPart)) {
                return (
                  spreadPart.definedElsewhere.includes(boundParam.paramName) &&
                  spreadPart.transpiledJavascript.includes(`${boundParam.paramName}.style`)
                )
              }
            }
            return false
          })
        case 'ATTRIBUTE_FUNCTION_CALL':
          return false
        case 'PART_OF_ATTRIBUTE_VALUE':
          return false
        default:
          const _exhaustiveCheck: never = styleAttribute
          throw new Error(`Unhandled attribute type: ${JSON.stringify(styleAttribute)}`)
      }
    }
    case 'DESTRUCTURED_OBJECT': {
      return boundParam.parts.some((part) => {
        const partBoundParam = part.param.boundParam
        if (partBoundParam.type === 'REGULAR_PARAM') {
          // This handles the aliasing that may be applied to the destructured field.
          const propertyToCheck = part.propertyName ?? partBoundParam.paramName
          if (propertyToCheck === 'style') {
            // This is the aliased name or if there's no alias the field name.
            const propertyToLookFor = partBoundParam.paramName

            const styleProp = getJSXAttributeAtPath(attributes, PP.create('style'))
            const styleAttribute = styleProp.attribute
            switch (styleAttribute.type) {
              case 'ATTRIBUTE_NOT_FOUND':
                return false
              case 'ATTRIBUTE_VALUE':
                return false
              case 'ATTRIBUTE_OTHER_JAVASCRIPT':
                return false
              case 'ATTRIBUTE_NESTED_ARRAY':
                return false
              case 'ATTRIBUTE_NESTED_OBJECT':
                return styleAttribute.content.some((attributePart) => {
                  if (isSpreadAssignment(attributePart)) {
                    const spreadPart = attributePart.value
                    if (modifiableAttributeIsAttributeOtherJavaScript(spreadPart)) {
                      return (
                        spreadPart.definedElsewhere.includes(propertyToLookFor) &&
                        spreadPart.transpiledJavascript.includes(propertyToLookFor)
                      )
                    }
                  }
                  return false
                })
              case 'ATTRIBUTE_FUNCTION_CALL':
                return false
              case 'PART_OF_ATTRIBUTE_VALUE':
                return false
              default:
                const _exhaustiveCheck: never = styleAttribute
                throw new Error(`Unhandled attribute type: ${JSON.stringify(styleAttribute)}`)
            }
          }
        }
        return false
      })
    }
    case 'DESTRUCTURED_ARRAY':
      return false
    default:
      const _exhaustiveCheck: never = boundParam
      throw new Error(`Unhandled param type: ${JSON.stringify(boundParam)}`)
  }
}

export function propertyComesFromPropsStyle(
  propsParam: Param,
  result: GetJSXAttributeResult,
  propName: string,
): boolean {
  const attribute = result.attribute
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
      return false
    case 'ATTRIBUTE_VALUE':
      return false
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      const boundParam = propsParam.boundParam
      switch (boundParam.type) {
        case 'REGULAR_PARAM':
          return (
            attribute.definedElsewhere.includes(boundParam.paramName) &&
            attribute.javascript.includes(`${boundParam.paramName}.style.${propName}`)
          )
        case 'DESTRUCTURED_OBJECT':
          return boundParam.parts.some((part) => {
            const partBoundParam = part.param.boundParam
            if (partBoundParam.type === 'REGULAR_PARAM') {
              // This handles the aliasing that may be applied to the destructured field.
              const propertyToCheck = part.propertyName ?? partBoundParam.paramName
              if (propertyToCheck === 'style') {
                // This is the aliased name or if there's no alias the field name.
                const propertyToLookFor = partBoundParam.paramName
                return (
                  attribute.definedElsewhere.includes(propertyToLookFor) &&
                  attribute.transpiledJavascript.includes(`${propertyToLookFor}.${propName}`)
                )
              } else {
                return false
              }
            } else {
              return false
            }
          })
        default:
          return false
      }
    case 'ATTRIBUTE_NESTED_ARRAY':
      return false
    case 'ATTRIBUTE_NESTED_OBJECT':
      return false
    case 'ATTRIBUTE_FUNCTION_CALL':
      return false
    case 'PART_OF_ATTRIBUTE_VALUE':
      return false
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute type: ${JSON.stringify(attribute)}`)
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
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return codeUsesProperty(element.originalJavascript, propsParam, property)
    case 'JSX_TEXT_BLOCK':
      return false
    case 'JSX_FRAGMENT':
      return element.children.some((child) => {
        return elementUsesProperty(child, propsParam, property)
      })
    case 'JSX_CONDITIONAL_EXPRESSION':
      return (
        attributeUsesProperty(element.condition, propsParam, property) ||
        elementUsesProperty(element.whenTrue, propsParam, property) ||
        elementUsesProperty(element.whenFalse, propsParam, property)
      )
    case 'ATTRIBUTE_VALUE':
      return false
    case 'ATTRIBUTE_NESTED_ARRAY':
      return element.content.some((child) => {
        return elementUsesProperty(child.value, propsParam, property)
      })
    case 'ATTRIBUTE_NESTED_OBJECT':
      return element.content.some((child) => {
        return elementUsesProperty(child.value, propsParam, property)
      })
    case 'ATTRIBUTE_FUNCTION_CALL':
      return element.parameters.some((param) => {
        return elementUsesProperty(param, propsParam, property)
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
  attribute: JSExpression,
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

export type ElementSupportsChildren =
  | 'supportsChildren'
  | 'hasOnlyTextChildren'
  | 'doesNotSupportChildren'

export function elementChildSupportsChildrenAlsoText(
  element: JSXElementChild,
): ElementSupportsChildren | null {
  if (isJSXConditionalExpression(element)) {
    return 'doesNotSupportChildren'
  }
  if (elementOnlyHasTextChildren(element)) {
    // Prevent re-parenting into an element that only has text children, as that is rarely a desired goal.
    return 'hasOnlyTextChildren'
  } else {
    if (isJSXElement(element)) {
      if (isIntrinsicElement(element.name)) {
        return intrinsicHTMLElementNamesThatSupportChildren.includes(element.name.baseVariable)
          ? 'supportsChildren'
          : 'doesNotSupportChildren'
      }
    }
    // We don't know at this stage.
    return null
  }
}
