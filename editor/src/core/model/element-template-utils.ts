import {
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  walkContentsTreeForParseSuccess,
} from '../../components/assets'
import Utils, { IndexPosition } from '../../utils/utils'
import {
  ElementsWithin,
  isJSXArbitraryBlock,
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
  JSXAttribute,
  JSXArrayElement,
  JSXProperty,
  isSpreadAssignment,
  isJSXAttributeOtherJavaScript,
  jsxElementName,
  jsxElementNameEquals,
  isJSXElementLike,
  isJSXConditionalExpression,
  childOrBlockIsChild,
  emptyComments,
  ChildOrAttribute,
  jsxAttributeValue,
  childOrBlockIsAttribute,
  jsxFragment,
  jsxTextBlock,
  jsxArbitraryBlock,
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
} from '../shared/element-template'
import {
  isParseSuccess,
  isTextFile,
  StaticElementPathPart,
  StaticElementPath,
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
import {
  getJSXAttributeAtPath,
  GetJSXAttributeResult,
  jsxSimpleAttributeToValue,
} from '../shared/jsx-attributes'
import { forceNotNull } from '../shared/optional-utils'
import {
  ConditionalCase,
  conditionalWhenFalseOptic,
  conditionalWhenTrueOptic,
  getConditionalCase,
  getConditionalCasePath,
  getConditionalClausePath,
} from './conditionals'
import { modify } from '../shared/optics/optic-utilities'
import { foldEither } from '../shared/either'
import {
  getElementPathFromReparentTargetParent,
  ReparentTargetParent,
  reparentTargetParentIsConditionalClause,
} from '../../components/editor/store/reparent-target'

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
      if (childOrBlockIsChild(element.whenTrue)) {
        extractUid(element.whenTrue)
      }
      if (childOrBlockIsChild(element.whenFalse)) {
        extractUid(element.whenFalse)
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
  if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
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
    } else if (isJSXArbitraryBlock(element)) {
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
        const updatedWhenTrue = childOrBlockIsChild(element.whenTrue)
          ? findAndTransformAtPathInner(element.whenTrue, tailPath)
          : null
        const updatedWhenFalse = childOrBlockIsChild(element.whenFalse)
          ? findAndTransformAtPathInner(element.whenFalse, tailPath)
          : null
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
      if (
        childOrBlockIsChild(element.whenTrue) &&
        getUtopiaID(element.whenTrue) === firstUIDOrIndex
      ) {
        const updated = findAndTransformAtPathInner(element.whenTrue, workingPath)
        if (updated != null && isJSXElement(updated)) {
          return {
            ...element,
            whenTrue: updated,
          }
        }
      }
      if (
        childOrBlockIsChild(element.whenFalse) &&
        getUtopiaID(element.whenFalse) === firstUIDOrIndex
      ) {
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
    if (isJSXElementLike(element) && getUtopiaID(element) === firstUIDOrIndex) {
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
    } else if (isJSXArbitraryBlock(element) && firstUIDOrIndex in element.elementsWithin) {
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
        function elementOrNullFromClause(
          clause: ChildOrAttribute,
          branch: ConditionalCase,
        ): JSXElementChild | null {
          // handle the special cased true-case / false-case path element first
          if (tailPath.length === 1 && tailPath[0] === branch) {
            // return null in case this is a JSXAttribute, since this function is looking for a JSXElementChild
            return childOrBlockIsAttribute(clause) ? null : clause
          }

          if (childOrBlockIsChild(clause)) {
            // if it's a child, get its inner element
            return findAtPathInner(clause, tailPath)
          }

          return null
        }
        return (
          elementOrNullFromClause(element.whenTrue, 'true-case') ??
          elementOrNullFromClause(element.whenFalse, 'false-case')
        )
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
      const trueCasePath = getConditionalClausePath(parentPath, parentElement.whenTrue, 'true-case')
      const falseCasePath = getConditionalClausePath(
        parentPath,
        parentElement.whenFalse,
        'false-case',
      )

      const nullAttribute = jsxAttributeValue(null, emptyComments)

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

export function insertJSXElementChild(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  targetParent: ReparentTargetParent<StaticElementPath> | null,
  elementToInsert: JSXElementChild,
  components: Array<UtopiaJSXComponent>,
  indexPosition: IndexPosition | null,
  spyMetadata: ElementInstanceMetadataMap,
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
    const parentPath = getElementPathFromReparentTargetParent(targetParentIncludingStoryboardRoot)
    return transformJSXComponentAtPath(components, parentPath, (parentElement) => {
      if (
        reparentTargetParentIsConditionalClause(targetParentIncludingStoryboardRoot) &&
        isJSXConditionalExpression(parentElement)
      ) {
        // Determine which clause of the conditional we want to modify.
        const toClauseOptic =
          targetParentIncludingStoryboardRoot.clause === 'true-case'
            ? conditionalWhenTrueOptic
            : conditionalWhenFalseOptic
        // Update the clause if it currently holds a null value.
        return modify(
          toClauseOptic,
          (clauseValue) => {
            if (childOrBlockIsAttribute(clauseValue)) {
              const simpleValue = jsxSimpleAttributeToValue(clauseValue)
              return foldEither(
                () => {
                  // Not a simple value, so replacing it would be a bad thing.
                  return clauseValue
                },
                (value) => {
                  // Simple value of some kind.
                  if (value == null) {
                    // Simple value is null, so replace it with the new content.
                    return elementToInsert
                  } else {
                    // FIXME: We have a simple `JSXAttribute` value here which is not null/undefined.
                    // Recent conversations have been about unifying attributes and arbitrary JSX elements,
                    // which could result in this content being a valid child of any element.
                    return clauseValue
                  }
                },
                simpleValue,
              )
            } else {
              if (isJSXFragment(clauseValue)) {
                // Existing fragment, so add it in as appropriate.
                let updatedChildren: Array<JSXElementChild>
                if (indexPosition == null) {
                  updatedChildren = clauseValue.children.concat(elementToInsert)
                } else {
                  updatedChildren = Utils.addToArrayWithFill(
                    elementToInsert,
                    clauseValue.children,
                    indexPosition,
                    makeE,
                  )
                }
                return jsxFragment(clauseValue.uid, updatedChildren, clauseValue.longForm)
              } else {
                // Something other than a fragment, so wrap that and the newly inserted element into a fragment.
                return jsxFragment(
                  generateUidWithExistingComponents(projectContents),
                  [clauseValue, elementToInsert],
                  false,
                )
              }
            }
          },
          parentElement,
        )
      } else if (isJSXElementLike(parentElement)) {
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
      } else if (isJSXConditionalExpression(parentElement)) {
        function getNewBranch(branch: ChildOrAttribute): ChildOrAttribute {
          switch (branch.type) {
            case 'ATTRIBUTE_VALUE':
              if (branch.value === null) {
                return elementToInsert
              }
              return jsxFragment(
                generateUidWithExistingComponents(projectContents),
                [jsxTextBlock(`${branch.value}`), elementToInsert],
                false,
              )
            case 'ATTRIBUTE_OTHER_JAVASCRIPT':
              return jsxFragment(
                generateUidWithExistingComponents(projectContents),
                [
                  jsxArbitraryBlock(
                    branch.javascript,
                    branch.javascript,
                    branch.transpiledJavascript,
                    branch.definedElsewhere,
                    branch.sourceMap,
                    branch.elementsWithin,
                  ),
                  elementToInsert,
                ],
                false,
              )
            case 'ATTRIBUTE_FUNCTION_CALL':
            case 'ATTRIBUTE_NESTED_ARRAY':
            case 'ATTRIBUTE_NESTED_OBJECT':
              return branch
            case 'JSX_FRAGMENT':
              return { ...branch, children: [...branch.children, elementToInsert] }
            case 'JSX_ELEMENT':
            case 'JSX_ARBITRARY_BLOCK':
            case 'JSX_TEXT_BLOCK':
            case 'JSX_CONDITIONAL_EXPRESSION':
              return jsxFragment(
                generateUidWithExistingComponents(projectContents),
                [branch, elementToInsert],
                false,
              )
            default:
              assertNever(branch)
          }
        }

        function getIsTrueCase(conditional: JSXConditionalExpression): boolean {
          const spyParentMetadata = spyMetadata[EP.toString(parentPath)] ?? null
          if (spyParentMetadata == null) {
            return true
          }
          const conditionalCase = getConditionalCase(
            getConditionalCasePath(parentPath, 'true-case'),
            conditional,
            spyParentMetadata,
            parentPath,
          )
          return conditionalCase === 'true-case'
        }
        const isTrueCase = getIsTrueCase(parentElement)

        const branch = getNewBranch(isTrueCase ? parentElement.whenTrue : parentElement.whenFalse)

        return isTrueCase
          ? { ...parentElement, whenTrue: branch }
          : { ...parentElement, whenFalse: branch }
      } else {
        return parentElement
      }
    })
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
      return isJSXElementLike(child) && getUtopiaID(child) === elementUID
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
        case 'JSX_ARBITRARY_BLOCK':
        case 'JSX_CONDITIONAL_EXPRESSION': // TODO: maybe if it is true for the current branch?
          return false // We can't possibly know at this point
        case 'JSX_ELEMENT':
          return jsxElementNameEquals(element.name, jsxElementName('br', []))
        case 'JSX_FRAGMENT':
          return allElementsAndChildrenAreText(element.children)
        case 'JSX_TEXT_BLOCK':
          return textBlockIsNonEmpty(element)
        default:
          assertNever(element)
      }
    })
  )
}

export function elementOnlyHasTextChildren(element: JSXElementChild): boolean {
  switch (element.type) {
    case 'JSX_ARBITRARY_BLOCK':
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
              if (isJSXAttributeOtherJavaScript(spreadPart)) {
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
                    if (isJSXAttributeOtherJavaScript(spreadPart)) {
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
    case 'JSX_ARBITRARY_BLOCK':
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
        (childOrBlockIsChild(element.whenTrue) &&
          elementUsesProperty(element.whenTrue, propsParam, property)) ||
        (childOrBlockIsChild(element.whenFalse) &&
          elementUsesProperty(element.whenFalse, propsParam, property))
      )
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
