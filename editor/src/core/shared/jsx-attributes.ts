import * as ObjectPath from 'object-path'
import type { MapLike } from 'typescript'
import { UtopiaUtils } from 'utopia-api/core'
import { findLastIndex, uniqBy } from './array-utils'
import type { Either } from './either'
import {
  applicative2Either,
  flatMapEither,
  isLeft,
  isRight,
  left,
  mapEither,
  reduceWithEither,
  right,
  traverseEither,
} from './either'
import type {
  JSXArrayElement,
  JSExpression,
  JSExpressionNestedObject,
  JSXAttributeNotFound,
  JSXAttributes,
  JSXProperty,
  PartOfJSXAttributeValue,
  ElementsWithin,
  JSIdentifier,
  JSPropertyAccess,
  JSElementAccess,
} from './element-template'
import {
  isArraySpread,
  isPropertyAssignment,
  jsxArrayValue,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsxAttributeNotFound,
  jsExpressionValue,
  jsxPropertyAssignment,
  partOfJsxAttributeValue,
  modifiableAttributeIsPartOfAttributeValue,
  modifiableAttributeIsAttributeNotFound,
  getJSXAttribute,
  deleteJSXAttribute,
  setJSXAttributesAttribute,
  isJSXAttributeValue,
  simplifyAttributesIfPossible,
  modifiableAttributeIsAttributeOtherJavaScript,
  emptyComments,
  jsxAttributeNestedArraySimple,
  clearExpressionUniqueIDs,
  jsExpressionOtherJavaScript,
  getDefinedElsewhereFromAttribute,
  isJSIdentifier,
  jsxFragment,
  jsOpaqueArbitraryStatement,
} from './element-template'
import { resolveParamsAndRunJsCode } from './javascript-cache'
import type {
  ElementPath,
  HighlightBoundsForUids,
  Imports,
  PropertyPath,
  PropertyPathPart,
} from './project-file-types'
import * as PP from './property-path'
import { NO_OP, assertNever, fastForEach } from './utils'
import { optionalMap } from './optional-utils'
import { getAllObjectPaths } from './object-utils'
import type { RenderContext } from '../../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-element-renderer-utils'
import { renderCoreElement } from '../../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-element-renderer-utils'
import { emptyUiJsxCanvasContextData } from '../../components/canvas/ui-jsx-canvas'
import type { UIFileBase64Blobs } from '../../components/editor/store/editor-state'
import type {
  DomWalkerInvalidatePathsCtxData,
  UiJsxCanvasContextData,
  VariableData,
} from '../../components/canvas/ui-jsx-canvas'
import {
  getJSExpressionAtPathParts,
  getJSXAttributesAtPath,
  type GetJSXAttributeResult,
  setJSXValueAtPath,
  jsxSimpleAttributeToValue,
  dropKeyFromNestedObject,
} from './jsx-attribute-utils'
import * as EP from './element-path'
import { render } from 'enzyme'

export type AnyMap = { [key: string]: any }

export function jsxFunctionAttributeToValue(
  attribute: ModifiableAttribute,
): Either<null, { functionName: string; parameters: Array<any> }> {
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_IDENTIFIER':
    case 'JSX_ELEMENT':
      return left(null)
    case 'ATTRIBUTE_FUNCTION_CALL':
      const extractedSimpleValueParameters = traverseEither(
        jsxSimpleAttributeToValue,
        attribute.parameters,
      )
      if (isLeft(extractedSimpleValueParameters)) {
        return left(null)
      } else {
        return right({
          functionName: attribute.functionName,
          parameters: extractedSimpleValueParameters.value,
        })
      }
    default:
      assertNever(attribute)
  }
}

export function jsxFunctionAttributeToRawValue(
  attribute: ModifiableAttribute,
): Either<null, { functionName: string; parameters: Array<any> }> {
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_IDENTIFIER':
    case 'JSX_ELEMENT':
      return left(null)
    case 'ATTRIBUTE_FUNCTION_CALL':
      return right({
        functionName: attribute.functionName,
        parameters: attribute.parameters,
      })
    default:
      assertNever(attribute)
  }
}

// FIXME The naming here is **BAD**, because this is actually for JSExpressions, not just attributes
export function jsxAttributeToValue(
  inScope: MapLike<any>,
  attribute: JSExpression,
  elementPath: ElementPath | null,
  renderContext: RenderContext,
  uid: string | undefined,
  codeError: Error | null,
  assignedToProp: string | null,
): any {
  if (isExpressionAccessLike(attribute)) {
    try {
      return innerAttributeToValue(
        attribute,
        elementPath,
        inScope,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
    } catch {
      const originalJavascript = isJSIdentifier(attribute)
        ? attribute.name
        : attribute.originalJavascript
      // Run some arbitrary JavaScript to get a better error.
      const otherJavaScript = jsExpressionOtherJavaScript(
        [],
        originalJavascript,
        originalJavascript,
        `return ${originalJavascript}`,
        getDefinedElsewhereFromAttribute(attribute),
        attribute.sourceMap,
        {},
        attribute.comments,
        attribute.uid,
      )
      return resolveParamsAndRunJsCode(
        renderContext.filePath,
        otherJavaScript,
        renderContext.requireResult,
        inScope,
      )
    }
  } else {
    return innerAttributeToValue(
      attribute,
      elementPath,
      inScope,
      renderContext,
      uid,
      codeError,
      assignedToProp,
    )
  }
}

function isExpressionAccessLike(
  expression: JSExpression,
): expression is JSIdentifier | JSPropertyAccess | JSElementAccess {
  switch (expression.type) {
    case 'JS_IDENTIFIER':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_ELEMENT_ACCESS':
      return true
    default:
      return false
  }
}

function innerAttributeToValue(
  attribute: JSExpression,
  elementPath: ElementPath | null,
  inScope: MapLike<any>,
  renderContext: RenderContext,
  uid: string | undefined,
  codeError: Error | null,
  assignedToProp: string | null,
): any {
  const { filePath, requireResult } = renderContext
  switch (attribute.type) {
    case 'JSX_ELEMENT':
      const innerPath = optionalMap((path) => EP.appendToPath(path, attribute.uid), elementPath)
      return renderCoreElement(
        attribute,
        innerPath,
        inScope,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
    case 'ATTRIBUTE_VALUE':
      return attribute.value
    case 'JS_IDENTIFIER':
      if (attribute.name in inScope) {
        return inScope[attribute.name]
      } else if (attribute.name in requireResult) {
        return requireResult[attribute.name]
      } else {
        throw new Error('Identifier does not exist.')
      }
    case 'JS_PROPERTY_ACCESS': {
      const onValue = jsxAttributeToValue(
        inScope,
        attribute.onValue,
        elementPath,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )

      if (onValue == null) {
        if (attribute.optionallyChained === 'optionally-chained') {
          return undefined
        } else {
          throw new Error('The value is not defined.')
        }
      } else {
        if (Object.getOwnPropertyDescriptor(onValue, attribute.property) == null) {
          throw new Error('Does not have this property.')
        } else {
          return onValue[attribute.property]
        }
      }
    }
    case 'JS_ELEMENT_ACCESS': {
      const onValue = jsxAttributeToValue(
        inScope,
        attribute.onValue,
        elementPath,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
      const element = jsxAttributeToValue(
        inScope,
        attribute.element,
        elementPath,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
      if (attribute.optionallyChained === 'optionally-chained') {
        return onValue == null ? undefined : onValue[element]
      } else {
        return onValue[element]
      }
    }
    case 'ATTRIBUTE_NESTED_ARRAY':
      let returnArray: Array<any> = []
      for (const elem of attribute.content) {
        const value = jsxAttributeToValue(
          inScope,
          elem.value,
          elementPath,
          renderContext,
          uid,
          codeError,
          assignedToProp,
        )
        switch (elem.type) {
          case 'ARRAY_VALUE':
            returnArray.push(value)
            break
          case 'ARRAY_SPREAD':
            returnArray.push(...value)
            break
          default:
            assertNever(elem)
        }
      }

      return returnArray
    case 'ATTRIBUTE_NESTED_OBJECT':
      let returnObject: { [key: string]: any } = {}
      for (const prop of attribute.content) {
        const value = jsxAttributeToValue(
          inScope,
          prop.value,
          elementPath,
          renderContext,
          uid,
          codeError,
          assignedToProp,
        )

        switch (prop.type) {
          case 'PROPERTY_ASSIGNMENT':
            returnObject[prop.key] = value
            break
          case 'SPREAD_ASSIGNMENT':
            Object.assign(returnObject, value)
            break
          default:
            assertNever(prop)
        }
      }

      return returnObject
    case 'ATTRIBUTE_FUNCTION_CALL':
      const foundFunction = (UtopiaUtils as any)[attribute.functionName]
      if (foundFunction != null) {
        const resolvedParameters = attribute.parameters.map((param) => {
          return jsxAttributeToValue(
            inScope,
            param,
            elementPath,
            renderContext,
            uid,
            codeError,
            assignedToProp,
          )
        })
        return foundFunction(...resolvedParameters)
      }
      throw new Error(`Couldn't find helper function with name ${attribute.functionName}`)
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return resolveParamsAndRunJsCode(filePath, attribute, requireResult, inScope)
    default:
      assertNever(attribute)
  }
}

export function jsxAttributesToProps(
  inScope: MapLike<any>,
  attributes: JSXAttributes,
  elementPath: ElementPath | null,
  renderContext: RenderContext,
  uid: string | undefined,
  codeError: Error | null,
): any {
  let result: any = {}
  for (const entry of attributes) {
    switch (entry.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        result[entry.key] = jsxAttributeToValue(
          inScope,
          entry.value,
          elementPath,
          renderContext,
          uid,
          codeError,
          `${entry.key}`,
        )
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        Object.assign(
          result,
          jsxAttributeToValue(
            inScope,
            entry.spreadValue,
            elementPath,
            renderContext,
            uid,
            codeError,
            null,
          ),
        )
        break
      default:
        assertNever(entry)
    }
  }
  return result
}

export type GetModifiableAttributeResult = Either<string, ModifiableAttribute>

export type ModifiableAttribute = JSExpression | PartOfJSXAttributeValue | JSXAttributeNotFound

export function clearModifiableAttributeUniqueIDs(
  modifiableAttribute: ModifiableAttribute,
): ModifiableAttribute {
  switch (modifiableAttribute.type) {
    case 'PART_OF_ATTRIBUTE_VALUE':
      return modifiableAttribute
    case 'ATTRIBUTE_NOT_FOUND':
      return modifiableAttribute
    default:
      return clearExpressionUniqueIDs(modifiableAttribute)
  }
}

// FIXME The naming here is abysmal
export function getModifiableJSXAttributeAtPathFromAttribute(
  attribute: ModifiableAttribute,
  path: PropertyPath,
): GetModifiableAttributeResult {
  if (modifiableAttributeIsAttributeNotFound(attribute)) {
    return right(jsxAttributeNotFound())
  } else if (modifiableAttributeIsPartOfAttributeValue(attribute)) {
    const pathElems = PP.getElements(path)
    if (ObjectPath.has(attribute.value, pathElems)) {
      const extractedValue = ObjectPath.get(attribute.value, pathElems)
      return right(partOfJsxAttributeValue(extractedValue))
    } else {
      return right(jsxAttributeNotFound())
    }
  } else {
    const result = getJSExpressionAtPath(attribute, path)

    if (result.remainingPath == null) {
      return right(result.attribute)
    } else {
      return left(`Found non-value attribute ${result.attribute.type}`)
    }
  }
}

export function getJSExpressionAtPath(
  attribute: JSExpression,
  tail: PropertyPath,
): GetJSXAttributeResult {
  return getJSExpressionAtPathParts(attribute, PP.getElements(tail), 0)
}

export interface ValueAtPath {
  path: PropertyPath
  value: JSExpression
}
export function valueAtPath(path: PropertyPath, value: JSExpression): ValueAtPath {
  return {
    path,
    value,
  }
}

export function setJSXValuesAtPaths(
  attributes: JSXAttributes,
  valuesAtPaths: Array<ValueAtPath>,
): Either<string, JSXAttributes> {
  return reduceWithEither(
    (working, { path, value }) => {
      return setJSXValueAtPath(working, path, value)
    },
    attributes,
    valuesAtPaths,
  )
}

function unsetValueAtPath(value: any, path: PropertyPath): Either<string, any> {
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to manipulate value with an empty path.')
    default:
      const attributeKey = PP.firstPart(path)
      const lastPartOfPath = PP.depth(path) === 1
      if (typeof attributeKey === 'number' && typeof value === 'object' && Array.isArray(value)) {
        if (lastPartOfPath) {
          let newArray = [...value]
          newArray.splice(attributeKey, 1)
          return right(newArray)
        } else {
          const tailPath = PP.tail(path)
          const updatedInnerValue = unsetValueAtPath(value[attributeKey], tailPath)
          return mapEither((updated) => {
            let newArray = [...value]
            newArray[attributeKey] = updated
            return newArray
          }, updatedInnerValue)
        }
      } else if (typeof attributeKey === 'string' && typeof value === 'object') {
        if (lastPartOfPath) {
          let newObject = { ...value }
          delete newObject[attributeKey]
          return right(newObject)
        } else {
          const tailPath = PP.tail(path)
          const updatedInnerValue = unsetValueAtPath(value[attributeKey], tailPath)
          return mapEither((updated) => {
            let newObject = { ...value }
            newObject[attributeKey] = updated
            return newObject
          }, updatedInnerValue)
        }
      } else {
        return left('Cannot unset a value that does not exist.')
      }
  }
}

export function unsetJSXValuesAtPaths(
  attributes: JSXAttributes,
  paths: Array<PropertyPath>,
): Either<string, JSXAttributes> {
  return mapEither(
    simplifyAttributesIfPossible,
    reduceWithEither(
      (working, path) => {
        return unsetJSXValueAtPath(working, path)
      },
      attributes,
      paths,
    ),
  )
}

function unsetJSXValueInAttributeAtPath(
  attribute: JSExpression,
  path: PropertyPath,
): Either<string, JSExpression> {
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to manipulate attribute with an empty path.')
    default:
      const attributeKey = PP.firstPart(path)
      const lastPartOfPath = PP.depth(path) === 1
      switch (attribute.type) {
        case 'ATTRIBUTE_FUNCTION_CALL':
        case 'JSX_MAP_EXPRESSION':
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        case 'JS_ELEMENT_ACCESS':
        case 'JS_PROPERTY_ACCESS':
        case 'JS_IDENTIFIER':
        case 'JSX_ELEMENT':
          return left('Cannot unset a value set inside a value set from elsewhere.')
        case 'ATTRIBUTE_NESTED_ARRAY':
          if (typeof attributeKey === 'number') {
            if (attribute.content.some(isArraySpread)) {
              return left('Cannot unset a value in an array containing a spread')
            }

            if (lastPartOfPath) {
              let newArray: Array<JSXArrayElement> = [...attribute.content]
              newArray.splice(attributeKey, 1)
              return right(jsExpressionNestedArray(newArray, emptyComments))
            } else {
              const existingAttribute = attribute.content[attributeKey]
              if (existingAttribute == null) {
                return right(attribute)
              } else {
                const tailPath = PP.tail(path)
                const updatedNestedAttribute = unsetJSXValueInAttributeAtPath(
                  existingAttribute.value,
                  tailPath,
                )
                return mapEither((updated) => {
                  let newArray: Array<JSXArrayElement> = [...attribute.content]
                  newArray[attributeKey] = jsxArrayValue(updated, emptyComments)
                  return jsExpressionNestedArray(newArray, emptyComments)
                }, updatedNestedAttribute)
              }
            }
          } else {
            // Possibly this should be an exception, but lets err on the side
            // of not exploding things for the moment.
            return left('Cannot unset this property.')
          }
        case 'ATTRIBUTE_NESTED_OBJECT':
          const key = `${attributeKey}`
          const keysMatch = (prop: JSXProperty) => isPropertyAssignment(prop) && prop.key === key
          if (lastPartOfPath) {
            return right(dropKeyFromNestedObject(attribute, key))
          } else {
            const existingAttributeIndex = findLastIndex(keysMatch, attribute.content)
            if (existingAttributeIndex === -1) {
              return right(attribute)
            } else {
              const tailPath = PP.tail(path)
              const existingAttribute = attribute.content[existingAttributeIndex]
              const updatedAttribute = unsetJSXValueInAttributeAtPath(
                existingAttribute.value,
                tailPath,
              )
              return mapEither((updated) => {
                let newProps: Array<JSXProperty> = [...attribute.content]
                newProps[existingAttributeIndex] = jsxPropertyAssignment(
                  key,
                  updated,
                  emptyComments,
                  emptyComments,
                )
                return jsExpressionNestedObject(newProps, emptyComments)
              }, updatedAttribute)
            }
          }
        case 'ATTRIBUTE_VALUE':
          const updatedValue = unsetValueAtPath(attribute.value, path)
          return mapEither((updated) => {
            return jsExpressionValue(updated, emptyComments)
          }, updatedValue)
        default:
          const _exhaustiveCheck: never = attribute
          throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
      }
  }
}

export function unsetJSXValueAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
): Either<string, JSXAttributes> {
  const attributeKey = PP.firstPart(path)
  const attributeKeyAsString = typeof attributeKey === 'string' ? attributeKey : `${attributeKey}`
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to unset something from attributes with an empty path.')
    case 1:
      const updatedAttributes = deleteJSXAttribute(attributes, attributeKeyAsString)
      return right(updatedAttributes)
    default:
      const existingAttribute = getJSXAttribute(attributes, attributeKeyAsString)
      if (existingAttribute == null) {
        return right(attributes)
      } else {
        const tailPath = PP.tail(path)
        const updatedAttribute: Either<string, JSExpression> = unsetJSXValueInAttributeAtPath(
          existingAttribute,
          tailPath,
        )
        return mapEither((updated) => {
          return setJSXAttributesAttribute(attributes, attributeKeyAsString, updated)
        }, updatedAttribute)
      }
  }
}

function walkAttribute(
  attribute: JSExpression,
  path: PropertyPath | null,
  walk: (a: JSExpression, path: PropertyPath | null) => void,
): void {
  walk(attribute, path)
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JS_IDENTIFIER':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_ELEMENT_ACCESS':
    case 'JSX_ELEMENT':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      fastForEach(attribute.content, (elem, index) => {
        walkAttribute(
          elem.value,
          optionalMap((p) => PP.appendPropertyPathElems(p, [index]), path),
          walk,
        )
      })
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      fastForEach(attribute.parameters, (param) => {
        walkAttribute(param, null, walk)
      })
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      fastForEach(attribute.content, (elem) => {
        switch (elem.type) {
          case 'PROPERTY_ASSIGNMENT':
            walkAttribute(
              elem.value,
              optionalMap((p) => PP.appendPropertyPathElems(p, [elem.key]), path),
              walk,
            )
            break
          case 'SPREAD_ASSIGNMENT':
            walkAttribute(elem.value, path, walk)
            break
          default:
            const _exhaustiveCheck: never = elem
            throw new Error(`Unhandled attribute ${JSON.stringify(elem)}`)
        }
      })
      break
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function getAccumulatedElementsWithin(attributes: JSXAttributes): ElementsWithin {
  let elementsWithinAccumulator: ElementsWithin = {}
  walkAttributes(attributes, (attribute, path) => {
    if (modifiableAttributeIsAttributeOtherJavaScript(attribute)) {
      Object.assign(elementsWithinAccumulator, attribute.elementsWithin)
    }
  })

  return elementsWithinAccumulator
}

function walkAttributes(
  attributes: JSXAttributes,
  walk: (attribute: JSExpression, path: PropertyPath | null) => void,
): void {
  fastForEach(attributes, (attr) => {
    switch (attr.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        walkAttribute(attr.value, PP.create(attr.key), walk)
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        walkAttribute(attr.spreadValue, PP.create(), walk)
        break
      default:
        const _exhaustiveCheck: never = attr
        throw new Error(`Unhandled attribute ${JSON.stringify(attr)}`)
    }
  })
}

export function getAllPathsFromAttributes(attributes: JSXAttributes): Array<PropertyPath> {
  let paths: Array<PropertyPath> = []
  walkAttributes(attributes, (attribute, path) => {
    if (path != null) {
      paths.push(path)
      if (isJSXAttributeValue(attribute) && typeof attribute.value === 'object') {
        paths.push(
          ...getAllObjectPaths(attribute.value).map((p) =>
            PP.create(...path.propertyElements, ...p),
          ),
        )
      }
    }
  })
  return uniqBy(paths, PP.pathsEqual)
}

export function getNumberPropertyFromProps(
  props: JSXAttributes,
  property: PropertyPath,
): number | null {
  const possibleProperty = getJSXAttributesAtPath(props, property)
  const currentValue = optionalMap(jsxSimpleAttributeToValue, possibleProperty?.attribute)
  if (currentValue !== null && isRight(currentValue) && typeof currentValue.value === 'number') {
    return currentValue.value
  } else {
    return null
  }
}
