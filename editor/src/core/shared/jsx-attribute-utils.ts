import type { MapLike } from 'typescript'
import type { Either } from './either'
import { right, left, applicative2Either, flatMapEither, isLeft, mapEither } from './either'
import type {
  JSExpression,
  JSExpressionNestedObject,
  JSXArrayElement,
  JSXAttributeNotFound,
  JSXProperty,
} from './element-template'
import {
  getJSXAttribute,
  isPropertyAssignment,
  jsxAttributeNotFound,
  partOfJsxAttributeValue,
  type JSXAttributes,
  deleteJSXAttribute,
  setJSXAttributesAttribute,
  emptyComments,
  isArraySpread,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsExpressionValue,
  jsxArrayValue,
  jsxPropertyAssignment,
  jsxAttributeNestedArraySimple,
} from './element-template'
import type { GetModifiableAttributeResult, ModifiableAttribute } from './jsx-attributes'
import type { PropertyPath, PropertyPathPart } from './project-file-types'
import * as PP from './property-path'
import * as ObjectPath from 'object-path'
import { assertNever } from './utils'

export function dropKeyFromNestedObject(
  nestedObject: JSExpressionNestedObject,
  key: string,
): JSExpressionNestedObject {
  return jsExpressionNestedObject(
    nestedObject.content.filter((prop) => {
      if (isPropertyAssignment(prop)) {
        return prop.key !== key
      } else {
        return true
      }
    }),
    emptyComments,
  )
}

export function nestedObjectValueForKey(
  nestedObject: JSExpressionNestedObject,
  key: string,
): JSExpression | JSXAttributeNotFound {
  const value = nestedObject.content.find((prop) => isPropertyAssignment(prop) && prop.key === key)
  return value == null ? jsxAttributeNotFound() : value.value
}

function deeplyCreatedValue(elements: Array<PropertyPathPart>, value: JSExpression): JSExpression {
  return elements.reduceRight((acc: JSExpression, propName) => {
    if (typeof propName === 'number') {
      let newArray: Array<JSExpression> = []
      newArray[propName] = acc
      return jsxAttributeNestedArraySimple(newArray)
    } else {
      return jsExpressionNestedObject(
        [jsxPropertyAssignment(propName, acc, emptyComments, emptyComments)],
        emptyComments,
      )
    }
  }, value)
}

/**
 * getModifiableJSXAttributeAtPath
 * the return value is an `Either`, where the sides mean:
 * * Left side: UNOVERWRITABLE you tried to access an attribute at a path where you are *not* allowed to insert a value
 *    because one of the parent paths are "controlled" (read: they are a prop access, or a node graph value etc)
 *    Example: given the element `<View style={{backgroundColor: props.backgroundColor}} />`,
 *     trying to set `style.backgroundColor.red` to 5 is not sensible and will actually throw an error.
 *     This is what Left is indicating to us.
 * * Right side: FREE TO CHANGE you tried to access a path that you *are* allowed to dispatch an update action for! But it might or might not have a real value
 *    To figure out the real value, you have to drill into right's value
 */
export function getModifiableJSXAttributeAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
): GetModifiableAttributeResult {
  const result = getJSXAttributesAtPath(attributes, path)

  if (result.remainingPath == null) {
    return right(result.attribute)
  } else {
    return left(`Found non-value attribute ${result.attribute.type}`)
  }
}

export type GetJSXAttributeResult = {
  attribute: ModifiableAttribute
  remainingPath?: PropertyPath
}

export function getJSXAttributeResult(
  attribute: ModifiableAttribute,
  remainingPath?: PropertyPath,
): GetJSXAttributeResult {
  return {
    attribute: attribute,
    remainingPath: remainingPath,
  }
}

function getJSXAttributesAtPathParts(
  attributes: JSXAttributes,
  path: Array<PropertyPathPart>,
  pathIndex: number,
): GetJSXAttributeResult {
  switch (path.length - pathIndex) {
    case 0:
      throw new Error(`Cannot get attribute at empty path`)
    case 1: {
      const key = path[pathIndex]
      const attribute = getJSXAttribute(attributes, key)
      if (attribute == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        return getJSXAttributeResult(attribute)
      }
    }
    default: {
      const head = path[pathIndex]
      const attribute = getJSXAttribute(attributes, head)
      if (attribute == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        return getJSExpressionAtPathParts(attribute, path, pathIndex + 1)
      }
    }
  }
}

export function getJSExpressionAtPathParts(
  attribute: JSExpression,
  path: Array<PropertyPathPart>,
  pathIndex: number,
): GetJSXAttributeResult {
  switch (attribute.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JSX_ELEMENT':
      return getJSXAttributeResult(attribute, PP.createFromArray(path.slice(pathIndex)))
    case 'ATTRIBUTE_VALUE':
      const slicedPath = path.slice(pathIndex)
      if (ObjectPath.has(attribute.value, slicedPath)) {
        const extractedValue = ObjectPath.get(attribute.value, slicedPath)
        return getJSXAttributeResult(partOfJsxAttributeValue(extractedValue))
      } else {
        return getJSXAttributeResult(jsxAttributeNotFound())
      }
    case 'ATTRIBUTE_NESTED_OBJECT': {
      // We store objects similar to the TS compiler, so as an array of keys and values.
      // Duplicate keys will mean the last one overwrites, so we traverse backwards
      const nextKey = path[pathIndex]
      let foundProp: JSXProperty | undefined = undefined
      for (let contentIndex = attribute.content.length - 1; contentIndex >= 0; contentIndex--) {
        const prop = attribute.content[contentIndex]
        if (isPropertyAssignment(prop) && prop.key === nextKey) {
          foundProp = prop
          break
        }
      }
      if (foundProp == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        if (path.length - pathIndex <= 1) {
          return getJSXAttributeResult(foundProp.value)
        } else {
          return getJSExpressionAtPathParts(foundProp.value, path, pathIndex + 1)
        }
      }
    }
    case 'ATTRIBUTE_NESTED_ARRAY': {
      const possibleIndex = path[pathIndex]
      const index = typeof possibleIndex === 'number' ? possibleIndex : parseInt(possibleIndex)
      const foundProp = attribute.content[index]
      if (foundProp == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        if (path.length - pathIndex <= 1) {
          return getJSXAttributeResult(foundProp.value)
        } else {
          return getJSExpressionAtPathParts(foundProp.value, path, pathIndex + 1)
        }
      }
    }
    // FIXME: Do we invert these? Or should the representation be inverted?
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_IDENTIFIER':
      return getJSXAttributeResult(attribute, PP.createFromArray(path.slice(pathIndex)))
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Cannot resolve attribute ${JSON.stringify(attribute)}`)
  }
}

export function getJSXAttributesAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
): GetJSXAttributeResult {
  return getJSXAttributesAtPathParts(attributes, PP.getElements(path), 0)
}

export function jsxSimpleAttributeToValue(attribute: ModifiableAttribute): Either<string, any> {
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
      return right(undefined)
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JSX_MAP_EXPRESSION':
    case 'JSX_ELEMENT':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return left('Unable to get value from attribute.')
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
      return right(attribute.value)
    case 'JS_IDENTIFIER':
      return left('Unable to get value from identifier.')
    case 'JS_PROPERTY_ACCESS': {
      const simpleOnValue = jsxSimpleAttributeToValue(attribute.onValue)
      return flatMapEither((simplifiedOnValue) => {
        return right(simplifiedOnValue[attribute.property])
      }, simpleOnValue)
    }
    case 'JS_ELEMENT_ACCESS': {
      const simpleOnValue = jsxSimpleAttributeToValue(attribute.onValue)
      const simpleElement = jsxSimpleAttributeToValue(attribute.element)
      return applicative2Either(
        (simplifiedOnValue, simplifiedElement) => {
          return right(simplifiedOnValue[simplifiedElement])
        },
        simpleOnValue,
        simpleElement,
      )
    }
    case 'ATTRIBUTE_NESTED_ARRAY':
      let returnArray: Array<any> = []
      for (const elem of attribute.content) {
        const value = jsxSimpleAttributeToValue(elem.value)
        if (isLeft(value)) {
          return value
        } else {
          switch (elem.type) {
            case 'ARRAY_VALUE':
              returnArray.push(value.value)
              break
            case 'ARRAY_SPREAD':
              returnArray.push(...value.value)
              break
            default:
              assertNever(elem)
          }
        }
      }
      return right(returnArray)
    case 'ATTRIBUTE_NESTED_OBJECT':
      let returnObject: MapLike<any> = {}
      for (const prop of attribute.content) {
        const value = jsxSimpleAttributeToValue(prop.value)
        if (isLeft(value)) {
          return value
        } else {
          switch (prop.type) {
            case 'PROPERTY_ASSIGNMENT':
              returnObject[prop.key] = value.value
              break
            case 'SPREAD_ASSIGNMENT':
              returnObject = { ...returnObject, ...value.value }
              break
            default:
              assertNever(prop)
          }
        }
      }
      return right(returnObject)
    default:
      assertNever(attribute)
  }
}

export function setJSXValueAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
  value: JSExpression,
): Either<string, JSXAttributes> {
  return setJSXValueAtPathParts(attributes, PP.getElements(path), 0, value)
}

function setJSXValueAtPathParts(
  attributes: JSXAttributes,
  path: Array<PropertyPathPart>,
  pathIndex: number,
  value: JSExpression,
): Either<string, JSXAttributes> {
  const pathPart = path[pathIndex]
  const pathRemaining = path.length - pathIndex
  if (pathRemaining <= 0) {
    throw new Error('Attempted to manipulate attributes with an empty path.')
  } else if (pathRemaining === 1) {
    if (value === undefined) {
      const updatedAttributes = deleteJSXAttribute(attributes, pathPart)
      return right(updatedAttributes)
    } else {
      return right(setJSXAttributesAttribute(attributes, pathPart, value))
    }
  } else {
    const existingAttribute = getJSXAttribute(attributes, pathPart)
    if (existingAttribute == null) {
      return right(
        setJSXAttributesAttribute(
          attributes,
          pathPart,
          deeplyCreatedValue(path.slice(pathIndex + 1), value),
        ),
      )
    } else {
      const updatedAttribute = setJSXValueInAttributeAtPathParts(
        existingAttribute,
        path,
        pathIndex + 1,
        value,
      )
      return mapEither((updated) => {
        return setJSXAttributesAttribute(attributes, pathPart, updated)
      }, updatedAttribute)
    }
  }
}

const PositiveIntegerRegex = /^(0|[1-9]\d*)$/

function isArrayIndex(maybeIndex: string | number): maybeIndex is number {
  if (typeof maybeIndex === 'number') {
    return Number.isInteger(maybeIndex)
  } else if (typeof maybeIndex === 'string') {
    return PositiveIntegerRegex.test(maybeIndex)
  } else {
    return false
  }
}

export function setJSXValueInAttributeAtPath(
  attribute: JSExpression,
  path: PropertyPath,
  newAttrib: JSExpression,
): Either<string, JSExpression> {
  return setJSXValueInAttributeAtPathParts(attribute, PP.getElements(path), 0, newAttrib)
}

export function setJSXValueInAttributeAtPathParts(
  attribute: JSExpression,
  path: Array<PropertyPathPart>,
  pathIndex: number,
  newAttrib: JSExpression,
): Either<string, JSExpression> {
  if (path.length < pathIndex) {
    return left('Attempted to manipulate attribute with an empty path.')
  } else {
    const lastPartOfPath = pathIndex === path.length - 1
    switch (attribute.type) {
      case 'ATTRIBUTE_FUNCTION_CALL':
      case 'JSX_MAP_EXPRESSION':
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      case 'JS_IDENTIFIER':
      case 'JS_PROPERTY_ACCESS':
      case 'JS_ELEMENT_ACCESS':
      case 'JSX_ELEMENT':
        return left(
          `Attempted to set a value at ${PP.toString(
            PP.createFromArray(path.slice(pathIndex)),
          )} inside an ${JSON.stringify(attribute.type)}`,
        )
      case 'ATTRIBUTE_NESTED_ARRAY': {
        const attributeKey = path[pathIndex]
        if (attribute.content.some(isArraySpread)) {
          return left(`Unable to set an indexed value in an array containing spread elements`)
        }

        if (isArrayIndex(attributeKey)) {
          let newArray: Array<JSXArrayElement> = [...attribute.content]
          if (lastPartOfPath) {
            newArray[attributeKey] = jsxArrayValue(newAttrib, emptyComments)
          } else {
            const existingAttribute = attribute.content[attributeKey]
            if (existingAttribute == null) {
              newArray[attributeKey] = jsxArrayValue(
                deeplyCreatedValue(path.slice(pathIndex + 1), newAttrib),
                emptyComments,
              )
            } else {
              const updatedNestedAttribute = setJSXValueInAttributeAtPathParts(
                existingAttribute.value,
                path,
                pathIndex + 1,
                newAttrib,
              )

              if (isLeft(updatedNestedAttribute)) {
                return updatedNestedAttribute
              } else {
                newArray[attributeKey] = jsxArrayValue(updatedNestedAttribute.value, emptyComments)
              }
            }
          }

          return right(jsExpressionNestedArray(newArray, emptyComments))
        } else {
          // Convert the array to an object, which seems a little dubious.
          const newProps = attribute.content.map((attr, index) =>
            jsxPropertyAssignment(`${index}`, attr.value, emptyComments, emptyComments),
          )
          return setJSXValueInAttributeAtPathParts(
            jsExpressionNestedObject(newProps, emptyComments),
            path,
            pathIndex,
            newAttrib,
          )
        }
      }
      case 'ATTRIBUTE_NESTED_OBJECT': {
        const attributeKey = path[pathIndex]
        const key = `${attributeKey}`
        if (lastPartOfPath) {
          let updatedExistingProperty = false
          let updatedContent = attribute.content.flatMap((attr) => {
            if (attr.type === 'PROPERTY_ASSIGNMENT' && attr.key === key) {
              if (updatedExistingProperty) {
                return []
              } else {
                updatedExistingProperty = true
                return [jsxPropertyAssignment(key, newAttrib, emptyComments, emptyComments)]
              }
            } else {
              return [attr]
            }
          })
          if (updatedExistingProperty) {
            return right(jsExpressionNestedObject(updatedContent, emptyComments))
          } else {
            return right(
              jsExpressionNestedObject(
                attribute.content.concat(
                  jsxPropertyAssignment(key, newAttrib, emptyComments, emptyComments),
                ),
                emptyComments,
              ),
            )
          }
        } else {
          const newProps = dropKeyFromNestedObject(attribute, key).content
          const existingAttribute = nestedObjectValueForKey(attribute, key)
          const updatedNestedAttribute: Either<string, JSExpression> =
            existingAttribute.type === 'ATTRIBUTE_NOT_FOUND'
              ? right(deeplyCreatedValue(path.slice(pathIndex + 1), newAttrib))
              : setJSXValueInAttributeAtPathParts(existingAttribute, path, pathIndex + 1, newAttrib)

          return mapEither(
            (updated) =>
              jsExpressionNestedObject(
                newProps.concat(jsxPropertyAssignment(key, updated, emptyComments, emptyComments)),
                emptyComments,
              ),
            updatedNestedAttribute,
          )
        }
      }
      case 'ATTRIBUTE_VALUE':
        // we need to turn the attribute value into an ATTRIBUTE_NESTED_OBJECT
        const currentValue = attribute.value
        if (typeof currentValue === 'object') {
          // we are good, the current value is an object, we can insert into it
          if (Array.isArray(currentValue)) {
            // let's turn the found value into an ATTRIBUTE_NESTED_ARRAY
            const arrayifiedObject = jsExpressionNestedArray(
              currentValue.map((value) =>
                jsxArrayValue(jsExpressionValue(value, emptyComments), emptyComments),
              ),
              emptyComments,
            )
            return setJSXValueInAttributeAtPathParts(arrayifiedObject, path, pathIndex, newAttrib)
          } else {
            // let's turn the found object into a ATTRIBUTE_NESTED_OBJECT
            const nestedOject = jsExpressionNestedObject(
              Object.keys(currentValue).map((k) =>
                jsxPropertyAssignment(
                  k,
                  jsExpressionValue(currentValue[k], emptyComments),
                  emptyComments,
                  emptyComments,
                ),
              ),
              emptyComments,
            )
            return setJSXValueInAttributeAtPathParts(nestedOject, path, pathIndex, newAttrib)
          }
        }
        return left(`Attempted to deep update a value which is not an object`)
      default:
        const _exhaustiveCheck: never = attribute
        throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
    }
  }
}
