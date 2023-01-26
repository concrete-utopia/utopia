import * as ObjectPath from 'object-path'
import { MapLike } from 'typescript'
import { UtopiaUtils } from 'utopia-api/core'
import { findLastIndex, uniqBy } from './array-utils'
import {
  Either,
  isLeft,
  isRight,
  left,
  mapEither,
  reduceWithEither,
  right,
  sequenceEither,
} from './either'
import {
  isArraySpread,
  isPropertyAssignment,
  JSXArrayElement,
  jsxArrayValue,
  JSXAttribute,
  jsxAttributeNestedArray,
  jsxAttributeNestedObject,
  JSXAttributeNestedObject,
  JSXAttributeNotFound,
  jsxAttributeNotFound,
  JSXAttributes,
  jsxAttributeValue,
  JSXProperty,
  jsxPropertyAssignment,
  partOfJsxAttributeValue,
  PartOfJSXAttributeValue,
  isPartOfJSXAttributeValue,
  isJSXAttributeNotFound,
  getJSXAttribute,
  deleteJSXAttribute,
  setJSXAttributesAttribute,
  isJSXAttributeValue,
  simplifyAttributesIfPossible,
  ElementsWithin,
  isJSXAttributeOtherJavaScript,
  emptyComments,
  jsxAttributeNestedArraySimple,
  JSXAttributesPart,
} from './element-template'
import { resolveParamsAndRunJsCode } from './javascript-cache'
import { PropertyPath } from './project-file-types'
import * as PP from './property-path'
import { fastForEach } from './utils'
import { optionalMap } from './optional-utils'
import { getAllObjectPaths } from './object-utils'

export type AnyMap = { [key: string]: any }

export function nestedObjectValueForKey(
  nestedObject: JSXAttributeNestedObject,
  key: string,
): JSXAttribute | JSXAttributeNotFound {
  const value = nestedObject.content.find((prop) => isPropertyAssignment(prop) && prop.key === key)
  return value == null ? jsxAttributeNotFound() : value.value
}

export function dropKeyFromNestedObject(
  nestedObject: JSXAttributeNestedObject,
  key: string,
): JSXAttributeNestedObject {
  return jsxAttributeNestedObject(
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

export function jsxSimpleAttributeToValue(attribute: ModifiableAttribute): Either<string, any> {
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
      return right(undefined)
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return left('Unable to get value from attribute.')
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
      return right(attribute.value)
    case 'ATTRIBUTE_NESTED_ARRAY':
      let returnArray: Array<any> = []
      for (const elem of attribute.content) {
        const value = jsxSimpleAttributeToValue(elem.value)
        if (isLeft(value)) {
          return value
        } else {
          // We don't need to explicitly handle spreads because `concat` will take care of it for us
          returnArray = returnArray.concat(value.value)
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
              const _exhaustiveCheck: never = prop
              throw new Error(`Unhandled prop type ${JSON.stringify(prop)}`)
          }
        }
      }
      return right(returnObject)
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function jsxFunctionAttributeToValue(
  attribute: ModifiableAttribute,
): Either<null, { functionName: string; parameters: Array<any> }> {
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
      return left(null)
    case 'ATTRIBUTE_FUNCTION_CALL':
      const extractedSimpleValueParameters = sequenceEither(
        attribute.parameters.map(jsxSimpleAttributeToValue),
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
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${attribute}`)
  }
}

export function jsxFunctionAttributeToRawValue(
  attribute: ModifiableAttribute,
): Either<null, { functionName: string; parameters: Array<any> }> {
  switch (attribute.type) {
    case 'ATTRIBUTE_NOT_FOUND':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
      return left(null)
    case 'ATTRIBUTE_FUNCTION_CALL':
      return right({
        functionName: attribute.functionName,
        parameters: attribute.parameters,
      })
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${attribute}`)
  }
}

export function jsxAttributeToValue(
  filePath: string,
  inScope: MapLike<any>,
  requireResult: MapLike<any>,
  attribute: JSXAttribute,
): any {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return attribute.value
    case 'ATTRIBUTE_NESTED_ARRAY':
      let returnArray: Array<any> = []
      for (const elem of attribute.content) {
        const value = jsxAttributeToValue(filePath, inScope, requireResult, elem.value)

        // We don't need to explicitly handle spreads because `concat` will take care of it for us
        returnArray = returnArray.concat(value)
      }

      return returnArray
    case 'ATTRIBUTE_NESTED_OBJECT':
      let returnObject: { [key: string]: any } = {}
      fastForEach(attribute.content, (prop) => {
        const value = jsxAttributeToValue(filePath, inScope, requireResult, prop.value)

        switch (prop.type) {
          case 'PROPERTY_ASSIGNMENT':
            returnObject[prop.key] = value
            break
          case 'SPREAD_ASSIGNMENT':
            returnObject = { ...returnObject, ...value }
            break
          default:
            const _exhaustiveCheck: never = prop
            throw new Error(`Unhandled prop type ${prop}`)
        }
      })

      return returnObject
    case 'ATTRIBUTE_FUNCTION_CALL':
      const foundFunction = (UtopiaUtils as any)[attribute.functionName]
      if (foundFunction != null) {
        const resolvedParameters = attribute.parameters.map((param) =>
          jsxAttributeToValue(filePath, inScope, requireResult, param),
        )
        return foundFunction(...resolvedParameters)
      }
      throw new Error(`Couldn't find helper function with name ${attribute.functionName}`)
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return resolveParamsAndRunJsCode(filePath, attribute, requireResult, inScope)
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function jsxAttributesToProps(
  filePath: string,
  inScope: MapLike<any>,
  attributes: JSXAttributes,
  requireResult: MapLike<any>,
): any {
  let result: any = {}
  for (const entry of attributes) {
    switch (entry.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        result[entry.key] = jsxAttributeToValue(filePath, inScope, requireResult, entry.value)
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        result = {
          ...result,
          ...jsxAttributeToValue(filePath, inScope, requireResult, entry.spreadValue),
        }
        break
      default:
        const _exhaustiveCheck: never = entry
        throw new Error(`Unhandled entry ${JSON.stringify(entry)}`)
    }
  }
  return result
}

export type GetModifiableAttributeResult = Either<string, ModifiableAttribute>

export type ModifiableAttribute = JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound

export type GetJSXAttributeResult = {
  attribute: ModifiableAttribute
  remainingPath?: PropertyPath
}

function getJSXAttributeResult(
  attribute: ModifiableAttribute,
  remainingPath?: PropertyPath,
): GetJSXAttributeResult {
  return {
    attribute: attribute,
    remainingPath: remainingPath,
  }
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
  const result = getJSXAttributeAtPath(attributes, path)

  if (result.remainingPath == null) {
    return right(result.attribute)
  } else {
    return left(`Found non-value attribute ${result.attribute.type}`)
  }
}

// FIXME The naming here is abysmal
export function getModifiableJSXAttributeAtPathFromAttribute(
  attribute: ModifiableAttribute,
  path: PropertyPath,
): GetModifiableAttributeResult {
  if (isJSXAttributeNotFound(attribute)) {
    return right(jsxAttributeNotFound())
  } else if (isPartOfJSXAttributeValue(attribute)) {
    const pathElems = PP.getElements(path)
    if (ObjectPath.has(attribute.value, pathElems)) {
      const extractedValue = ObjectPath.get(attribute.value, pathElems)
      return right(partOfJsxAttributeValue(extractedValue))
    } else {
      return right(jsxAttributeNotFound())
    }
  } else {
    const result = getJSXAttributeAtPathInner(attribute, path)

    if (result.remainingPath == null) {
      return right(result.attribute)
    } else {
      return left(`Found non-value attribute ${result.attribute.type}`)
    }
  }
}

export function getJSXAttributeAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
): GetJSXAttributeResult {
  switch (PP.depth(path)) {
    case 0:
      throw new Error(`Cannot get attribute at empty path`)
    case 1: {
      const key = PP.firstPart(path)
      const keyAsString = typeof key === 'string' ? key : `${key}`
      const attribute = getJSXAttribute(attributes, keyAsString)
      if (attribute == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        return getJSXAttributeResult(attribute)
      }
    }
    default: {
      const head = PP.firstPart(path)
      const headAsString = typeof head === 'string' ? head : `${head}`
      const attribute = getJSXAttribute(attributes, headAsString)
      if (attribute == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        const tail = PP.tail(path)
        return getJSXAttributeAtPathInner(attribute, tail)
      }
    }
  }
}

export function getJSXAttributeAtPathInner(
  attribute: JSXAttribute,
  tail: PropertyPath,
): GetJSXAttributeResult {
  switch (attribute.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return getJSXAttributeResult(attribute, tail)
    case 'ATTRIBUTE_VALUE':
      const pathElems = PP.getElements(tail)
      if (ObjectPath.has(attribute.value, pathElems)) {
        const extractedValue = ObjectPath.get(attribute.value, pathElems)
        return getJSXAttributeResult(partOfJsxAttributeValue(extractedValue))
      } else {
        return getJSXAttributeResult(jsxAttributeNotFound())
      }
    case 'ATTRIBUTE_NESTED_OBJECT': {
      // We store objects similar to the TS compiler, so as an array of keys and values.
      // Duplicate keys will mean the last one overwrites, so we traverse backwards
      const nextKey = PP.firstPart(tail)
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
        if (PP.depth(tail) <= 1) {
          return getJSXAttributeResult(foundProp.value)
        } else {
          const newTail = PP.tail(tail)
          return getJSXAttributeAtPathInner(foundProp.value, newTail)
        }
      }
    }
    case 'ATTRIBUTE_NESTED_ARRAY': {
      const possibleIndex = PP.firstPart(tail)
      const index = typeof possibleIndex === 'number' ? possibleIndex : parseInt(possibleIndex)
      if (isNaN(index)) {
        throw new Error(`Attempted to access an array item at index ${possibleIndex}`)
      }
      const foundProp = attribute.content[index]
      if (foundProp == null) {
        return getJSXAttributeResult(jsxAttributeNotFound())
      } else {
        if (PP.depth(tail) <= 1) {
          return getJSXAttributeResult(foundProp.value)
        } else {
          const newTail = PP.tail(tail)
          return getJSXAttributeAtPathInner(foundProp.value, newTail)
        }
      }
    }
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Cannot resolve attribute ${JSON.stringify(attribute)}`)
  }
}

export function deeplyCreatedValue(path: PropertyPath, value: JSXAttribute): JSXAttribute {
  const elements = PP.getElements(path)
  return elements.reduceRight((acc: JSXAttribute, propName) => {
    if (typeof propName === 'number') {
      let newArray: Array<JSXAttribute> = []
      newArray[propName] = acc
      return jsxAttributeNestedArraySimple(newArray)
    } else {
      return jsxAttributeNestedObject(
        [jsxPropertyAssignment(propName, acc, emptyComments, emptyComments)],
        emptyComments,
      )
    }
  }, value)
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
  attribute: JSXAttribute,
  path: PropertyPath,
  newAttrib: JSXAttribute,
): Either<string, JSXAttribute> {
  switch (PP.depth(path)) {
    case 0:
      return left('Attempted to manipulate attribute with an empty path.')
    default:
      const lastPartOfPath = PP.depth(path) === 1
      switch (attribute.type) {
        case 'ATTRIBUTE_FUNCTION_CALL':
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
          return left(
            `Attempted to set a value at ${PP.toString(path)} inside an ${JSON.stringify(
              attribute.type,
            )}`,
          )
        case 'ATTRIBUTE_NESTED_ARRAY': {
          const attributeKey = PP.firstPart(path)
          const tailPath = PP.tail(path)
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
                  deeplyCreatedValue(tailPath, newAttrib),
                  emptyComments,
                )
              } else {
                const updatedNestedAttribute = setJSXValueInAttributeAtPath(
                  existingAttribute.value,
                  tailPath,
                  newAttrib,
                )

                if (isLeft(updatedNestedAttribute)) {
                  return updatedNestedAttribute
                } else {
                  newArray[attributeKey] = jsxArrayValue(
                    updatedNestedAttribute.value,
                    emptyComments,
                  )
                }
              }
            }

            return right(jsxAttributeNestedArray(newArray, emptyComments))
          } else {
            // Convert the array to an object, which seems a little dubious.
            const newProps = attribute.content.map((attr, index) =>
              jsxPropertyAssignment(`${index}`, attr.value, emptyComments, emptyComments),
            )
            return setJSXValueInAttributeAtPath(
              jsxAttributeNestedObject(newProps, emptyComments),
              path,
              newAttrib,
            )
          }
        }
        case 'ATTRIBUTE_NESTED_OBJECT': {
          const attributeKey = PP.firstPart(path)
          const tailPath = PP.tail(path)
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
              return right(jsxAttributeNestedObject(updatedContent, emptyComments))
            } else {
              return right(
                jsxAttributeNestedObject(
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
            const updatedNestedAttribute: Either<string, JSXAttribute> =
              existingAttribute.type === 'ATTRIBUTE_NOT_FOUND'
                ? right(deeplyCreatedValue(tailPath, newAttrib))
                : setJSXValueInAttributeAtPath(existingAttribute, tailPath, newAttrib)

            return mapEither(
              (updated) =>
                jsxAttributeNestedObject(
                  newProps.concat(
                    jsxPropertyAssignment(key, updated, emptyComments, emptyComments),
                  ),
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
              const arrayifiedObject = jsxAttributeNestedArray(
                currentValue.map((value) =>
                  jsxArrayValue(jsxAttributeValue(value, emptyComments), emptyComments),
                ),
                emptyComments,
              )
              return setJSXValueInAttributeAtPath(arrayifiedObject, path, newAttrib)
            } else {
              // let's turn the found object into a ATTRIBUTE_NESTED_OBJECT
              const nestedOject = jsxAttributeNestedObject(
                Object.keys(currentValue).map((k) =>
                  jsxPropertyAssignment(
                    k,
                    jsxAttributeValue(currentValue[k], emptyComments),
                    emptyComments,
                    emptyComments,
                  ),
                ),
                emptyComments,
              )
              return setJSXValueInAttributeAtPath(nestedOject, path, newAttrib)
            }
          }
          return left(`Attempted to deep update a value which is not an object`)
        default:
          const _exhaustiveCheck: never = attribute
          throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
      }
  }
}

export function setJSXValueAtPath(
  attributes: JSXAttributes,
  path: PropertyPath,
  value: JSXAttribute,
): Either<string, JSXAttributes> {
  const attributeKey = PP.firstPart(path)
  const attributeKeyAsString = typeof attributeKey === 'string' ? attributeKey : `${attributeKey}`
  switch (PP.depth(path)) {
    case 0:
      throw new Error('Attempted to manipulate attributes with an empty path.')
    case 1:
      if (value === undefined) {
        const updatedAttributes = deleteJSXAttribute(attributes, attributeKeyAsString)
        return right(updatedAttributes)
      } else {
        return right(setJSXAttributesAttribute(attributes, attributeKeyAsString, value))
      }
    default:
      const tailPath = PP.tail(path)
      const existingAttribute = getJSXAttribute(attributes, attributeKeyAsString)
      if (existingAttribute == null) {
        return right(
          setJSXAttributesAttribute(
            attributes,
            attributeKeyAsString,
            deeplyCreatedValue(tailPath, value),
          ),
        )
      } else {
        const updatedAttribute = setJSXValueInAttributeAtPath(existingAttribute, tailPath, value)
        return mapEither((updated) => {
          return setJSXAttributesAttribute(attributes, attributeKeyAsString, updated)
        }, updatedAttribute)
      }
  }
}

export interface ValueAtPath {
  path: PropertyPath
  value: JSXAttribute
}
export function valueAtPath(path: PropertyPath, value: JSXAttribute): ValueAtPath {
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
  attribute: JSXAttribute,
  path: PropertyPath,
): Either<string, JSXAttribute> {
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to manipulate attribute with an empty path.')
    default:
      const attributeKey = PP.firstPart(path)
      const lastPartOfPath = PP.depth(path) === 1
      switch (attribute.type) {
        case 'ATTRIBUTE_FUNCTION_CALL':
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
          return left('Cannot unset a value set inside a value set from elsewhere.')
        case 'ATTRIBUTE_NESTED_ARRAY':
          if (typeof attributeKey === 'number') {
            if (attribute.content.some(isArraySpread)) {
              return left('Cannot unset a value in an array containing a spread')
            }

            if (lastPartOfPath) {
              let newArray: Array<JSXArrayElement> = [...attribute.content]
              newArray.splice(attributeKey, 1)
              return right(jsxAttributeNestedArray(newArray, emptyComments))
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
                  return jsxAttributeNestedArray(newArray, emptyComments)
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
                return jsxAttributeNestedObject(newProps, emptyComments)
              }, updatedAttribute)
            }
          }
        case 'ATTRIBUTE_VALUE':
          const updatedValue = unsetValueAtPath(attribute.value, path)
          return mapEither((updated) => {
            return jsxAttributeValue(updated, emptyComments)
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
        const updatedAttribute: Either<string, JSXAttribute> = unsetJSXValueInAttributeAtPath(
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
  attribute: JSXAttribute,
  path: PropertyPath | null,
  walk: (a: JSXAttribute, path: PropertyPath | null) => void,
): void {
  walk(attribute, path)
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
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
    if (isJSXAttributeOtherJavaScript(attribute)) {
      Object.assign(elementsWithinAccumulator, attribute.elementsWithin)
    }
  })

  return elementsWithinAccumulator
}

function walkAttributes(
  attributes: JSXAttributes,
  walk: (attribute: JSXAttribute, path: PropertyPath | null) => void,
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
  const possibleProperty = getJSXAttributeAtPath(props, property)
  const currentValue = optionalMap(jsxSimpleAttributeToValue, possibleProperty?.attribute)
  if (currentValue !== null && isRight(currentValue) && typeof currentValue.value === 'number') {
    return currentValue.value
  } else {
    return null
  }
}
