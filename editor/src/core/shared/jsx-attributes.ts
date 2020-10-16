import * as ObjectPath from 'object-path'
import { MapLike } from 'typescript'
import { UtopiaUtils } from 'utopia-api'
import { findLastIndex } from './array-utils'
import { Either, isLeft, left, mapEither, reduceWithEither, right, sequenceEither } from './either'
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
} from './element-template'
import { resolveParamsAndRunJsCode } from './javascript-cache'
import { objectMap } from './object-utils'
import { PropertyPath } from './project-file-types'
import * as PP from './property-path'
import { fastForEach } from './utils'

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

export const jsxAttributeToValue = (inScope: MapLike<any>, requireResult: MapLike<any>) => (
  attribute: JSXAttribute,
): any => {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return attribute.value
    case 'ATTRIBUTE_NESTED_ARRAY':
      let returnArray: Array<any> = []
      for (const elem of attribute.content) {
        const value = jsxAttributeToValue(inScope, requireResult)(elem.value)

        // We don't need to explicitly handle spreads because `concat` will take care of it for us
        returnArray = returnArray.concat(value)
      }

      return returnArray
    case 'ATTRIBUTE_NESTED_OBJECT':
      let returnObject: { [key: string]: any } = {}
      fastForEach(attribute.content, (prop) => {
        const value = jsxAttributeToValue(inScope, requireResult)(prop.value)

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
        const resolvedParameters = attribute.parameters.map(
          jsxAttributeToValue(inScope, requireResult),
        )
        return foundFunction(...resolvedParameters)
      }
      throw new Error(`Couldn't find helper function with name ${attribute.functionName}`)
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return resolveParamsAndRunJsCode(attribute, requireResult, inScope)
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function jsxAttributesToProps(
  inScope: MapLike<any>,
  attributes: JSXAttributes,
  requireResult: MapLike<any>,
): any {
  return objectMap(jsxAttributeToValue(inScope, requireResult), attributes)
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
    const pathString = PP.toString(path)
    if (ObjectPath.has(attribute.value, pathString)) {
      const extractedValue = ObjectPath.get(attribute.value, pathString)
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
    case 1:
      const key = PP.firstPart(path)
      if (key in attributes) {
        return getJSXAttributeResult(attributes[key])
      } else {
        return getJSXAttributeResult(jsxAttributeNotFound())
      }
    default:
      const head = PP.firstPart(path)
      const tail = PP.tail(path)
      if (head in attributes) {
        const headAttribute = attributes[head]
        return getJSXAttributeAtPathInner(headAttribute, tail)
      } else {
        return getJSXAttributeResult(jsxAttributeNotFound())
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
      const pathString = PP.toString(tail)
      if (ObjectPath.has(attribute.value, pathString)) {
        const extractedValue = ObjectPath.get(attribute.value, PP.toString(tail))
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
      if (foundProp != null) {
        const newTail = PP.tail(tail)
        if (PP.depth(newTail) === 0) {
          return getJSXAttributeResult(foundProp.value)
        } else {
          return getJSXAttributeAtPathInner(foundProp.value, newTail)
        }
      } else {
        return getJSXAttributeResult(jsxAttributeNotFound())
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
        const newTail = PP.tail(tail)
        if (PP.depth(newTail) === 0) {
          return getJSXAttributeResult(foundProp.value)
        } else {
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
    return jsxAttributeNestedObject([jsxPropertyAssignment(`${propName}`, acc)])
  }, value)
}

// TODO This function should absolutely be returning an Either since there is no guarantee that it
// won't be called with "illegal" params
export function setJSXValueInAttributeAtPath(
  attribute: JSXAttribute,
  path: PropertyPath,
  newAttrib: JSXAttribute,
): Either<string, JSXAttribute> {
  switch (PP.depth(path)) {
    case 0:
      return left('Attempted to manipulate attribute with an empty path.')
    default:
      const attributeKey = PP.firstPart(path)
      const tailPath = PP.tail(path)
      const lastPartOfPath = PP.depth(path) === 1
      switch (attribute.type) {
        case 'ATTRIBUTE_FUNCTION_CALL':
        case 'ATTRIBUTE_OTHER_JAVASCRIPT':
          return left(
            `Attempted to set a value at ${PP.toString(path)} inside an ${JSON.stringify(
              attribute.type,
            )}`,
          )
        case 'ATTRIBUTE_NESTED_ARRAY':
          if (attribute.content.some(isArraySpread)) {
            return left(`Unable to set an indexed value in an array containing spread elements`)
          }

          if (typeof attributeKey === 'number') {
            let newArray: Array<JSXArrayElement> = [...attribute.content]
            if (lastPartOfPath) {
              newArray[attributeKey] = jsxArrayValue(newAttrib)
            } else {
              const existingAttribute = attribute.content[attributeKey]
              if (existingAttribute == null) {
                newArray[attributeKey] = jsxArrayValue(deeplyCreatedValue(tailPath, newAttrib))
              } else {
                const updatedNestedAttribute = setJSXValueInAttributeAtPath(
                  existingAttribute.value,
                  tailPath,
                  newAttrib,
                )

                if (isLeft(updatedNestedAttribute)) {
                  return updatedNestedAttribute
                } else {
                  newArray[attributeKey] = jsxArrayValue(updatedNestedAttribute.value)
                }
              }
            }

            return right(jsxAttributeNestedArray(newArray))
          } else {
            // Convert the array to an object, which seems a little dubious.
            const newProps = attribute.content.map((attr, index) =>
              jsxPropertyAssignment(`${index}`, attr.value),
            )
            return setJSXValueInAttributeAtPath(
              jsxAttributeNestedObject(newProps),
              tailPath,
              newAttrib,
            )
          }
        case 'ATTRIBUTE_NESTED_OBJECT':
          const key = `${attributeKey}`
          const newProps = dropKeyFromNestedObject(attribute, key).content
          if (lastPartOfPath) {
            return right(
              jsxAttributeNestedObject(newProps.concat(jsxPropertyAssignment(key, newAttrib))),
            )
          } else {
            const existingAttribute = nestedObjectValueForKey(attribute, key)
            const updatedNestedAttribute: Either<string, JSXAttribute> =
              existingAttribute.type === 'ATTRIBUTE_NOT_FOUND'
                ? right(deeplyCreatedValue(tailPath, newAttrib))
                : setJSXValueInAttributeAtPath(existingAttribute, tailPath, newAttrib)

            return mapEither(
              (updated) =>
                jsxAttributeNestedObject(newProps.concat(jsxPropertyAssignment(key, updated))),
              updatedNestedAttribute,
            )
          }
        case 'ATTRIBUTE_VALUE':
          // we need to turn the attribute value into an ATTRIBUTE_NESTED_OBJECT
          const currentValue = attribute.value
          if (typeof currentValue === 'object') {
            // we are good, the current value is an object, we can insert into it
            if (Array.isArray(currentValue)) {
              // let's turn the found value into an ATTRIBUTE_NESTED_ARRAY
              const arrayifiedObject = jsxAttributeNestedArray(
                currentValue.map((value) => jsxArrayValue(jsxAttributeValue(value))),
              )
              return setJSXValueInAttributeAtPath(arrayifiedObject, path, newAttrib)
            } else {
              // let's turn the found object into a ATTRIBUTE_NESTED_OBJECT
              const nestedOject = jsxAttributeNestedObject(
                Object.keys(currentValue).map((k) =>
                  jsxPropertyAssignment(k, jsxAttributeValue(currentValue[k])),
                ),
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
  switch (PP.depth(path)) {
    case 0:
      throw new Error('Attempted to manipulate attributes with an empty path.')
    case 1:
      if (value === undefined) {
        let updatedAttributes = { ...attributes }
        delete updatedAttributes[attributeKey]
        return right(updatedAttributes)
      } else {
        return right({
          ...attributes,
          [attributeKey]: value,
        })
      }
    default:
      const tailPath = PP.tail(path)
      const existingAttribute = attributes[attributeKey]
      if (existingAttribute == null) {
        return right({
          ...attributes,
          [attributeKey]: deeplyCreatedValue(tailPath, value),
        })
      } else {
        const updatedAttribute = setJSXValueInAttributeAtPath(existingAttribute, tailPath, value)
        return mapEither((updated) => {
          return {
            ...attributes,
            [attributeKey]: updated,
          }
        }, updatedAttribute)
      }
  }
}

export interface ValueAtPath {
  path: PropertyPath
  value: JSXAttribute
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

export function unsetValueAtPath(value: any, path: PropertyPath): Either<string, any> {
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to manipulate value with an empty path.')
    default:
      const attributeKey = PP.firstPart(path)
      const tailPath = PP.tail(path)
      const lastPartOfPath = PP.depth(path) === 1
      if (typeof attributeKey === 'number' && typeof value === 'object' && Array.isArray(value)) {
        if (lastPartOfPath) {
          let newArray = [...value]
          newArray.splice(attributeKey, 1)
          return right(newArray)
        } else {
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
  return reduceWithEither(
    (working, path) => {
      return unsetJSXValueAtPath(working, path)
    },
    attributes,
    paths,
  )
}

export function unsetJSXValueInAttributeAtPath(
  attribute: JSXAttribute,
  path: PropertyPath,
): Either<string, JSXAttribute> {
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to manipulate attribute with an empty path.')
    default:
      const attributeKey = PP.firstPart(path)
      const tailPath = PP.tail(path)
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
              return right(jsxAttributeNestedArray(newArray))
            } else {
              const existingAttribute = attribute.content[attributeKey]
              if (existingAttribute == null) {
                return right(attribute)
              } else {
                const updatedNestedAttribute = unsetJSXValueInAttributeAtPath(
                  existingAttribute.value,
                  tailPath,
                )
                return mapEither((updated) => {
                  let newArray: Array<JSXArrayElement> = [...attribute.content]
                  newArray[attributeKey] = jsxArrayValue(updated)
                  return jsxAttributeNestedArray(newArray)
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
              const existingAttribute = attribute.content[existingAttributeIndex]
              const updatedAttribute = unsetJSXValueInAttributeAtPath(
                existingAttribute.value,
                tailPath,
              )
              return mapEither((updated) => {
                let newProps: Array<JSXProperty> = [...attribute.content]
                newProps[existingAttributeIndex] = jsxPropertyAssignment(key, updated)
                return jsxAttributeNestedObject(newProps)
              }, updatedAttribute)
            }
          }
        case 'ATTRIBUTE_VALUE':
          const updatedValue = unsetValueAtPath(attribute.value, path)
          return mapEither((updated) => {
            return jsxAttributeValue(updated)
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
  switch (PP.depth(path)) {
    case 0:
      // As this is invalid throw an exception.
      throw new Error('Attempted to unset something from attributes with an empty path.')
    case 1:
      let updatedAttributes = { ...attributes }
      delete updatedAttributes[attributeKey]
      return right(updatedAttributes)
    default:
      const tailPath = PP.tail(path)
      const existingAttribute = attributes[attributeKey]
      if (existingAttribute == null) {
        return right(attributes)
      } else {
        const updatedAttribute: Either<string, JSXAttribute> = unsetJSXValueInAttributeAtPath(
          existingAttribute,
          tailPath,
        )
        return mapEither((updated) => {
          return {
            ...attributes,
            [attributeKey]: updated,
          }
        }, updatedAttribute)
      }
  }
}

export function walkAttribute(attribute: JSXAttribute, walk: (a: JSXAttribute) => void): void {
  walk(attribute)
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      fastForEach(attribute.content, (elem) => {
        walkAttribute(elem.value, walk)
      })
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      fastForEach(attribute.parameters, (param) => {
        walkAttribute(param, walk)
      })
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      fastForEach(attribute.content, (elem) => {
        walkAttribute(elem.value, walk)
      })
      break
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function walkAttributes(
  attributes: JSXAttributes,
  walk: (attribute: JSXAttribute) => void,
): void {
  fastForEach(Object.keys(attributes), (attrKey) => {
    const attribute = attributes[attrKey]
    walkAttribute(attribute, walk)
  })
}
