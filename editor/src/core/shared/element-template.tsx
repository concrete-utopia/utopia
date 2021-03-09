import {
  PropertyPath,
  InstancePath,
  PropertyPathPart,
  ScenePath,
  StaticElementPath,
} from './project-file-types'
import { CanvasRectangle, LocalRectangle, LocalPoint, zeroCanvasRect } from './math-utils'
import { Either, isLeft } from './either'
import { v4 as UUID } from 'uuid'
import { RawSourceMap } from '../workers/ts/ts-typings/RawSourceMap'
import * as PP from './property-path'
import { Sides, sides, NormalisedFrame, LayoutSystem } from 'utopia-api'
import { fastForEach, unknownObjectProperty } from './utils'
import { addAllUniquely, mapDropNulls } from './array-utils'
import { objectMap } from './object-utils'
import { parseUID } from './uid-utils'
import { CSSPosition } from '../../components/inspector/common/css-utils'
import { ModifiableAttribute } from './jsx-attributes'
import * as TP from './template-path'
import { firstLetterIsLowerCase } from './string-utils'
import { intrinsicHTMLElementNamesAsStrings } from './dom-utils'
import {
  emptyComments,
  isParsedCommentsEmpty,
  ParsedComments,
} from '../workers/parser-printer/parser-printer-comments'
import { MapLike } from 'typescript'
import { forceNotNull } from './optional-utils'

interface BaseComment {
  comment: string
  rawText: string
  trailingNewLine: boolean
  pos: number | null
}

export interface MultiLineComment extends BaseComment {
  type: 'MULTI_LINE_COMMENT'
}

export function multiLineComment(
  comment: string,
  rawText: string,
  trailingNewLine: boolean,
  pos: number | null,
): MultiLineComment {
  return {
    type: 'MULTI_LINE_COMMENT',
    comment: comment,
    rawText: rawText,
    trailingNewLine: trailingNewLine,
    pos: pos,
  }
}

export interface SingleLineComment extends BaseComment {
  type: 'SINGLE_LINE_COMMENT'
}

export function singleLineComment(
  comment: string,
  rawText: string,
  trailingNewLine: boolean,
  pos: number | null,
): SingleLineComment {
  return {
    type: 'SINGLE_LINE_COMMENT',
    comment: comment,
    rawText: rawText,
    trailingNewLine: trailingNewLine,
    pos: pos,
  }
}

export type Comment = MultiLineComment | SingleLineComment

export function isMultiLineComment(comment: Comment): comment is MultiLineComment {
  return comment.type === 'MULTI_LINE_COMMENT'
}

export function isSingleLineComment(comment: Comment): comment is SingleLineComment {
  return comment.type === 'SINGLE_LINE_COMMENT'
}

export interface WithComments {
  comments: ParsedComments
}

export interface JSXAttributeValue<T> extends WithComments {
  type: 'ATTRIBUTE_VALUE'
  value: T
}

export function jsxAttributeValue<T>(value: T, comments: ParsedComments): JSXAttributeValue<T> {
  return {
    type: 'ATTRIBUTE_VALUE',
    value: value,
    comments: comments,
  }
}

/**
 If the contents of an `ATTRIBUTE_VALUE` is an object (whose values are not `JSXAttribute`s), we might still want to
 dig directly into them. This is the return type for when we dig into the `JSXAttribute`s using a path, which
 results in digging directly into an `ATTRIBUTE_VALUE` somewhere along the way.
 
 For example: `<View style={backgroundColor: 'red'} />`
 Here `style` will be an `ATTRIBUTE_VALUE`, whose value is the object `{backgroundColor: 'red'}`
 Then, if we attempt to read `style.backgroundColor`, because we are digging into the contents of the `style` `ATTRIBUTE_VALUE`
 we will get a `PART_OF_ATTRIBUTE_VALUE`, whose value is `'red'`. If alternatively `style` were an `ATTRIBUTE_NESTED_OBJECT`, then
 each of its contents would all be a `JSXAttribute`, in which case `'red'` might be an `ATTRIBUTE_VALUE`
 */
export interface PartOfJSXAttributeValue {
  type: 'PART_OF_ATTRIBUTE_VALUE'
  value: any
}

export function partOfJsxAttributeValue(value: any): PartOfJSXAttributeValue {
  return {
    type: 'PART_OF_ATTRIBUTE_VALUE',
    value: value,
  }
}

export interface JSXAttributeNotFound {
  type: 'ATTRIBUTE_NOT_FOUND'
}

export function jsxAttributeNotFound(): JSXAttributeNotFound {
  return {
    type: 'ATTRIBUTE_NOT_FOUND',
  }
}

export interface JSXAttributeOtherJavaScript {
  type: 'ATTRIBUTE_OTHER_JAVASCRIPT'
  javascript: string
  transpiledJavascript: string
  definedElsewhere: Array<string>
  sourceMap: RawSourceMap | null
  uniqueID: string
}

export function jsxAttributeOtherJavaScript(
  javascript: string,
  transpiledJavascript: string,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
): JSXAttributeOtherJavaScript {
  return {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uniqueID: UUID(),
  }
}

export interface JSXSpreadAssignment extends WithComments {
  type: 'SPREAD_ASSIGNMENT'
  value: JSXAttribute
}

export function jsxSpreadAssignment(
  value: JSXAttribute,
  comments: ParsedComments,
): JSXSpreadAssignment {
  return {
    type: 'SPREAD_ASSIGNMENT',
    value: value,
    comments: comments,
  }
}

export interface JSXPropertyAssignment extends WithComments {
  type: 'PROPERTY_ASSIGNMENT'
  key: string
  value: JSXAttribute
  keyComments: ParsedComments
}

export function jsxPropertyAssignment(
  key: string,
  value: JSXAttribute,
  comments: ParsedComments,
  keyComments: ParsedComments,
): JSXPropertyAssignment {
  return {
    type: 'PROPERTY_ASSIGNMENT',
    key: key,
    value: value,
    comments: comments,
    keyComments: keyComments,
  }
}

export type JSXProperty = JSXPropertyAssignment | JSXSpreadAssignment

export function isSpreadAssignment(property: JSXProperty): property is JSXSpreadAssignment {
  return property.type === 'SPREAD_ASSIGNMENT'
}

export function isPropertyAssignment(property: JSXProperty): property is JSXPropertyAssignment {
  return property.type === 'PROPERTY_ASSIGNMENT'
}

export interface JSXAttributeNestedObject extends WithComments {
  type: 'ATTRIBUTE_NESTED_OBJECT'
  content: Array<JSXProperty>
}

export function jsxAttributeNestedObject(
  content: Array<JSXProperty>,
  comments: ParsedComments,
): JSXAttributeNestedObject {
  return {
    type: 'ATTRIBUTE_NESTED_OBJECT',
    content: content,
    comments: comments,
  }
}

export function jsxAttributeNestedObjectSimple(
  content: JSXAttributes,
  comments: ParsedComments,
): JSXAttributeNestedObject {
  return {
    type: 'ATTRIBUTE_NESTED_OBJECT',
    content: content.map((elem) =>
      jsxPropertyAssignment(elem.key, elem.value, emptyComments, emptyComments),
    ),
    comments: comments,
  }
}

export interface JSXArrayValue extends WithComments {
  type: 'ARRAY_VALUE'
  value: JSXAttribute
}

export function jsxArrayValue(value: JSXAttribute, comments: ParsedComments): JSXArrayValue {
  return {
    type: 'ARRAY_VALUE',
    value: value,
    comments: comments,
  }
}

export interface JSXArraySpread extends WithComments {
  type: 'ARRAY_SPREAD'
  value: JSXAttribute
}

export function jsxArraySpread(value: JSXAttribute, comments: ParsedComments): JSXArraySpread {
  return {
    type: 'ARRAY_SPREAD',
    value: value,
    comments: comments,
  }
}

export type JSXArrayElement = JSXArrayValue | JSXArraySpread

export function isArrayValue(elem: JSXArrayElement): elem is JSXArrayValue {
  return elem.type === 'ARRAY_VALUE'
}

export function isArraySpread(elem: JSXArrayElement): elem is JSXArraySpread {
  return elem.type === 'ARRAY_SPREAD'
}

export interface JSXAttributeNestedArray extends WithComments {
  type: 'ATTRIBUTE_NESTED_ARRAY'
  content: Array<JSXArrayElement>
}

export function jsxAttributeNestedArray(
  content: Array<JSXArrayElement>,
  comments: ParsedComments,
): JSXAttributeNestedArray {
  return {
    type: 'ATTRIBUTE_NESTED_ARRAY',
    content: content,
    comments: comments,
  }
}

export function jsxAttributeNestedArraySimple(
  content: Array<JSXAttribute>,
): JSXAttributeNestedArray {
  return jsxAttributeNestedArray(
    content.map((value) => jsxArrayValue(value, emptyComments)),
    emptyComments,
  )
}

export interface JSXAttributeFunctionCall {
  type: 'ATTRIBUTE_FUNCTION_CALL'
  functionName: string
  parameters: Array<JSXAttribute>
}

export function jsxAttributeFunctionCall(
  functionName: string,
  parameters: Array<JSXAttribute>,
): JSXAttributeFunctionCall {
  return {
    type: 'ATTRIBUTE_FUNCTION_CALL',
    functionName: functionName,
    parameters: parameters,
  }
}

export type JSXAttribute =
  | JSXAttributeValue<any>
  | JSXAttributeOtherJavaScript
  | JSXAttributeNestedArray
  | JSXAttributeNestedObject
  | JSXAttributeFunctionCall

export function clearJSXAttributeOtherJavaScriptUniqueIDs(
  attribute: JSXAttributeOtherJavaScript,
): JSXAttributeOtherJavaScript {
  return {
    ...attribute,
    uniqueID: '',
  }
}

export function simplifyAttributeIfPossible(attribute: JSXAttribute): JSXAttribute {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return attribute
    case 'ATTRIBUTE_NESTED_ARRAY':
      let simpleArray: Array<unknown> = []
      let notSoSimpleArray: Array<JSXArrayElement> = []
      let isSimpleArray: boolean = true
      for (const elem of attribute.content) {
        const simplifiedAttribute = simplifyAttributeIfPossible(elem.value)
        switch (elem.type) {
          case 'ARRAY_SPREAD':
            notSoSimpleArray.push(jsxArraySpread(simplifiedAttribute, elem.comments))
            break
          case 'ARRAY_VALUE':
            notSoSimpleArray.push(jsxArrayValue(simplifiedAttribute, elem.comments))
            break
          default:
            const _exhaustiveCheck: never = elem
            throw new Error(`Unhandled elem ${JSON.stringify(elem)}`)
        }
        if (
          isSimpleArray &&
          isParsedCommentsEmpty(elem.comments) &&
          isJSXAttributeValue(simplifiedAttribute)
        ) {
          simpleArray.push(simplifiedAttribute.value)
        } else {
          isSimpleArray = false
        }
      }
      if (isSimpleArray) {
        return jsxAttributeValue(simpleArray, attribute.comments)
      } else {
        return jsxAttributeNestedArray(notSoSimpleArray, attribute.comments)
      }
    case 'ATTRIBUTE_NESTED_OBJECT':
      let simpleObject: MapLike<unknown> = {}
      let notSoSimpleObject: Array<JSXProperty> = []
      let isSimpleObject: boolean = true
      for (const elem of attribute.content) {
        const simplifiedAttribute = simplifyAttributeIfPossible(elem.value)
        switch (elem.type) {
          case 'SPREAD_ASSIGNMENT': {
            notSoSimpleObject.push(jsxSpreadAssignment(simplifiedAttribute, elem.comments))
            if (isSimpleObject) {
              const noComments = isParsedCommentsEmpty(elem.comments)
              if (isJSXAttributeValue(simplifiedAttribute) && noComments) {
                simpleObject = {
                  ...simpleObject,
                  ...simplifiedAttribute.value,
                }
              } else {
                isSimpleObject = false
              }
            }
            break
          }
          case 'PROPERTY_ASSIGNMENT': {
            notSoSimpleObject.push(
              jsxPropertyAssignment(elem.key, simplifiedAttribute, elem.comments, elem.keyComments),
            )
            if (isSimpleObject) {
              const noComments =
                isParsedCommentsEmpty(elem.comments) && isParsedCommentsEmpty(elem.keyComments)
              if (isJSXAttributeValue(simplifiedAttribute) && noComments) {
                simpleObject[elem.key] = simplifiedAttribute.value
              } else {
                isSimpleObject = false
              }
            }
            break
          }
          default:
            const _exhaustiveCheck: never = elem
            throw new Error(`Unhandled elem ${JSON.stringify(elem)}`)
        }
      }
      if (isSimpleObject) {
        return jsxAttributeValue(simpleObject, attribute.comments)
      } else {
        return jsxAttributeNestedObject(notSoSimpleObject, attribute.comments)
      }
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function clearAttributeUniqueIDs(attribute: JSXAttribute): JSXAttribute {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return attribute
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return clearJSXAttributeOtherJavaScriptUniqueIDs(attribute)
    case 'ATTRIBUTE_NESTED_ARRAY':
      return jsxAttributeNestedArray(
        attribute.content.map((elem) => {
          switch (elem.type) {
            case 'ARRAY_SPREAD':
              return jsxArraySpread(clearAttributeUniqueIDs(elem.value), elem.comments)
            case 'ARRAY_VALUE':
              return jsxArrayValue(clearAttributeUniqueIDs(elem.value), elem.comments)
            default:
              const _exhaustiveCheck: never = elem
              throw new Error(`Unhandled array element type ${JSON.stringify(elem)}`)
          }
        }),
        attribute.comments,
      )
    case 'ATTRIBUTE_FUNCTION_CALL':
      return jsxAttributeFunctionCall(
        attribute.functionName,
        attribute.parameters.map(clearAttributeUniqueIDs),
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      return jsxAttributeNestedObject(
        attribute.content.map((prop) => {
          switch (prop.type) {
            case 'SPREAD_ASSIGNMENT':
              return jsxSpreadAssignment(clearAttributeUniqueIDs(prop.value), prop.comments)
            case 'PROPERTY_ASSIGNMENT':
              return jsxPropertyAssignment(
                prop.key,
                clearAttributeUniqueIDs(prop.value),
                prop.comments,
                prop.keyComments,
              )
            default:
              const _exhaustiveCheck: never = prop
              throw new Error(`Unhandled property type ${JSON.stringify(prop)}`)
          }
        }),
        attribute.comments,
      )
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function clearJSXAttributeOtherJavaScriptSourceMaps(
  attribute: JSXAttributeOtherJavaScript,
): JSXAttributeOtherJavaScript {
  return {
    ...attribute,
    sourceMap: null,
  }
}

export function clearAttributeSourceMaps(attribute: JSXAttribute): JSXAttribute {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return attribute
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return clearJSXAttributeOtherJavaScriptSourceMaps(attribute)
    case 'ATTRIBUTE_NESTED_ARRAY':
      return jsxAttributeNestedArray(
        attribute.content.map((elem) => {
          switch (elem.type) {
            case 'ARRAY_SPREAD':
              return jsxArraySpread(clearAttributeSourceMaps(elem.value), emptyComments)
            case 'ARRAY_VALUE':
              return jsxArrayValue(clearAttributeSourceMaps(elem.value), emptyComments)
            default:
              const _exhaustiveCheck: never = elem
              throw new Error(`Unhandled array element type ${JSON.stringify(elem)}`)
          }
        }),
        emptyComments,
      )
    case 'ATTRIBUTE_FUNCTION_CALL':
      return jsxAttributeFunctionCall(
        attribute.functionName,
        attribute.parameters.map(clearAttributeSourceMaps),
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      return jsxAttributeNestedObject(
        attribute.content.map((prop) => {
          switch (prop.type) {
            case 'SPREAD_ASSIGNMENT':
              return jsxSpreadAssignment(clearAttributeSourceMaps(prop.value), emptyComments)
            case 'PROPERTY_ASSIGNMENT':
              return jsxPropertyAssignment(
                prop.key,
                clearAttributeSourceMaps(prop.value),
                emptyComments,
                emptyComments,
              )
            default:
              const _exhaustiveCheck: never = prop
              throw new Error(`Unhandled property type ${JSON.stringify(prop)}`)
          }
        }),
        emptyComments,
      )
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function isJSXAttributeValue(
  attribute: JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound,
): attribute is JSXAttributeValue<any> {
  return attribute != null && attribute.type === 'ATTRIBUTE_VALUE'
}

export function isPartOfJSXAttributeValue(
  attribute: JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound,
): attribute is PartOfJSXAttributeValue {
  return attribute != null && attribute.type === 'PART_OF_ATTRIBUTE_VALUE'
}

export function isJSXAttributeOtherJavaScript(
  attribute: ModifiableAttribute,
): attribute is JSXAttributeOtherJavaScript {
  return attribute != null && attribute.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
}

export function isJSXAttributeFunctionCall(
  attribute: JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound,
): attribute is JSXAttributeFunctionCall {
  return attribute != null && attribute.type === 'ATTRIBUTE_FUNCTION_CALL'
}

export function isJSXAttributeNestedArray(
  attribute: JSXAttribute | PartOfJSXAttributeValue,
): attribute is JSXAttributeNestedArray {
  return attribute != null && attribute.type === 'ATTRIBUTE_NESTED_ARRAY'
}

export function isJSXAttributeNestedObject(
  attribute: JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound,
): attribute is JSXAttributeNestedObject {
  return attribute != null && attribute.type === 'ATTRIBUTE_NESTED_OBJECT'
}

export function isJSXAttributeNotFound(attribute: unknown): attribute is JSXAttributeNotFound {
  return unknownObjectProperty(attribute, 'type') === 'ATTRIBUTE_NOT_FOUND'
}

export function isRegularJSXAttribute(
  attribute: JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound,
): attribute is JSXAttribute {
  return (
    attribute != null && !isPartOfJSXAttributeValue(attribute) && !isJSXAttributeNotFound(attribute)
  )
}

export interface JSXAttributesEntry extends WithComments {
  key: string
  value: JSXAttribute
}

export function jsxAttributesEntry(
  key: string,
  value: JSXAttribute,
  comments: ParsedComments,
): JSXAttributesEntry {
  return {
    key: key,
    value: value,
    comments: comments,
  }
}

export type JSXAttributes = Array<JSXAttributesEntry>

export function jsxAttributesFromMap(map: MapLike<JSXAttribute>): JSXAttributes {
  return Object.keys(map).map((objectKey) => {
    return jsxAttributesEntry(objectKey, map[objectKey], emptyComments)
  })
}

export function getJSXAttribute(attributes: JSXAttributes, key: string): JSXAttribute | null {
  const entry = attributes.find((attr) => attr.key === key)
  if (entry == null) {
    return null
  } else {
    return entry.value
  }
}

export function getJSXAttributeForced(attributes: JSXAttributes, key: string): JSXAttribute {
  return forceNotNull('Should not be null.', getJSXAttribute(attributes, key))
}

export function deleteJSXAttribute(attributes: JSXAttributes, key: string): JSXAttributes {
  return attributes.filter((a) => a.key !== key)
}

export function setJSXAttributesAttribute(
  attributes: JSXAttributes,
  key: string,
  value: JSXAttribute,
): JSXAttributes {
  let updatedExistingField: boolean = false
  let result: JSXAttributes = attributes.map((attr) => {
    if (attr.key === key) {
      updatedExistingField = true
      return jsxAttributesEntry(key, value, attr.comments)
    } else {
      return attr
    }
  })
  if (!updatedExistingField) {
    result.push(jsxAttributesEntry(key, value, emptyComments))
  }
  return result
}

export function getDefinedElsewhereFromAttribute(attribute: JSXAttribute): Array<string> {
  if (isJSXAttributeOtherJavaScript(attribute)) {
    return attribute.definedElsewhere
  } else if (isJSXAttributeNestedObject(attribute)) {
    return attribute.content.reduce<Array<string>>((working, elem) => {
      return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem.value))
    }, [])
  } else if (isJSXAttributeNestedArray(attribute)) {
    return attribute.content.reduce<Array<string>>((working, elem) => {
      return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem.value))
    }, [])
  } else {
    return []
  }
}

export function getDefinedElsewhereFromAttributes(attributes: JSXAttributes): Array<string> {
  return attributes.reduce<Array<string>>((working, entry) => {
    return addAllUniquely(working, getDefinedElsewhereFromAttribute(entry.value))
  }, [])
}

export function getDefinedElsewhereFromElement(element: JSXElement): Array<string> {
  const fromAttributes = getDefinedElsewhereFromAttributes(element.props)
  return element.children.reduce((working, child) => {
    if (isJSXArbitraryBlock(child)) {
      return addAllUniquely(working, child.definedElsewhere)
    } else if (isJSXElement(child)) {
      return addAllUniquely(working, getDefinedElsewhereFromElement(child))
    } else {
      return working
    }
  }, fromAttributes)
}

export function clearAttributesUniqueIDs(attributes: JSXAttributes): JSXAttributes {
  return attributes.map((attribute) => {
    return jsxAttributesEntry(
      attribute.key,
      clearAttributeUniqueIDs(attribute.value),
      attribute.comments,
    )
  })
}

export function clearAttributesSourceMaps(attributes: JSXAttributes): JSXAttributes {
  return attributes.map((attribute) => {
    return jsxAttributesEntry(
      attribute.key,
      clearAttributeSourceMaps(attribute.value),
      attribute.comments,
    )
  })
}

export interface JSXElementName {
  baseVariable: string
  propertyPath: PropertyPath
}

export function jsxElementName(
  baseVariable: string,
  propertyPathParts: Array<PropertyPathPart>,
): JSXElementName {
  return {
    baseVariable: baseVariable,
    propertyPath: PP.create(propertyPathParts),
  }
}

export function jsxElementNameEquals(first: JSXElementName, second: JSXElementName): boolean {
  return (
    first.baseVariable === second.baseVariable &&
    PP.pathsEqual(first.propertyPath, second.propertyPath)
  )
}

export function isIntrinsicElement(name: JSXElementName): boolean {
  // Elements with a lowercase first character are assumed to be intrinsic, since React treats them differently
  // https://reactjs.org/docs/jsx-in-depth.html#user-defined-components-must-be-capitalized
  return PP.depth(name.propertyPath) === 0 && firstLetterIsLowerCase(name.baseVariable)
}

export function isIntrinsicHTMLElementString(name: string): boolean {
  return intrinsicHTMLElementNamesAsStrings.includes(name)
}

export function isIntrinsicHTMLElement(name: JSXElementName): boolean {
  return isIntrinsicElement(name) && isIntrinsicHTMLElementString(name.baseVariable)
}

export function getJSXElementNameLastPart(name: JSXElementName): string {
  if (PP.depth(name.propertyPath) === 0) {
    return name.baseVariable
  } else {
    return PP.lastPart(name.propertyPath).toString()
  }
}

export function getJSXElementNameNoPathName(name: JSXElementName): string | null {
  if (PP.depth(name.propertyPath) === 0) {
    return name.baseVariable
  } else {
    return null
  }
}

export function getJSXElementNameAsString(name: JSXElementName): string {
  if (PP.depth(name.propertyPath) === 0) {
    return name.baseVariable
  } else {
    return `${name.baseVariable}.${PP.toString(name.propertyPath)}`
  }
}

export interface JSXElement {
  type: 'JSX_ELEMENT'
  name: JSXElementName
  props: JSXAttributes
  children: JSXElementChildren
}

export type ElementsWithin = { [uid: string]: JSXElement }

export interface JSXArbitraryBlock {
  type: 'JSX_ARBITRARY_BLOCK'
  originalJavascript: string
  javascript: string
  transpiledJavascript: string
  definedElsewhere: Array<string>
  sourceMap: RawSourceMap | null
  uniqueID: string
  elementsWithin: ElementsWithin
}

export function jsxArbitraryBlock(
  originalJavascript: string,
  javascript: string,
  transpiledJavascript: string,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
): JSXArbitraryBlock {
  return {
    type: 'JSX_ARBITRARY_BLOCK',
    originalJavascript: originalJavascript,
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uniqueID: UUID(),
    elementsWithin: elementsWithin,
  }
}

export interface JSXTextBlock {
  type: 'JSX_TEXT_BLOCK'
  text: string
  uniqueID: string
}

export function jsxTextBlock(text: string): JSXTextBlock {
  return {
    type: 'JSX_TEXT_BLOCK',
    text: text,
    uniqueID: UUID(),
  }
}

export interface JSXFragment {
  type: 'JSX_FRAGMENT'
  children: JSXElementChildren
  uniqueID: string
  longForm: boolean // When true, <React.Fragment> instead of <>.
}

export function jsxFragment(children: JSXElementChildren, longForm: boolean): JSXFragment {
  return {
    type: 'JSX_FRAGMENT',
    children: children,
    uniqueID: UUID(),
    longForm: longForm,
  }
}

export type JSXElementChild = JSXElement | JSXArbitraryBlock | JSXTextBlock | JSXFragment

export function isJSXElement(element: JSXElementChild): element is JSXElement {
  return element.type === 'JSX_ELEMENT'
}

export function isJSXArbitraryBlock(element: JSXElementChild): element is JSXArbitraryBlock {
  return element.type === 'JSX_ARBITRARY_BLOCK'
}

export function isJSXTextBlock(element: JSXElementChild): element is JSXTextBlock {
  return element.type === 'JSX_TEXT_BLOCK'
}

export function isJSXFragment(element: JSXElementChild): element is JSXFragment {
  return element.type === 'JSX_FRAGMENT'
}

export type JSXElementChildren = Array<JSXElementChild>

export function clearJSXElementUniqueIDs<T extends JSXElementChild>(element: T): T {
  if (isJSXElement(element)) {
    const updatedProps = clearAttributesUniqueIDs(element.props)
    const updatedChildren: JSXElementChildren = element.children.map(clearJSXElementUniqueIDs)
    return {
      ...element,
      props: updatedProps,
      children: updatedChildren,
    }
  } else if (isJSXArbitraryBlock(element)) {
    const updatedElementsWithin = objectMap(clearJSXElementUniqueIDs, element.elementsWithin)
    return {
      ...element,
      uniqueID: '',
      elementsWithin: updatedElementsWithin,
    }
  } else if (isJSXFragment(element)) {
    const updatedChildren: JSXElementChildren = element.children.map(clearJSXElementUniqueIDs)
    return {
      ...element,
      uniqueID: '',
      children: updatedChildren,
    }
  } else {
    return {
      ...element,
      uniqueID: '',
    }
  }
}

export function jsxElement(
  name: JSXElementName | string,
  props: JSXAttributes,
  children: JSXElementChildren,
): JSXElement {
  return {
    type: 'JSX_ELEMENT',
    name: typeof name === 'string' ? jsxElementName(name, []) : name,
    props: props,
    children: children,
  }
}

export function jsxTestElement(
  name: JSXElementName | string,
  props: JSXAttributes,
  children: Array<JSXElement>,
  uid: string = 'aaa',
): JSXElement {
  return jsxElement(
    name,
    setJSXAttributesAttribute(props, 'data-uid', jsxAttributeValue(uid, emptyComments)),
    children,
  )
}

export function utopiaJSXComponent(
  name: string,
  isFunction: boolean,
  declarationSyntax: FunctionDeclarationSyntax,
  blockOrExpression: BlockOrExpression,
  param: Param | null,
  propsUsed: Array<string>,
  rootElement: JSXElementChild,
  jsBlock: ArbitraryJSBlock | null,
  usedInReactDOMRender: boolean,
  returnStatementComments: ParsedComments,
): UtopiaJSXComponent {
  return {
    type: 'UTOPIA_JSX_COMPONENT',
    name: name,
    isFunction: isFunction,
    declarationSyntax: declarationSyntax,
    blockOrExpression: blockOrExpression,
    param: param,
    propsUsed: propsUsed,
    rootElement: rootElement,
    arbitraryJSBlock: jsBlock,
    usedInReactDOMRender: usedInReactDOMRender,
    returnStatementComments: returnStatementComments,
  }
}

export function arbitraryJSBlock(
  javascript: string,
  transpiledJavascript: string,
  definedWithin: Array<string>,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
): ArbitraryJSBlock {
  return {
    type: 'ARBITRARY_JS_BLOCK',
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedWithin: definedWithin,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uniqueID: UUID(),
  }
}

export function importStatement(
  rawCode: string,
  importStarAs: boolean,
  importWithName: boolean,
  imports: Array<string>,
  module: string,
): ImportStatement {
  return {
    type: 'IMPORT_STATEMENT',
    rawCode: rawCode,
    importStarAs: importStarAs,
    importWithName: importWithName,
    imports: imports,
    module: module,
  }
}

export function unparsedCode(rawCode: string): UnparsedCode {
  return {
    type: 'UNPARSED_CODE',
    rawCode: rawCode,
  }
}

interface RegularParam {
  type: 'REGULAR_PARAM'
  paramName: string
  defaultExpression: JSXAttributeOtherJavaScript | null
}

export function regularParam(
  paramName: string,
  defaultExpression: JSXAttributeOtherJavaScript | null,
): RegularParam {
  return {
    type: 'REGULAR_PARAM',
    paramName: paramName,
    defaultExpression: defaultExpression,
  }
}

export interface DestructuredParamPart {
  propertyName: string | undefined
  param: Param
  defaultExpression: JSXAttributeOtherJavaScript | null
}

export function destructuredParamPart(
  propertyName: string | undefined,
  param: Param,
  defaultExpression: JSXAttributeOtherJavaScript | null,
): DestructuredParamPart {
  return {
    propertyName: propertyName,
    param: param,
    defaultExpression: defaultExpression,
  }
}

interface DestructuredObject {
  type: 'DESTRUCTURED_OBJECT'
  parts: Array<DestructuredParamPart>
}

export function destructuredObject(parts: Array<DestructuredParamPart>): DestructuredObject {
  return {
    type: 'DESTRUCTURED_OBJECT',
    parts: parts,
  }
}

interface OmittedParam {
  type: 'OMITTED_PARAM'
}

export function omittedParam(): OmittedParam {
  return {
    type: 'OMITTED_PARAM',
  }
}

export type DestructuredArrayPart = Param | OmittedParam

export function isOmittedParam(param: DestructuredArrayPart): param is OmittedParam {
  return (param as any).type === 'OMITTED_PARAM'
}

interface DestructuredArray {
  type: 'DESTRUCTURED_ARRAY'
  parts: Array<DestructuredArrayPart>
}

export function destructuredArray(parts: Array<DestructuredArrayPart>): DestructuredArray {
  return {
    type: 'DESTRUCTURED_ARRAY',
    parts: parts,
  }
}

export type BoundParam = RegularParam | DestructuredObject | DestructuredArray

export function isRegularParam(param: BoundParam): param is RegularParam {
  return param.type === 'REGULAR_PARAM'
}

export function isDestructuredObject(param: BoundParam): param is DestructuredObject {
  return param.type === 'DESTRUCTURED_OBJECT'
}

export function isDestructuredArray(param: BoundParam): param is DestructuredArray {
  return param.type === 'DESTRUCTURED_ARRAY'
}

export type Param = {
  type: 'PARAM'
  dotDotDotToken: boolean
  boundParam: BoundParam
}

export function functionParam(dotDotDotToken: boolean, boundParam: BoundParam): Param {
  return {
    type: 'PARAM',
    dotDotDotToken: dotDotDotToken,
    boundParam: boundParam,
  }
}

export const defaultPropsParam: Param = functionParam(false, regularParam('props', null))

export function propNamesForParam(param: Param): Array<string> {
  const { boundParam } = param
  if (isDestructuredObject(boundParam)) {
    // This is the only case where we'll know the names of the props
    return mapDropNulls((part) => {
      const innerBoundParam = part.param.boundParam
      if (
        part.propertyName == null &&
        isRegularParam(innerBoundParam) &&
        !part.param.dotDotDotToken // Ignore rest params, since they don't tell us the names of the params
      ) {
        return innerBoundParam.paramName
      } else {
        return part.propertyName
      }
    }, boundParam.parts)
  } else {
    return []
  }
}

export type VarLetOrConst = 'var' | 'let' | 'const'
export type FunctionDeclarationSyntax = 'function' | VarLetOrConst
export type BlockOrExpression = 'block' | 'parenthesized-expression' | 'expression'

export interface UtopiaJSXComponent {
  type: 'UTOPIA_JSX_COMPONENT'
  name: string
  /**
   * isFunction is true if we are talking about a Function Component
   * isFunction false means that this is an exported Element with no props
   * (NOT a component, NOT a class component!)
   */
  isFunction: boolean
  declarationSyntax: FunctionDeclarationSyntax
  blockOrExpression: BlockOrExpression
  param: Param | null
  propsUsed: Array<string>
  rootElement: JSXElementChild
  arbitraryJSBlock: ArbitraryJSBlock | null
  usedInReactDOMRender: boolean
  returnStatementComments: ParsedComments
}

export interface ArbitraryJSBlock {
  type: 'ARBITRARY_JS_BLOCK'
  javascript: string
  transpiledJavascript: string
  definedWithin: Array<string>
  definedElsewhere: Array<string>
  sourceMap: RawSourceMap | null
  uniqueID: string
}

export interface ImportStatement {
  type: 'IMPORT_STATEMENT'
  rawCode: string
  importStarAs: boolean // Includes `import * as Name from`
  importWithName: boolean // Includes `import Name from`
  imports: Array<string> // All other imports inside braces i.e. `import { Name } from`
  module: string
}

export interface UnparsedCode {
  type: 'UNPARSED_CODE'
  rawCode: string
}

export type TopLevelElement = UtopiaJSXComponent | ArbitraryJSBlock | ImportStatement | UnparsedCode

export function clearArbitraryJSBlockUniqueIDs(block: ArbitraryJSBlock): ArbitraryJSBlock {
  return {
    ...block,
    uniqueID: '',
  }
}

export function clearDestructuredArrayPartUniqueIDs(
  arrayPart: DestructuredArrayPart,
): DestructuredArrayPart {
  switch (arrayPart.type) {
    case 'OMITTED_PARAM':
      return arrayPart
    case 'PARAM':
      return clearParamUniqueIDs(arrayPart)
    default:
      const _exhaustiveCheck: never = arrayPart
      throw new Error(`Unhandled array part ${JSON.stringify(arrayPart)}`)
  }
}

export function clearDestructuredParamPartUniqueIDs(
  paramPart: DestructuredParamPart,
): DestructuredParamPart {
  return destructuredParamPart(
    paramPart.propertyName,
    clearParamUniqueIDs(paramPart.param),
    paramPart.defaultExpression == null
      ? null
      : clearJSXAttributeOtherJavaScriptUniqueIDs(paramPart.defaultExpression),
  )
}

export function clearBoundParamUniqueIDs(boundParam: BoundParam): BoundParam {
  switch (boundParam.type) {
    case 'DESTRUCTURED_ARRAY':
      return destructuredArray(boundParam.parts.map(clearDestructuredArrayPartUniqueIDs))
    case 'DESTRUCTURED_OBJECT':
      return destructuredObject(boundParam.parts.map(clearDestructuredParamPartUniqueIDs))
    case 'REGULAR_PARAM':
      return regularParam(
        boundParam.paramName,
        boundParam.defaultExpression == null
          ? null
          : clearJSXAttributeOtherJavaScriptUniqueIDs(boundParam.defaultExpression),
      )
    default:
      const _exhaustiveCheck: never = boundParam
      throw new Error(`Unhandled element ${JSON.stringify(boundParam)}`)
  }
}

export function clearParamUniqueIDs(param: Param): Param {
  return functionParam(param.dotDotDotToken, clearBoundParamUniqueIDs(param.boundParam))
}

// FIXME: Should only really be in test code.
export function clearTopLevelElementUniqueIDs(element: UtopiaJSXComponent): UtopiaJSXComponent
export function clearTopLevelElementUniqueIDs(element: ArbitraryJSBlock): ArbitraryJSBlock
export function clearTopLevelElementUniqueIDs(element: TopLevelElement): TopLevelElement
export function clearTopLevelElementUniqueIDs(element: TopLevelElement): TopLevelElement {
  switch (element.type) {
    case 'UTOPIA_JSX_COMPONENT':
      let updatedComponent: UtopiaJSXComponent = {
        ...element,
        rootElement: clearJSXElementUniqueIDs(element.rootElement),
      }
      if (updatedComponent.arbitraryJSBlock != null) {
        updatedComponent.arbitraryJSBlock = clearArbitraryJSBlockUniqueIDs(
          updatedComponent.arbitraryJSBlock,
        )
      }
      if (updatedComponent.param != null) {
        updatedComponent.param = clearParamUniqueIDs(updatedComponent.param)
      }
      return updatedComponent
    case 'ARBITRARY_JS_BLOCK':
      return clearArbitraryJSBlockUniqueIDs(element)
    case 'IMPORT_STATEMENT':
    case 'UNPARSED_CODE':
      return element
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled element ${JSON.stringify(element)}`)
  }
}

export function isUtopiaJSXComponent(
  topLevelElement: TopLevelElement,
): topLevelElement is UtopiaJSXComponent {
  return topLevelElement.type === 'UTOPIA_JSX_COMPONENT'
}

export function isArbitraryJSBlock(
  topLevelElement: TopLevelElement,
): topLevelElement is ArbitraryJSBlock {
  return topLevelElement.type === 'ARBITRARY_JS_BLOCK'
}

export function isImportStatement(
  topLevelElement: TopLevelElement,
): topLevelElement is ImportStatement {
  return topLevelElement.type === 'IMPORT_STATEMENT'
}

export function isUnparsedCode(topLevelElement: TopLevelElement): topLevelElement is UnparsedCode {
  return topLevelElement.type === 'UNPARSED_CODE'
}

export type ComputedStyle = { [key: string]: string }
export type StyleAttributeMetadataEntry = { fromStyleSheet: boolean } // TODO rename me to StyleAttributeMetadata, the other one to StyleAttributeMetadataMap
export type StyleAttributeMetadata = { [key: string]: StyleAttributeMetadataEntry | undefined }

export interface JSXMetadata {
  components: Array<ComponentMetadata>
  elements: ElementInstanceMetadataMap
}

export function jsxMetadata(
  components: Array<ComponentMetadata>,
  elements: ElementInstanceMetadataMap,
): JSXMetadata {
  return {
    components: components,
    elements: elements,
  }
}

export const emptyJsxMetadata: JSXMetadata = {
  components: [],
  elements: {},
}

export type ElementInstanceMetadataMap = { [key: string]: ElementInstanceMetadata }

export interface ElementInstanceMetadata {
  templatePath: InstancePath
  element: Either<string, JSXElementChild>
  props: { [key: string]: any } // the final, resolved, static props value
  globalFrame: CanvasRectangle | null
  localFrame: LocalRectangle | null
  children: Array<InstancePath>
  componentInstance: boolean
  specialSizeMeasurements: SpecialSizeMeasurements
  computedStyle: ComputedStyle | null
  attributeMetadatada: StyleAttributeMetadata | null
}

export function elementInstanceMetadata(
  templatePath: InstancePath,
  element: Either<string, JSXElementChild>,
  props: { [key: string]: any },
  globalFrame: CanvasRectangle | null,
  localFrame: LocalRectangle | null,
  children: Array<InstancePath>,
  componentInstance: boolean,
  sizeMeasurements: SpecialSizeMeasurements,
  computedStyle: ComputedStyle | null,
  attributeMetadatada: StyleAttributeMetadata | null,
): ElementInstanceMetadata {
  return {
    templatePath: templatePath,
    element: element,
    props: props,
    globalFrame: globalFrame,
    localFrame: localFrame,
    children: children,
    componentInstance: componentInstance,
    specialSizeMeasurements: sizeMeasurements,
    computedStyle: computedStyle,
    attributeMetadatada: attributeMetadatada,
  }
}

export type DetectedLayoutSystem = 'flex' | 'grid' | 'flow' | 'none'
export type SettableLayoutSystem = 'flex' | 'flow' | 'grid' | LayoutSystem

export interface SpecialSizeMeasurements {
  offset: LocalPoint
  coordinateSystemBounds: CanvasRectangle | null
  immediateParentBounds: CanvasRectangle | null
  immediateParentProvidesLayout: boolean
  usesParentBounds: boolean
  parentLayoutSystem: DetectedLayoutSystem // TODO make a specific boolean prop that tells us the parent is flex or not
  layoutSystemForChildren: DetectedLayoutSystem
  providesBoundsForChildren: boolean
  display: string
  position: CSSPosition | null
  margin: Sides
  padding: Sides
  naturalWidth: number | null
  naturalHeight: number | null
  clientWidth: number
  clientHeight: number
  parentFlexDirection: string | null
  htmlElementName: string
}

export function specialSizeMeasurements(
  offset: LocalPoint,
  coordinateSystemBounds: CanvasRectangle | null,
  immediateParentBounds: CanvasRectangle | null,
  immediateParentProvidesLayout: boolean,
  usesParentBounds: boolean,
  parentLayoutSystem: DetectedLayoutSystem,
  layoutSystemForChildren: DetectedLayoutSystem,
  providesBoundsForChildren: boolean,
  display: string,
  position: CSSPosition | null,
  margin: Sides,
  padding: Sides,
  naturalWidth: number | null,
  naturalHeight: number | null,
  clientWidth: number,
  clientHeight: number,
  parentFlexDirection: string | null,
  htmlElementName: string,
): SpecialSizeMeasurements {
  return {
    offset,
    coordinateSystemBounds,
    immediateParentBounds,
    immediateParentProvidesLayout,
    usesParentBounds,
    parentLayoutSystem,
    layoutSystemForChildren,
    providesBoundsForChildren,
    display,
    position,
    margin,
    padding,
    naturalWidth,
    naturalHeight,
    clientWidth,
    clientHeight,
    parentFlexDirection,
    htmlElementName,
  }
}

export const emptySpecialSizeMeasurements = specialSizeMeasurements(
  {
    x: 0,
    y: 0,
  } as LocalPoint,
  zeroCanvasRect,
  zeroCanvasRect,
  true,
  false,
  'flow',
  'flow',
  false,
  'initial',
  'static',
  sides(undefined, undefined, undefined, undefined),
  sides(undefined, undefined, undefined, undefined),
  null,
  null,
  0,
  0,
  null,
  'div',
)

export const emptyComputedStyle: ComputedStyle = {}
export const emptyAttributeMetadatada: StyleAttributeMetadata = {}

export interface ComponentMetadata {
  scenePath: ScenePath
  templatePath: InstancePath
  rootElements: Array<InstancePath>
  component: string | null
  globalFrame: CanvasRectangle | null
  sceneResizesContent: boolean
  label?: string
  style: React.CSSProperties
}

export function isComponentMetadata(
  maybeComponentMetadata: ComponentMetadata | ElementInstanceMetadata,
): maybeComponentMetadata is ComponentMetadata {
  return (maybeComponentMetadata as ComponentMetadata).scenePath != null
}

type Omit<T, K> = Pick<T, Exclude<keyof T, K>> // TODO update typescript!!
export type MetadataWithoutChildren = Omit<ElementInstanceMetadata, 'children'> & {
  childrenTemplatePaths: Array<InstancePath>
}

export type ComponentMetadataWithoutRootElements = Omit<ComponentMetadata, 'rootElements'>

export type ElementsByUID = { [uid: string]: JSXElement }

export function walkElement(
  element: JSXElementChild,
  parentPath: StaticElementPath,
  depth: number,
  forEach: (e: JSXElementChild, path: StaticElementPath, depth: number) => void,
): void {
  switch (element.type) {
    case 'JSX_ELEMENT':
      const uidAttr = getJSXAttribute(element.props, 'data-uid')
      if (uidAttr != null && isJSXAttributeValue(uidAttr) && typeof uidAttr.value === 'string') {
        const path = TP.appendToElementPath(parentPath, uidAttr.value)
        forEach(element, path, depth)
        fastForEach(element.children, (child) => walkElement(child, path, depth + 1, forEach))
      }
      break
    case 'JSX_FRAGMENT':
      forEach(element, parentPath, depth)
      fastForEach(element.children, (child) => walkElement(child, parentPath, depth + 1, forEach))
      break
    case 'JSX_TEXT_BLOCK':
      forEach(element, parentPath, depth)
      break
    case 'JSX_ARBITRARY_BLOCK':
      forEach(element, parentPath, depth)
      fastForEach(Object.keys(element.elementsWithin), (childKey) =>
        walkElement(element.elementsWithin[childKey], parentPath, depth + 1, forEach),
      )
      break
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled element type ${JSON.stringify(element)}`)
  }
}

export function walkElements(
  topLevelElements: Array<TopLevelElement>,
  forEach: (element: JSXElementChild, path: StaticElementPath) => void,
): void {
  const emptyPath = ([] as any) as StaticElementPath // Oh my word
  fastForEach(topLevelElements, (rootComponent) => {
    if (isUtopiaJSXComponent(rootComponent)) {
      walkElement(rootComponent.rootElement, emptyPath, 0, forEach)
    }
  })
}

export function getElementsByUIDFromTopLevelElements(
  elements: Array<TopLevelElement>,
): ElementsByUID {
  let result: ElementsByUID = {}

  walkElements(elements, (element: JSXElementChild) => {
    if (isJSXElement(element)) {
      const possibleUIDAttribute = getJSXAttribute(element.props, 'data-uid')
      if (
        possibleUIDAttribute != null &&
        isJSXAttributeValue(possibleUIDAttribute) &&
        typeof possibleUIDAttribute.value === 'string'
      ) {
        result[possibleUIDAttribute.value] = element
      }
    }
  })
  return result
}
