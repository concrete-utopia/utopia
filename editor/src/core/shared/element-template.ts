import type {
  PropertyPath,
  PropertyPathPart,
  StaticElementPathPart,
  ElementPath,
} from './project-file-types'
import {
  CanvasRectangle,
  LocalRectangle,
  LocalPoint,
  zeroCanvasRect,
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
} from './math-utils'
import { Either, foldEither, isLeft, left, right } from './either'
import { v4 as UUID } from 'uuid'
import { RawSourceMap } from '../workers/ts/ts-typings/RawSourceMap'
import * as PP from './property-path'
import { Sides, sides, LayoutSystem } from 'utopia-api/core'
import { assertNever, fastForEach, unknownObjectProperty } from './utils'
import { addAllUniquely, mapDropNulls, reverse } from './array-utils'
import { objectMap } from './object-utils'
import { CSSPosition, FlexDirection } from '../../components/inspector/common/css-utils'
import { ModifiableAttribute } from './jsx-attributes'
import * as EP from './element-path'
import { firstLetterIsLowerCase } from './string-utils'
import { intrinsicHTMLElementNamesAsStrings } from './dom-utils'
import type { MapLike } from 'typescript'
import { forceNotNull } from './optional-utils'
import type { FlexAlignment, FlexJustifyContent } from '../../components/inspector/inspector-common'
import { allComments } from './comment-flags'

export interface ParsedComments {
  leadingComments: Array<Comment>
  trailingComments: Array<Comment>
  questionTokenComments?: ParsedComments
}

export const emptyComments: ParsedComments = {
  leadingComments: [],
  trailingComments: [],
}

export function isParsedCommentsEmpty(comments: ParsedComments): boolean {
  return comments.leadingComments.length === 0 && comments.trailingComments.length === 0
}

export function parsedComments(
  leadingComments: Array<Comment>,
  trailingComments: Array<Comment>,
): ParsedComments {
  return {
    leadingComments: leadingComments,
    trailingComments: trailingComments,
  }
}

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

export function isWithComments(e: unknown): e is WithComments {
  return (e as WithComments).comments != null
}

export interface JSExpressionValue<T> extends WithComments {
  type: 'ATTRIBUTE_VALUE'
  uid: string
  value: T
}

export function jsExpressionValue<T>(
  value: T,
  comments: ParsedComments,
  uid: string = UUID(),
): JSExpressionValue<T> {
  return {
    type: 'ATTRIBUTE_VALUE',
    value: value,
    comments: comments,
    uid: uid,
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

export interface JSExpressionOtherJavaScript {
  type: 'ATTRIBUTE_OTHER_JAVASCRIPT'
  originalJavascript: string
  javascript: string
  transpiledJavascript: string
  definedElsewhere: Array<string>
  sourceMap: RawSourceMap | null
  uid: string
  elementsWithin: ElementsWithin
}

export function jsExpressionOtherJavaScript(
  javascript: string,
  transpiledJavascript: string,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
  uid: string = UUID(),
): JSExpressionOtherJavaScript {
  return {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    originalJavascript: javascript,
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uid: uid,
    elementsWithin: elementsWithin,
  }
}

export interface JSXSpreadAssignment extends WithComments {
  type: 'SPREAD_ASSIGNMENT'
  value: JSExpression
}

export function jsxSpreadAssignment(
  value: JSExpression,
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
  value: JSExpression
  keyComments: ParsedComments
}

export function jsxPropertyAssignment(
  key: string,
  value: JSExpression,
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

export interface JSExpressionNestedObject extends WithComments {
  type: 'ATTRIBUTE_NESTED_OBJECT'
  content: Array<JSXProperty>
  uid: string
}

export function jsExpressionNestedObject(
  content: Array<JSXProperty>,
  comments: ParsedComments,
  uid: string = UUID(),
): JSExpressionNestedObject {
  return {
    type: 'ATTRIBUTE_NESTED_OBJECT',
    content: content,
    comments: comments,
    uid: uid,
  }
}

export function jsxAttributeNestedObjectSimple(
  content: Array<JSXAttributesEntry>,
  comments: ParsedComments,
  uid: string = UUID(),
): JSExpressionNestedObject {
  return {
    type: 'ATTRIBUTE_NESTED_OBJECT',
    content: content.map((elem) =>
      jsxPropertyAssignment(elem.key, elem.value, emptyComments, emptyComments),
    ),
    comments: comments,
    uid: uid,
  }
}

export interface JSXArrayValue extends WithComments {
  type: 'ARRAY_VALUE'
  value: JSExpression
}

export function jsxArrayValue(value: JSExpression, comments: ParsedComments): JSXArrayValue {
  return {
    type: 'ARRAY_VALUE',
    value: value,
    comments: comments,
  }
}

export interface JSXArraySpread extends WithComments {
  type: 'ARRAY_SPREAD'
  value: JSExpression
}

export function jsxArraySpread(value: JSExpression, comments: ParsedComments): JSXArraySpread {
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

export interface JSExpressionNestedArray extends WithComments {
  type: 'ATTRIBUTE_NESTED_ARRAY'
  content: Array<JSXArrayElement>
  uid: string
}

export function jsExpressionNestedArray(
  content: Array<JSXArrayElement>,
  comments: ParsedComments,
  uid: string = UUID(),
): JSExpressionNestedArray {
  return {
    type: 'ATTRIBUTE_NESTED_ARRAY',
    content: content,
    comments: comments,
    uid: uid,
  }
}

export function jsxAttributeNestedArraySimple(
  content: Array<JSExpression>,
): JSExpressionNestedArray {
  return jsExpressionNestedArray(
    content.map((value) => jsxArrayValue(value, emptyComments)),
    emptyComments,
  )
}

export interface JSExpressionFunctionCall {
  type: 'ATTRIBUTE_FUNCTION_CALL'
  functionName: string
  parameters: Array<JSExpression>
  uid: string
}

export function jsExpressionFunctionCall(
  functionName: string,
  parameters: Array<JSExpression>,
  uid: string = UUID(),
): JSExpressionFunctionCall {
  return {
    type: 'ATTRIBUTE_FUNCTION_CALL',
    functionName: functionName,
    parameters: parameters,
    uid: uid,
  }
}

export type JSExpression =
  | JSExpressionValue<any>
  | JSExpressionOtherJavaScript
  | JSExpressionNestedArray
  | JSExpressionNestedObject
  | JSExpressionFunctionCall

export function clearJSExpressionOtherJavaScriptUniqueIDs(
  attribute: JSExpressionOtherJavaScript,
): JSExpressionOtherJavaScript {
  const updatedElementsWithin = objectMap(clearJSXElementUniqueIDs, attribute.elementsWithin)
  return {
    ...attribute,
    uid: '',
    elementsWithin: updatedElementsWithin,
  }
}

export function simplifyAttributesIfPossible(attributes: JSXAttributes): JSXAttributes {
  return attributes.map((attribute) => {
    switch (attribute.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        return jsxAttributesEntry(
          attribute.key,
          simplifyAttributeIfPossible(attribute.value),
          attribute.comments,
        )
      case 'JSX_ATTRIBUTES_SPREAD':
        return jsxAttributesSpread(
          simplifyAttributeIfPossible(attribute.spreadValue),
          attribute.comments,
        )
      default:
        const _exhaustiveCheck: never = attribute
        throw new Error(`Unhandled attribute type ${JSON.stringify(attribute)}`)
    }
  })
}

export function simplifyAttributeIfPossible(attribute: JSExpression): JSExpression {
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
        return jsExpressionValue(simpleArray, attribute.comments)
      } else {
        return jsExpressionNestedArray(notSoSimpleArray, attribute.comments)
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
        return jsExpressionValue(simpleObject, attribute.comments)
      } else {
        return jsExpressionNestedObject(notSoSimpleObject, attribute.comments)
      }
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function clearExpressionUniqueIDs(attribute: JSExpression): JSExpression {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return jsExpressionValue(attribute.value, attribute.comments, '')
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return clearJSExpressionOtherJavaScriptUniqueIDs(attribute)
    case 'ATTRIBUTE_NESTED_ARRAY':
      return jsExpressionNestedArray(
        attribute.content.map((elem) => {
          switch (elem.type) {
            case 'ARRAY_SPREAD':
              return jsxArraySpread(clearExpressionUniqueIDs(elem.value), elem.comments)
            case 'ARRAY_VALUE':
              return jsxArrayValue(clearExpressionUniqueIDs(elem.value), elem.comments)
            default:
              const _exhaustiveCheck: never = elem
              throw new Error(`Unhandled array element type ${JSON.stringify(elem)}`)
          }
        }),
        attribute.comments,
        '',
      )
    case 'ATTRIBUTE_FUNCTION_CALL':
      return jsExpressionFunctionCall(
        attribute.functionName,
        attribute.parameters.map(clearExpressionUniqueIDs),
        '',
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      return jsExpressionNestedObject(
        attribute.content.map((prop) => {
          switch (prop.type) {
            case 'SPREAD_ASSIGNMENT':
              return jsxSpreadAssignment(clearExpressionUniqueIDs(prop.value), prop.comments)
            case 'PROPERTY_ASSIGNMENT':
              return jsxPropertyAssignment(
                prop.key,
                clearExpressionUniqueIDs(prop.value),
                prop.comments,
                prop.keyComments,
              )
            default:
              const _exhaustiveCheck: never = prop
              throw new Error(`Unhandled property type ${JSON.stringify(prop)}`)
          }
        }),
        attribute.comments,
        '',
      )
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function clearJSXAttributeOtherJavaScriptSourceMaps(
  attribute: JSExpressionOtherJavaScript,
): JSExpressionOtherJavaScript {
  return {
    ...attribute,
    sourceMap: null,
  }
}

export function clearAttributeSourceMaps(attribute: JSExpression): JSExpression {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return attribute
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return clearJSXAttributeOtherJavaScriptSourceMaps(attribute)
    case 'ATTRIBUTE_NESTED_ARRAY':
      return jsExpressionNestedArray(
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
      return jsExpressionFunctionCall(
        attribute.functionName,
        attribute.parameters.map(clearAttributeSourceMaps),
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      return jsExpressionNestedObject(
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

export function isJSXAttributeValue(element: JSXElementChild): element is JSExpressionValue<any> {
  return element.type === 'ATTRIBUTE_VALUE'
}

export function modifiableAttributeIsPartOfAttributeValue(
  attribute: ModifiableAttribute,
): attribute is PartOfJSXAttributeValue {
  return attribute != null && attribute.type === 'PART_OF_ATTRIBUTE_VALUE'
}

export function modifiableAttributeIsAttributeValue(
  attribute: ModifiableAttribute,
): attribute is JSExpressionValue<any> {
  return attribute != null && attribute.type === 'ATTRIBUTE_VALUE'
}

export function modifiableAttributeIsAttributeOtherJavaScript(
  attribute: ModifiableAttribute,
): attribute is JSExpressionOtherJavaScript {
  return attribute != null && attribute.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
}

export function modifiableAttributeIsAttributeFunctionCall(
  attribute: ModifiableAttribute,
): attribute is JSExpressionFunctionCall {
  return attribute != null && attribute.type === 'ATTRIBUTE_FUNCTION_CALL'
}

export function modifiableAttributeIsAttributeNestedArray(
  attribute: ModifiableAttribute,
): attribute is JSExpressionNestedArray {
  return attribute != null && attribute.type === 'ATTRIBUTE_NESTED_ARRAY'
}

export function modifiableAttributeIsAttributeNestedObject(
  attribute: ModifiableAttribute,
): attribute is JSExpressionNestedObject {
  return attribute != null && attribute.type === 'ATTRIBUTE_NESTED_OBJECT'
}

export function modifiableAttributeIsAttributeNotFound(
  attribute: unknown,
): attribute is JSXAttributeNotFound {
  return unknownObjectProperty(attribute, 'type') === 'ATTRIBUTE_NOT_FOUND'
}

export function isRegularJSXAttribute(
  attribute: JSExpression | PartOfJSXAttributeValue | JSXAttributeNotFound,
): attribute is JSExpression {
  return (
    attribute != null &&
    !modifiableAttributeIsPartOfAttributeValue(attribute) &&
    !modifiableAttributeIsAttributeNotFound(attribute)
  )
}

export interface JSXAttributesEntry extends WithComments {
  type: 'JSX_ATTRIBUTES_ENTRY'
  key: string
  value: JSExpression
}

export function jsxAttributesEntry(
  key: string,
  value: JSExpression,
  comments: ParsedComments,
): JSXAttributesEntry {
  return {
    type: 'JSX_ATTRIBUTES_ENTRY',
    key: key,
    value: value,
    comments: comments,
  }
}

export function simpleAttribute(key: string, value: unknown): JSXAttributesEntry {
  return jsxAttributesEntry(key, jsExpressionValue(value, emptyComments), emptyComments)
}

export interface JSXAttributesSpread extends WithComments {
  type: 'JSX_ATTRIBUTES_SPREAD'
  spreadValue: JSExpression
}

export function jsxAttributesSpread(
  spreadValue: JSExpression,
  comments: ParsedComments,
): JSXAttributesSpread {
  return {
    type: 'JSX_ATTRIBUTES_SPREAD',
    spreadValue: spreadValue,
    comments: comments,
  }
}

export type JSXAttributesPart = JSXAttributesEntry | JSXAttributesSpread

export function isJSXAttributesEntry(part: JSXAttributesPart): part is JSXAttributesEntry {
  return part.type === 'JSX_ATTRIBUTES_ENTRY'
}

export function isJSXAttributesSpread(part: JSXAttributesPart): part is JSXAttributesSpread {
  return part.type === 'JSX_ATTRIBUTES_SPREAD'
}

export type JSXAttributes = Array<JSXAttributesPart>

export function jsxAttributesFromMap(map: MapLike<JSExpression>): Array<JSXAttributesEntry> {
  return Object.keys(map).map((objectKey) => {
    return jsxAttributesEntry(objectKey, map[objectKey], emptyComments)
  })
}

export function getJSXAttribute(attributes: JSXAttributes, key: string): JSExpression | null {
  for (const attrPart of reverse(attributes)) {
    switch (attrPart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        if (attrPart.key === key) {
          return attrPart.value
        }
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        // Ignore these for now.
        break
      default:
        const _exhaustiveCheck: never = attrPart
        throw new Error(`Unhandled attribute type ${JSON.stringify(attrPart)}`)
    }
  }
  return null
}

export function getJSXAttributeForced(attributes: JSXAttributes, key: string): ModifiableAttribute {
  return forceNotNull('Should not be null.', getJSXAttribute(attributes, key))
}

export function deleteJSXAttribute(attributes: JSXAttributes, key: string): JSXAttributes {
  let newAttributes: JSXAttributes = []
  for (const attrPart of attributes) {
    switch (attrPart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        if (attrPart.key !== key) {
          newAttributes.push(attrPart)
        }
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        newAttributes.push(attrPart)
        break
      default:
        const _exhaustiveCheck: never = attrPart
        throw new Error(`Unhandled attribute type ${JSON.stringify(attrPart)}`)
    }
  }
  return newAttributes
}

export function setJSXAttributesAttribute(
  attributes: JSXAttributes,
  key: string,
  value: JSExpression,
): JSXAttributes {
  let updatedExistingField: boolean = false
  const simplifiedValue = simplifyAttributeIfPossible(value)
  let result: JSXAttributes = []

  for (const attrPart of attributes) {
    switch (attrPart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        if (attrPart.key === key) {
          result.push(jsxAttributesEntry(key, simplifiedValue, attrPart.comments))
          updatedExistingField = true
        } else {
          result.push(attrPart)
        }
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        result.push(attrPart)
        break
      default:
        const _exhaustiveCheck: never = attrPart
        throw new Error(`Unhandled attribute type ${JSON.stringify(attrPart)}`)
    }
  }

  if (!updatedExistingField) {
    result.push(jsxAttributesEntry(key, simplifiedValue, emptyComments))
  }
  return result
}

export function attributeReferencesElsewhere(attribute: JSExpression): boolean {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      return false
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return attribute.definedElsewhere.length > 0
    case 'ATTRIBUTE_NESTED_OBJECT':
      return attribute.content.some((subAttr) => {
        return attributeReferencesElsewhere(subAttr.value)
      })
    case 'ATTRIBUTE_NESTED_ARRAY':
      return attribute.content.some((subAttr) => {
        return attributeReferencesElsewhere(subAttr.value)
      })
    case 'ATTRIBUTE_FUNCTION_CALL':
      return attribute.parameters.some((parameter) => {
        return attributeReferencesElsewhere(parameter)
      })
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute type ${JSON.stringify(attribute)}`)
  }
}

export function jsxAttributesPartReferencesElsewhere(attrPart: JSXAttributesPart): boolean {
  switch (attrPart.type) {
    case 'JSX_ATTRIBUTES_ENTRY':
      return attributeReferencesElsewhere(attrPart.value)
    case 'JSX_ATTRIBUTES_SPREAD':
      return attributeReferencesElsewhere(attrPart.spreadValue)
    default:
      const _exhaustiveCheck: never = attrPart
      throw new Error(`Unhandled attribute type ${JSON.stringify(attrPart)}`)
  }
}

export function elementReferencesElsewhere(element: JSXElementChild): boolean {
  switch (element.type) {
    case 'JSX_ELEMENT':
      return element.props.some(jsxAttributesPartReferencesElsewhere)
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return element.definedElsewhere.length > 0
    case 'JSX_TEXT_BLOCK':
      return false
    case 'JSX_FRAGMENT':
      return element.children.some(elementReferencesElsewhere)
    case 'JSX_CONDITIONAL_EXPRESSION':
      return (
        elementReferencesElsewhere(element.whenTrue) ||
        elementReferencesElsewhere(element.whenFalse)
      )
    case 'ATTRIBUTE_VALUE':
      return false
    case 'ATTRIBUTE_NESTED_ARRAY':
      return element.content.some((contentPart) => {
        return elementReferencesElsewhere(contentPart.value)
      })
    case 'ATTRIBUTE_NESTED_OBJECT':
      return element.content.some((contentPart) => {
        return elementReferencesElsewhere(contentPart.value)
      })
    case 'ATTRIBUTE_FUNCTION_CALL':
      return element.parameters.some(elementReferencesElsewhere)
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled element type ${JSON.stringify(element)}`)
  }
}

export function getDefinedElsewhereFromAttribute(attribute: JSExpression): Array<string> {
  if (modifiableAttributeIsAttributeOtherJavaScript(attribute)) {
    return attribute.definedElsewhere
  } else if (modifiableAttributeIsAttributeNestedObject(attribute)) {
    return attribute.content.reduce<Array<string>>((working, elem) => {
      return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem.value))
    }, [])
  } else if (modifiableAttributeIsAttributeNestedArray(attribute)) {
    return attribute.content.reduce<Array<string>>((working, elem) => {
      return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem.value))
    }, [])
  } else {
    return []
  }
}

export function getDefinedElsewhereFromAttributes(attributes: JSXAttributes): Array<string> {
  return attributes.reduce<Array<string>>((working, entry) => {
    switch (entry.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        return addAllUniquely(working, getDefinedElsewhereFromAttribute(entry.value))
      case 'JSX_ATTRIBUTES_SPREAD':
        return addAllUniquely(working, getDefinedElsewhereFromAttribute(entry.spreadValue))
      default:
        const _exhaustiveCheck: never = entry
        throw new Error(`Unhandled attribute type ${JSON.stringify(entry)}`)
    }
  }, [])
}

export function getDefinedElsewhereFromElement(element: JSXElement): Array<string> {
  const fromAttributes = getDefinedElsewhereFromAttributes(element.props)
  return element.children.reduce((working, child) => {
    if (isJSExpressionOtherJavaScript(child)) {
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
    switch (attribute.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        return jsxAttributesEntry(
          attribute.key,
          clearExpressionUniqueIDs(attribute.value),
          attribute.comments,
        )
      case 'JSX_ATTRIBUTES_SPREAD':
        return jsxAttributesSpread(
          clearExpressionUniqueIDs(attribute.spreadValue),
          attribute.comments,
        )
      default:
        const _exhaustiveCheck: never = attribute
        throw new Error(`Unhandled attribute type ${JSON.stringify(attribute)}`)
    }
  })
}

export function clearAttributesSourceMaps(attributes: JSXAttributes): JSXAttributes {
  return attributes.map((attribute) => {
    switch (attribute.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        return jsxAttributesEntry(
          attribute.key,
          clearAttributeSourceMaps(attribute.value),
          attribute.comments,
        )
      case 'JSX_ATTRIBUTES_SPREAD':
        return jsxAttributesSpread(
          clearAttributeSourceMaps(attribute.spreadValue),
          attribute.comments,
        )
      default:
        const _exhaustiveCheck: never = attribute
        throw new Error(`Unhandled attribute type ${JSON.stringify(attribute)}`)
    }
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
    propertyPath: PP.createFromArray(propertyPathParts),
  }
}

export function jsxElementNameEquals(first: JSXElementName, second: JSXElementName): boolean {
  return (
    first.baseVariable === second.baseVariable &&
    PP.pathsEqual(first.propertyPath, second.propertyPath)
  )
}

export function isIntrinsicElementFromString(name: string): boolean {
  // Elements with a lowercase first character are assumed to be intrinsic, since React treats them differently
  // https://reactjs.org/docs/jsx-in-depth.html#user-defined-components-must-be-capitalized
  return firstLetterIsLowerCase(name) && !name.includes('.')
}

export function isIntrinsicElement(name: JSXElementName): boolean {
  return PP.depth(name.propertyPath) === 0 && isIntrinsicElementFromString(name.baseVariable)
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
  uid: string
}

export type JSXElementWithoutUID = Omit<JSXElement, 'uid'>

export type JSXConditionalExpressionWithoutUID = Omit<JSXConditionalExpression, 'uid'>

export type JSXFragmentWithoutUID = Omit<JSXFragment, 'uid'>

export function clearJSXElementWithoutUIDUniqueIDs(
  element: JSXElementWithoutUID,
): JSXElementWithoutUID {
  return {
    ...element,
    props: clearAttributesUniqueIDs(element.props),
    children: element.children.map(clearJSXElementChildUniqueIDs),
  }
}

export function clearJSXConditionalExpressionWithoutUIDUniqueIDs(
  conditional: JSXConditionalExpressionWithoutUID,
): JSXConditionalExpressionWithoutUID {
  return {
    ...conditional,
    condition: clearExpressionUniqueIDs(conditional.condition),
    whenTrue: clearJSXElementChildUniqueIDs(conditional.whenTrue),
    whenFalse: clearJSXElementChildUniqueIDs(conditional.whenFalse),
  }
}

export function clearJSXFragmentWithoutUIDUniqueIDs(
  fragment: JSXFragmentWithoutUID,
): JSXFragmentWithoutUID {
  return {
    ...fragment,
    children: fragment.children.map(clearJSXElementChildUniqueIDs),
  }
}

export type ElementsWithin = { [uid: string]: JSXElement }

export type JSXArbitraryBlock = JSExpression

export function jsxArbitraryBlock(
  originalJavascript: string,
  javascript: string,
  transpiledJavascript: string,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
): JSXArbitraryBlock {
  return {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    originalJavascript: originalJavascript,
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uid: UUID(),
    elementsWithin: elementsWithin,
  }
}

export interface JSXTextBlock {
  type: 'JSX_TEXT_BLOCK'
  text: string
  uid: string
}

export function jsxTextBlock(text: string): JSXTextBlock {
  return {
    type: 'JSX_TEXT_BLOCK',
    text: text,
    uid: UUID(),
  }
}

export interface JSXFragment {
  type: 'JSX_FRAGMENT'
  uid: string
  children: JSXElementChildren
  longForm: boolean
}

export function jsxFragment(
  uid: string,
  children: JSXElementChildren,
  longForm: boolean,
): JSXFragment {
  return {
    type: 'JSX_FRAGMENT',
    uid: uid,
    children: children,
    longForm: longForm,
  }
}

export type JSXElementLike = JSXElement | JSXFragment

export interface JSXConditionalExpression extends WithComments {
  type: 'JSX_CONDITIONAL_EXPRESSION'
  uid: string
  condition: JSExpression
  originalConditionString: string
  whenTrue: JSXElementChild
  whenFalse: JSXElementChild
}

function fixBaseComments(comments: ParsedComments, parentComments: Comment[]): ParsedComments {
  function commentIsNotIncluded(comment: Comment) {
    return !parentComments.some(
      (other) =>
        other.comment === comment.comment &&
        other.rawText === comment.rawText &&
        other.pos === comment.pos,
    )
  }
  return {
    leadingComments: comments.leadingComments.filter(commentIsNotIncluded),
    trailingComments: comments.trailingComments.filter(commentIsNotIncluded),
  }
}

function fixJSXConditionalExpressionCondition(
  condition: JSExpression,
  comments: ParsedComments,
): JSExpression {
  if (!isWithComments(condition)) {
    return condition
  }

  const flatComments = allComments(comments)

  return {
    ...condition,
    comments: {
      ...fixBaseComments(condition.comments, flatComments),
      questionTokenComments:
        condition.comments.questionTokenComments != null
          ? fixBaseComments(condition.comments.questionTokenComments, flatComments)
          : undefined,
    },
  }
}

export function jsxConditionalExpression(
  uid: string,
  condition: JSExpression,
  originalConditionString: string,
  whenTrue: JSXElementChild,
  whenFalse: JSXElementChild,
  comments: ParsedComments,
): JSXConditionalExpression {
  return {
    type: 'JSX_CONDITIONAL_EXPRESSION',
    uid: uid,
    originalConditionString: originalConditionString,
    condition: fixJSXConditionalExpressionCondition(condition, comments), // remove any duplicate comments shared with the element
    whenTrue: whenTrue,
    whenFalse: whenFalse,
    comments: comments,
  }
}

export type JSXElementChild =
  | JSXElement
  | JSXArbitraryBlock
  | JSXTextBlock
  | JSXFragment
  | JSXConditionalExpression

export function isJSXElement(element: JSXElementChild): element is JSXElement {
  return element.type === 'JSX_ELEMENT'
}

export function isJSExpressionValue(element: JSXElementChild): element is JSExpressionValue<any> {
  return element.type === 'ATTRIBUTE_VALUE'
}

export function isJSExpressionNestedArray(
  element: JSXElementChild,
): element is JSExpressionNestedArray {
  return element.type === 'ATTRIBUTE_NESTED_ARRAY'
}

export function isJSExpressionNestedObject(
  element: JSXElementChild,
): element is JSExpressionNestedObject {
  return element.type === 'ATTRIBUTE_NESTED_OBJECT'
}

export function isJSExpressionFunctionCall(
  element: JSXElementChild,
): element is JSExpressionFunctionCall {
  return element.type === 'ATTRIBUTE_FUNCTION_CALL'
}

export function isJSExpressionOtherJavaScript(
  element: JSXElementChild,
): element is JSExpressionOtherJavaScript {
  return element.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
}

export function isJSXArbitraryBlock(element: JSXElementChild): element is JSXArbitraryBlock {
  switch (element.type) {
    case 'JSX_ELEMENT':
    case 'JSX_TEXT_BLOCK':
    case 'JSX_FRAGMENT':
    case 'JSX_CONDITIONAL_EXPRESSION':
      return false
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return true
    default:
      assertNever(element)
  }
}

export function isJSXTextBlock(element: JSXElementChild): element is JSXTextBlock {
  return element.type === 'JSX_TEXT_BLOCK'
}

export function isJSXFragment(element: JSXElementChild): element is JSXFragment {
  return element.type === 'JSX_FRAGMENT'
}

export function isJSXConditionalExpression(
  element: JSXElementChild,
): element is JSXConditionalExpression {
  return element.type === 'JSX_CONDITIONAL_EXPRESSION'
}

export function isJSXElementLike(element: JSXElementChild): element is JSXElementLike {
  return isJSXElement(element) || isJSXFragment(element)
}

type UtopiaElement = JSXElement | JSXFragment | JSXConditionalExpression

// A utopia element can be either a HTML DOM element or a React-only exotic element (ie the Fragment) or a Utopia-only element, aka Elefant (ie the Conditional Expression)
export function isUtopiaElement(element: JSXElementChild): element is UtopiaElement {
  return isJSXElementLike(element) || isJSXConditionalExpression(element)
}

interface ElementWithUid {
  uid: string
}

export function isElementWithUid(element: unknown): element is ElementWithUid {
  return (element as ElementWithUid).uid != null
}

export type JSXElementChildren = Array<JSXElementChild>

export function clearJSXElementUniqueIDs(element: JSXElement): JSXElement {
  const updatedProps = clearAttributesUniqueIDs(element.props)
  const updatedChildren: JSXElementChildren = element.children.map(clearJSXElementChildUniqueIDs)
  return {
    ...element,
    props: updatedProps,
    children: updatedChildren,
    uid: '',
  }
}

export function clearJSXElementChildUniqueIDs(element: JSXElementChild): JSXElementChild {
  switch (element.type) {
    case 'JSX_ELEMENT': {
      return clearJSXElementUniqueIDs(element)
    }
    case 'JSX_FRAGMENT': {
      const updatedChildren: JSXElementChildren = element.children.map(
        clearJSXElementChildUniqueIDs,
      )
      return {
        ...element,
        children: updatedChildren,
        uid: '',
      }
    }
    case 'JSX_CONDITIONAL_EXPRESSION': {
      const updatedCondition = clearExpressionUniqueIDs(element.condition)
      const updatedWhenTrue = clearJSXElementChildUniqueIDs(element.whenTrue)
      const updatedWhenFalse = clearJSXElementChildUniqueIDs(element.whenFalse)
      return {
        ...element,
        condition: updatedCondition,
        whenTrue: updatedWhenTrue,
        whenFalse: updatedWhenFalse,
        uid: '',
      }
    }
    case 'JSX_TEXT_BLOCK': {
      return {
        ...element,
        uid: '',
      }
    }
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return clearExpressionUniqueIDs(element)
    default:
      assertNever(element)
  }
}

export function jsxElement(
  name: JSXElementName | string,
  uid: string,
  props: JSXAttributes,
  children: JSXElementChildren,
): JSXElement {
  return {
    type: 'JSX_ELEMENT',
    name: typeof name === 'string' ? jsxElementName(name, []) : name,
    uid: uid,
    props: props,
    children: children,
  }
}

export function jsxTestElement(
  name: JSXElementName | string,
  props: JSXAttributes,
  children: JSXElementChildren,
  uid: string = 'aaa',
): JSXElement {
  return jsxElement(
    name,
    uid,
    setJSXAttributesAttribute(props, 'data-uid', jsExpressionValue(uid, emptyComments)),
    children,
  )
}

export function jsxElementWithoutUID(
  name: JSXElementName | string,
  props: JSXAttributes,
  children: JSXElementChildren,
): JSXElementWithoutUID {
  return {
    type: 'JSX_ELEMENT',
    name: typeof name === 'string' ? jsxElementName(name, []) : name,
    props: props,
    children: children,
  }
}

export function jsxConditionalExpressionWithoutUID(
  condition: JSExpression,
  originalConditionString: string,
  whenTrue: JSXElementChild,
  whenFalse: JSXElementChild,
  comments: ParsedComments,
): JSXConditionalExpressionWithoutUID {
  return {
    type: 'JSX_CONDITIONAL_EXPRESSION',
    condition: condition,
    originalConditionString: originalConditionString,
    whenTrue: whenTrue,
    whenFalse: whenFalse,
    comments: comments,
  }
}

export function jsxFragmentWithoutUID(
  children: JSXElementChildren,
  longForm: boolean,
): JSXFragmentWithoutUID {
  return {
    type: 'JSX_FRAGMENT',
    children: children,
    longForm: longForm,
  }
}

export function utopiaJSXComponent(
  name: string | null,
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
  elementsWithin: ElementsWithin,
): ArbitraryJSBlock {
  return {
    type: 'ARBITRARY_JS_BLOCK',
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedWithin: definedWithin,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uid: UUID(),
    elementsWithin: elementsWithin,
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

export interface RegularParam {
  type: 'REGULAR_PARAM'
  paramName: string
  defaultExpression: JSExpressionOtherJavaScript | null
}

export function regularParam(
  paramName: string,
  defaultExpression: JSExpressionOtherJavaScript | null,
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
  defaultExpression: JSExpressionOtherJavaScript | null
}

export function destructuredParamPart(
  propertyName: string | undefined,
  param: Param,
  defaultExpression: JSExpressionOtherJavaScript | null,
): DestructuredParamPart {
  return {
    propertyName: propertyName,
    param: param,
    defaultExpression: defaultExpression,
  }
}

export interface DestructuredObject {
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

export interface DestructuredArray {
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

export function isParam(maybeParam: unknown): maybeParam is Param {
  return (
    typeof maybeParam === 'object' &&
    maybeParam != null &&
    'type' in maybeParam &&
    (maybeParam as any)['type'] === 'PARAM'
  )
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
  name: string | null
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
  uid: string
  elementsWithin: ElementsWithin
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
    uid: '',
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
      : clearJSExpressionOtherJavaScriptUniqueIDs(paramPart.defaultExpression),
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
          : clearJSExpressionOtherJavaScriptUniqueIDs(boundParam.defaultExpression),
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
        rootElement: clearJSXElementChildUniqueIDs(element.rootElement),
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
  return topLevelElement?.type === 'UTOPIA_JSX_COMPONENT'
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

export type ElementInstanceMetadataMap = { [key: string]: Readonly<ElementInstanceMetadata> }
export const emptyJsxMetadata: ElementInstanceMetadataMap = {}

export interface SameFileOrigin {
  type: 'SAME_FILE_ORIGIN'
  filePath: string
  variableName: string
}

export function isSameFileOrigin(importInfo: ImportInfo): importInfo is SameFileOrigin {
  return importInfo.type === 'SAME_FILE_ORIGIN'
}

export function sameFileOrigin(filePath: string, variableName: string): SameFileOrigin {
  return {
    type: 'SAME_FILE_ORIGIN',
    filePath: filePath,
    variableName: variableName,
  }
}

export interface ImportedOrigin {
  type: 'IMPORTED_ORIGIN'
  filePath: string
  variableName: string
  exportedName: string | null
}

export function isImportedOrigin(importInfo: ImportInfo): importInfo is ImportedOrigin {
  return importInfo.type === 'IMPORTED_ORIGIN'
}

export function importedOrigin(
  filePath: string,
  variableName: string,
  exportedName: string | null,
): ImportedOrigin {
  return {
    type: 'IMPORTED_ORIGIN',
    filePath: filePath,
    variableName: variableName,
    exportedName: exportedName,
  }
}

export type ImportInfo = SameFileOrigin | ImportedOrigin

export function createImportedFrom(
  variableName: string,
  originalName: string | null,
  path: string,
): ImportInfo {
  return importedOrigin(path, variableName, originalName)
}

export function createNotImported(path: string, variableName: string): ImportInfo {
  return sameFileOrigin(path, variableName)
}

export type ConditionValue = boolean | 'not-a-conditional'

export interface ElementInstanceMetadata {
  elementPath: ElementPath
  element: Either<string, JSXElementChild>
  globalFrame: MaybeInfinityCanvasRectangle | null
  localFrame: MaybeInfinityLocalRectangle | null
  componentInstance: boolean
  isEmotionOrStyledComponent: boolean
  specialSizeMeasurements: SpecialSizeMeasurements
  computedStyle: ComputedStyle | null
  attributeMetadatada: StyleAttributeMetadata | null
  label: string | null
  importInfo: ImportInfo | null
  conditionValue: ConditionValue
}

export function elementInstanceMetadata(
  elementPath: ElementPath,
  element: Either<string, JSXElementChild>,
  globalFrame: MaybeInfinityCanvasRectangle | null,
  localFrame: MaybeInfinityLocalRectangle | null,
  componentInstance: boolean,
  isEmotionOrStyledComponent: boolean,
  sizeMeasurements: SpecialSizeMeasurements,
  computedStyle: ComputedStyle | null,
  attributeMetadatada: StyleAttributeMetadata | null,
  label: string | null,
  importInfo: ImportInfo | null,
  conditionValue: ConditionValue,
): ElementInstanceMetadata {
  return {
    elementPath: elementPath,
    element: element,
    globalFrame: globalFrame,
    localFrame: localFrame,
    componentInstance: componentInstance,
    isEmotionOrStyledComponent: isEmotionOrStyledComponent,
    specialSizeMeasurements: sizeMeasurements,
    computedStyle: computedStyle,
    attributeMetadatada: attributeMetadatada,
    label: label,
    importInfo: importInfo,
    conditionValue: conditionValue,
  }
}

export type DetectedLayoutSystem = 'flex' | 'grid' | 'flow' | 'none'
export type SettableLayoutSystem = 'flex' | 'flow' | 'grid' | LayoutSystem
export type TextDirection = 'ltr' | 'rtl'

export interface SpecialSizeMeasurements {
  offset: LocalPoint
  coordinateSystemBounds: CanvasRectangle | null
  immediateParentBounds: CanvasRectangle | null
  immediateParentProvidesLayout: boolean
  closestOffsetParentPath: ElementPath
  usesParentBounds: boolean
  parentLayoutSystem: DetectedLayoutSystem // TODO make a specific boolean prop that tells us the parent is flex or not
  layoutSystemForChildren: DetectedLayoutSystem
  providesBoundsForAbsoluteChildren: boolean
  display: string
  position: CSSPosition | null
  margin: Sides
  padding: Sides
  naturalWidth: number | null
  naturalHeight: number | null
  clientWidth: number
  clientHeight: number
  parentFlexDirection: FlexDirection | null
  parentJustifyContent: FlexJustifyContent | null
  parentFlexGap: number
  parentPadding: Sides
  parentHugsOnMainAxis: boolean
  gap: number | null
  flexDirection: FlexDirection | null
  justifyContent: FlexJustifyContent | null
  alignItems: FlexAlignment | null
  htmlElementName: string
  renderedChildrenCount: number
  globalContentBoxForChildren: MaybeInfinityCanvasRectangle | null
  float: string
  hasPositionOffset: boolean
  parentTextDirection: TextDirection | null
  hasTransform: boolean
  borderRadius: Sides | null
  fontSize: string | null
  fontWeight: string | null
  fontStyle: string | null
  textDecorationLine: string | null
}

export function specialSizeMeasurements(
  offset: LocalPoint,
  coordinateSystemBounds: CanvasRectangle | null,
  immediateParentBounds: CanvasRectangle | null,
  immediateParentProvidesLayout: boolean,
  closestOffsetParentPath: ElementPath,
  usesParentBounds: boolean,
  parentLayoutSystem: DetectedLayoutSystem,
  layoutSystemForChildren: DetectedLayoutSystem,
  providesBoundsForAbsoluteChildren: boolean,
  display: string,
  position: CSSPosition | null,
  margin: Sides,
  padding: Sides,
  naturalWidth: number | null,
  naturalHeight: number | null,
  clientWidth: number,
  clientHeight: number,
  parentFlexDirection: FlexDirection | null,
  parentJustifyContent: FlexJustifyContent | null,
  parentFlexGap: number,
  parentPadding: Sides,
  parentHugsOnMainAxis: boolean,
  gap: number | null,
  flexDirection: FlexDirection | null,
  justifyContent: FlexJustifyContent | null,
  alignItems: FlexAlignment | null,
  htmlElementName: string,
  renderedChildrenCount: number,
  globalContentBoxForChildren: MaybeInfinityCanvasRectangle | null,
  float: string,
  hasPositionOffset: boolean,
  parentTextDirection: TextDirection | null,
  hasTransform: boolean,
  borderRadius: Sides | null,
  fontSize: string | null,
  fontWeight: string | null,
  fontStyle: string | null,
  textDecorationLine: string | null,
): SpecialSizeMeasurements {
  return {
    offset,
    coordinateSystemBounds,
    immediateParentBounds,
    immediateParentProvidesLayout,
    closestOffsetParentPath,
    usesParentBounds,
    parentLayoutSystem,
    layoutSystemForChildren,
    providesBoundsForAbsoluteChildren,
    display,
    position,
    margin,
    padding,
    naturalWidth,
    naturalHeight,
    clientWidth,
    clientHeight,
    parentFlexDirection,
    parentJustifyContent,
    parentFlexGap,
    parentPadding,
    parentHugsOnMainAxis,
    gap: gap,
    flexDirection,
    justifyContent,
    alignItems,
    htmlElementName,
    renderedChildrenCount,
    globalContentBoxForChildren: globalContentBoxForChildren,
    float,
    hasPositionOffset,
    parentTextDirection,
    hasTransform,
    borderRadius,
    fontSize,
    fontWeight,
    fontStyle,
    textDecorationLine,
  }
}

export const emptySpecialSizeMeasurements = specialSizeMeasurements(
  {
    x: 0,
    y: 0,
  } as LocalPoint,
  null,
  zeroCanvasRect,
  true,
  EP.emptyElementPath,
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
  null,
  0,
  sides(undefined, undefined, undefined, undefined),
  false,
  null,
  null,
  null,
  null,
  'div',
  0,
  null,
  'none',
  false,
  'ltr',
  false,
  null,
  null,
  null,
  null,
  null,
)

export const emptyComputedStyle: ComputedStyle = {}
export const emptyAttributeMetadatada: StyleAttributeMetadata = {}

export type ElementsByUID = { [uid: string]: JSXElement }

export function walkElement(
  element: JSXElementChild,
  parentPath: StaticElementPathPart,
  depth: number,
  forEach: (e: JSXElementChild, path: StaticElementPathPart, depth: number) => void,
): void {
  switch (element.type) {
    case 'JSX_ELEMENT':
      const uidAttr = getJSXAttribute(element.props, 'data-uid')
      if (uidAttr != null && isJSXAttributeValue(uidAttr) && typeof uidAttr.value === 'string') {
        const path = EP.appendToElementPath(parentPath, uidAttr.value)
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
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      forEach(element, parentPath, depth)
      fastForEach(Object.keys(element.elementsWithin), (childKey) =>
        walkElement(element.elementsWithin[childKey], parentPath, depth + 1, forEach),
      )
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      forEach(element, parentPath, depth)
      walkElement(element.whenTrue, parentPath, depth + 1, forEach)
      walkElement(element.whenFalse, parentPath, depth + 1, forEach)
      break
    case 'ATTRIBUTE_VALUE':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      fastForEach(element.content, (elem) => {
        forEach(elem.value, parentPath, depth + 1)
      })
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      fastForEach(element.content, (elem) => {
        forEach(elem.value, parentPath, depth + 1)
      })
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      fastForEach(element.parameters, (elem) => {
        forEach(elem, parentPath, depth + 1)
      })
      break
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled element type ${JSON.stringify(element)}`)
  }
}

export function walkElements(
  topLevelElements: Array<TopLevelElement>,
  forEach: (element: JSXElementChild, path: StaticElementPathPart) => void,
): void {
  const emptyPath = EP.emptyStaticElementPathPart()
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
