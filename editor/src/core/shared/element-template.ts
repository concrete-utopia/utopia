import type {
  PropertyPath,
  PropertyPathPart,
  StaticElementPathPart,
  ElementPath,
} from './project-file-types'
import type {
  CanvasRectangle,
  LocalPoint,
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
} from './math-utils'
import { zeroCanvasRect } from './math-utils'
import type { Either } from './either'
import { flatMapEither, isRight, left, mapEither, right } from './either'
import { v4 as UUID } from 'uuid'
import type { RawSourceMap } from '../workers/ts/ts-typings/RawSourceMap'
import * as PP from './property-path'
import type { Sides, LayoutSystem } from 'utopia-api/core'
import { sides } from 'utopia-api/core'
import { assertNever, fastForEach, unknownObjectProperty } from './utils'
import { addAllUniquely, mapDropNulls } from './array-utils'
import { objectMap } from './object-utils'
import type {
  CSSKeyword,
  CSSPosition,
  FlexDirection,
  GridAutoFlow,
  GridDimension,
} from '../../components/inspector/common/css-utils'
import type { ModifiableAttribute } from './jsx-attributes'
import * as EP from './element-path'
import { firstLetterIsLowerCase } from './string-utils'
import { intrinsicHTMLElementNamesAsStrings } from './dom-utils'
import type { MapLike } from 'typescript'
import { forceNotNull } from './optional-utils'
import type { FlexAlignment, FlexJustifyContent } from '../../components/inspector/inspector-common'
import { allComments } from './utopia-flags'
import type { Optic } from './optics/optics'
import { fromField } from './optics/optic-creators'
import { jsxSimpleAttributeToValue } from './jsx-attribute-utils'
import type {
  ParsedComments,
  MultiLineComment,
  SingleLineComment,
  WithComments,
  JSExpressionValue,
  PartOfJSXAttributeValue,
  JSXAttributeNotFound,
  JSOpaqueArbitraryStatement,
  JSAssignment,
  JSAssignmentStatement,
  JSExpressionOtherJavaScript,
  JSXMapExpression,
  JSXSpreadAssignment,
  JSXPropertyAssignment,
  JSExpressionNestedObject,
  JSXArrayValue,
  JSXArraySpread,
  JSExpressionNestedArray,
  JSExpressionFunctionCall,
  JSIdentifier,
  JSPropertyAccess,
  JSElementAccess,
  JSXAttributesEntry,
  JSXAttributesSpread,
  JSXElementName,
  JSXElement,
  WithElementsWithin,
  JSXTextBlock,
  JSXFragment,
  JSXConditionalExpression,
  RegularParam,
  DestructuredParamPart,
  DestructuredObject,
  DestructuredArray,
  UtopiaJSXComponent,
  ImportStatement,
  UnparsedCode,
  SameFileOrigin,
  ImportedOrigin,
  Comment,
  JSArbitraryStatement,
  JSXProperty,
  JSXArrayElement,
  OptionallyChained,
  IdentifierOrAccess,
  JSExpression,
  JSExpressionMapOrOtherJavascript,
  JSXAttributesPart,
  JSXAttributes,
  JSXElementWithoutUID,
  JSXConditionalExpressionWithoutUID,
  JSXFragmentWithoutUID,
  JSXMapExpressionWithoutUID,
  ElementsWithin,
  JSXElementLike,
  JSXElementChild,
  JSXElementChildWithoutUID,
  JSXElementChildren,
  DestructuredArrayPart,
  BoundParam,
  Param,
  VarLetOrConst,
  FunctionDeclarationSyntax,
  BlockOrExpression,
  ArbitraryJSBlock,
  TopLevelElement,
  ComputedStyle,
  StyleAttributeMetadataEntry,
  StyleAttributeMetadata,
  ImportInfo,
  ActiveAndDefaultConditionValues,
  ConditionValue,
  DetectedLayoutSystem,
  TextDirection,
  HugProperty,
  HugPropertyWidthHeight,
  ElementsByUID,
  FunctionWrap,
} from 'utopia-shared/src/types/element-template'
import type { VariableData } from '../../components/canvas/ui-jsx-canvas'

export type {
  ParsedComments,
  MultiLineComment,
  SingleLineComment,
  WithComments,
  JSExpressionValue,
  PartOfJSXAttributeValue,
  JSXAttributeNotFound,
  JSOpaqueArbitraryStatement,
  JSAssignment,
  JSAssignmentStatement,
  JSExpressionOtherJavaScript,
  JSXMapExpression,
  JSXSpreadAssignment,
  JSXPropertyAssignment,
  JSExpressionNestedObject,
  JSXArrayValue,
  JSXArraySpread,
  JSExpressionNestedArray,
  JSExpressionFunctionCall,
  JSIdentifier,
  JSPropertyAccess,
  JSElementAccess,
  JSXAttributesEntry,
  JSXAttributesSpread,
  JSXElementName,
  JSXElement,
  WithElementsWithin,
  JSXTextBlock,
  JSXFragment,
  JSXConditionalExpression,
  RegularParam,
  DestructuredParamPart,
  DestructuredObject,
  DestructuredArray,
  UtopiaJSXComponent,
  ImportStatement,
  UnparsedCode,
  SameFileOrigin,
  ImportedOrigin,
  Comment,
  JSArbitraryStatement,
  JSXProperty,
  JSXArrayElement,
  OptionallyChained,
  IdentifierOrAccess,
  JSExpression,
  JSExpressionMapOrOtherJavascript,
  JSXAttributesPart,
  JSXAttributes,
  JSXElementWithoutUID,
  JSXConditionalExpressionWithoutUID,
  JSXFragmentWithoutUID,
  JSXMapExpressionWithoutUID,
  ElementsWithin,
  JSXElementLike,
  JSXElementChild,
  JSXElementChildWithoutUID,
  JSXElementChildren,
  DestructuredArrayPart,
  BoundParam,
  Param,
  VarLetOrConst,
  FunctionDeclarationSyntax,
  BlockOrExpression,
  ArbitraryJSBlock,
  TopLevelElement,
  ComputedStyle,
  StyleAttributeMetadataEntry,
  StyleAttributeMetadata,
  ImportInfo,
  ActiveAndDefaultConditionValues,
  ConditionValue,
  DetectedLayoutSystem,
  TextDirection,
  HugProperty,
  HugPropertyWidthHeight,
  ElementsByUID,
}

import {
  emptyComments,
  emptyComputedStyle,
  emptyAttributeMetadata,
  simpleFunctionWrap,
} from 'utopia-shared/src/types/element-template'
export { emptyComments, emptyComputedStyle, emptyAttributeMetadata }

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

export function isMultiLineComment(comment: Comment): comment is MultiLineComment {
  return comment.type === 'MULTI_LINE_COMMENT'
}

export function isSingleLineComment(comment: Comment): comment is SingleLineComment {
  return comment.type === 'SINGLE_LINE_COMMENT'
}

export function isWithComments(e: unknown): e is WithComments {
  return (e as WithComments).comments != null
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

export function partOfJsxAttributeValue(value: any): PartOfJSXAttributeValue {
  return {
    type: 'PART_OF_ATTRIBUTE_VALUE',
    value: value,
  }
}

export function jsxAttributeNotFound(): JSXAttributeNotFound {
  return {
    type: 'ATTRIBUTE_NOT_FOUND',
  }
}

export function jsOpaqueArbitraryStatement(
  originalJavascript: string,
  definedWithin: Array<string>,
  definedElsewhere: Array<string>,
  uid: string,
): JSOpaqueArbitraryStatement {
  return {
    type: 'JS_OPAQUE_ARBITRARY_STATEMENT',
    originalJavascript: originalJavascript,
    definedWithin: definedWithin,
    definedElsewhere: definedElsewhere,
    uid: uid,
  }
}

export function jsAssignment(leftHandSide: BoundParam, rightHandSide: JSExpression): JSAssignment {
  return {
    type: 'JS_ASSIGNMENT',
    leftHandSide: leftHandSide,
    rightHandSide: rightHandSide,
  }
}

export function jsAssignmentStatement(
  declarationKeyword: 'let' | 'const' | 'var',
  assignments: Array<JSAssignment>,
  uid: string,
): JSAssignmentStatement {
  return {
    type: 'JS_ASSIGNMENT_STATEMENT',
    declarationKeyword: declarationKeyword,
    assignments: assignments,
    uid: uid,
  }
}

export function simpleJSAssignmentStatement(
  declarationKeyword: JSAssignmentStatement['declarationKeyword'],
  name: string,
  value: unknown,
): JSAssignmentStatement {
  return jsAssignmentStatement(
    declarationKeyword,
    [
      jsAssignment(
        regularParam(name, jsExpressionValue(value, emptyComments)),
        jsExpressionValue(value, emptyComments),
      ),
    ],
    '',
  )
}

export function isJSAssignmentStatement(
  statement: JSArbitraryStatement,
): statement is JSAssignmentStatement {
  return statement.type === 'JS_ASSIGNMENT_STATEMENT'
}

export function isJSOpaqueArbitraryStatement(
  statement: JSArbitraryStatement,
): statement is JSOpaqueArbitraryStatement {
  return statement.type === 'JS_OPAQUE_ARBITRARY_STATEMENT'
}

export function jsExpressionOtherJavaScript(
  params: Array<Param>,
  originalJavascript: string,
  javascriptWithUIDs: string,
  transpiledJavascript: string,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
  comments: ParsedComments,
  uid: string = UUID(),
): JSExpressionOtherJavaScript {
  return {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    params: params,
    originalJavascript: originalJavascript,
    javascriptWithUIDs: javascriptWithUIDs,
    transpiledJavascript: transpiledJavascript,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uid: uid,
    comments: comments,
    elementsWithin: elementsWithin,
  }
}

export function jsExpressionOtherJavaScriptSimple(
  javascript: string,
  definedElsewhere: Array<string>,
): JSExpressionOtherJavaScript {
  return jsExpressionOtherJavaScript(
    [],
    javascript,
    javascript,
    `return ${javascript}`,
    definedElsewhere,
    null,
    {},
    emptyComments,
  )
}

export function jsxMapExpression(
  valueToMap: JSExpression,
  mapFunction: JSExpression,
  comments: ParsedComments,
  valuesInScopeFromParameters: Array<string>,
  uid: string = UUID(),
): JSXMapExpression {
  return {
    type: 'JSX_MAP_EXPRESSION',
    valueToMap: valueToMap,
    mapFunction: mapFunction,
    uid: uid,
    comments: comments,
    valuesInScopeFromParameters: valuesInScopeFromParameters,
  }
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

export function jsxPropertyAssignment(
  key: string | number,
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

export function isSpreadAssignment(property: JSXProperty): property is JSXSpreadAssignment {
  return property.type === 'SPREAD_ASSIGNMENT'
}

export function isPropertyAssignment(property: JSXProperty): property is JSXPropertyAssignment {
  return property.type === 'PROPERTY_ASSIGNMENT'
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

export function jsxArrayValue(value: JSExpression, comments: ParsedComments): JSXArrayValue {
  return {
    type: 'ARRAY_VALUE',
    value: value,
    comments: comments,
  }
}

export function jsxArraySpread(value: JSExpression, comments: ParsedComments): JSXArraySpread {
  return {
    type: 'ARRAY_SPREAD',
    value: value,
    comments: comments,
  }
}

export function isArrayValue(elem: JSXArrayElement): elem is JSXArrayValue {
  return elem.type === 'ARRAY_VALUE'
}

export function isArraySpread(elem: JSXArrayElement): elem is JSXArraySpread {
  return elem.type === 'ARRAY_SPREAD'
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

export function jsIdentifier(
  name: string,
  uid: string,
  sourceMap: RawSourceMap | null,
  comments: ParsedComments,
): JSIdentifier {
  return {
    type: 'JS_IDENTIFIER',
    name: name,
    uid: uid,
    sourceMap: sourceMap,
    comments: comments,
  }
}

export function isJSIdentifier(expression: JSXElementChild): expression is JSIdentifier {
  return expression.type === 'JS_IDENTIFIER'
}

export function isJSIdentifierForName(
  expression: JSExpression,
  name: string,
): expression is JSIdentifier {
  return isJSIdentifier(expression) && expression.name === name
}

export function jsPropertyAccess(
  onValue: JSExpression,
  property: string,
  uid: string,
  sourceMap: RawSourceMap | null,
  comments: ParsedComments,
  originalJavascript: string,
  optionallyChained: OptionallyChained,
): JSPropertyAccess {
  return {
    type: 'JS_PROPERTY_ACCESS',
    onValue: onValue,
    property: property,
    uid: uid,
    sourceMap: sourceMap,
    originalJavascript: originalJavascript,
    optionallyChained: optionallyChained,
    comments: comments,
  }
}

export function isJSPropertyAccess(expression: JSXElementChild): expression is JSPropertyAccess {
  return expression.type === 'JS_PROPERTY_ACCESS'
}

export function isJSPropertyAccessForProperty(
  expression: JSExpression,
  property: string,
): expression is JSPropertyAccess {
  return isJSPropertyAccess(expression) && expression.property === property
}

export function jsElementAccess(
  onValue: JSExpression,
  element: JSExpression,
  uid: string,
  sourceMap: RawSourceMap | null,
  comments: ParsedComments,
  originalJavascript: string,
  optionallyChained: OptionallyChained,
): JSElementAccess {
  return {
    type: 'JS_ELEMENT_ACCESS',
    onValue: onValue,
    element: element,
    uid: uid,
    sourceMap: sourceMap,
    originalJavascript: originalJavascript,
    optionallyChained: optionallyChained,
    comments: comments,
  }
}

export function isJSElementAccess(expression: JSXElementChild): expression is JSElementAccess {
  return expression.type === 'JS_ELEMENT_ACCESS'
}

export function isIdentifierOrAccess(
  expression: JSXElementChild,
): expression is IdentifierOrAccess {
  return (
    isJSIdentifier(expression) || isJSPropertyAccess(expression) || isJSElementAccess(expression)
  )
}

export function clearJSXMapExpressionUniqueIDs(mapExpression: JSXMapExpression): JSXMapExpression {
  const updatedValueToMap = clearExpressionUniqueIDs(mapExpression.valueToMap)
  const updatedMapFunction = clearExpressionUniqueIDs(mapExpression.mapFunction)
  return {
    ...mapExpression,
    uid: '',
    valueToMap: updatedValueToMap,
    mapFunction: updatedMapFunction,
  }
}

export function clearJSExpressionOtherJavaScriptUniqueIDs(
  expression: JSExpressionOtherJavaScript,
): JSExpressionOtherJavaScript {
  const updatedElementsWithin = objectMap(clearJSXElementLikeUniqueIDs, expression.elementsWithin)
  return {
    ...expression,
    uid: '',
    elementsWithin: updatedElementsWithin,
  }
}

export function clearJSExpressionOtherJavaScriptOrMapExpressionUniqueIDs(
  expression: JSExpressionMapOrOtherJavascript,
): JSExpressionMapOrOtherJavascript {
  if (isJSXMapExpression(expression)) {
    return clearJSXMapExpressionUniqueIDs(expression)
  } else {
    return clearJSExpressionOtherJavaScriptUniqueIDs(expression)
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
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JS_IDENTIFIER':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
    case 'JSX_ELEMENT':
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
        return jsExpressionValue(simpleArray, attribute.comments, attribute.uid)
      } else {
        return jsExpressionNestedArray(notSoSimpleArray, attribute.comments, attribute.uid)
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
        return jsExpressionValue(simpleObject, attribute.comments, attribute.uid)
      } else {
        return jsExpressionNestedObject(notSoSimpleObject, attribute.comments, attribute.uid)
      }
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function clearIdentifierUniqueIDs(identifier: JSIdentifier): JSIdentifier {
  return jsIdentifier(identifier.name, '', identifier.sourceMap, identifier.comments)
}

export function clearExpressionUniqueIDs(attribute: JSExpression): JSExpression {
  switch (attribute.type) {
    case 'JSX_ELEMENT':
      return jsxElement(attribute.name, '', attribute.props, attribute.children)
    case 'ATTRIBUTE_VALUE':
      return jsExpressionValue(attribute.value, attribute.comments, '')
    case 'JSX_MAP_EXPRESSION':
      return clearJSXMapExpressionUniqueIDs(attribute)
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
    case 'JS_IDENTIFIER':
      return clearIdentifierUniqueIDs(attribute)
    case 'JS_PROPERTY_ACCESS':
      return jsPropertyAccess(
        clearExpressionUniqueIDs(attribute.onValue),
        attribute.property,
        '',
        attribute.sourceMap,
        attribute.comments,
        attribute.originalJavascript,
        attribute.optionallyChained,
      )
    case 'JS_ELEMENT_ACCESS':
      return jsElementAccess(
        clearExpressionUniqueIDs(attribute.onValue),
        clearExpressionUniqueIDs(attribute.element),
        '',
        attribute.sourceMap,
        attribute.comments,
        attribute.originalJavascript,
        attribute.optionallyChained,
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
  attribute: JSExpressionMapOrOtherJavascript,
): JSExpressionMapOrOtherJavascript {
  if (isJSXMapExpression(attribute)) {
    const updatedValueToMap = clearExpressionSourceMaps(attribute.valueToMap)
    const updatedMapFunction = clearExpressionSourceMaps(attribute.mapFunction)
    return {
      ...attribute,
      valueToMap: updatedValueToMap,
      mapFunction: updatedMapFunction,
    }
  } else {
    return {
      ...attribute,
      sourceMap: null,
    }
  }
}

function clearJSXElementChildSourceMaps(element: JSXElementChild): JSXElementChild {
  if (isJSXElement(element)) {
    return jsxElement(
      element.name,
      element.uid,
      clearAttributesSourceMaps(element.props),
      element.children.map((c) => clearJSXElementChildSourceMaps(c)),
    )
  } else if (isJSExpression(element)) {
    return clearExpressionSourceMaps(element)
  } else if (isJSXTextBlock(element)) {
    return element
  } else if (isJSXFragment(element)) {
    return jsxFragment(
      element.uid,
      element.children.map((c) => clearJSXElementChildSourceMaps(c)),
      element.longForm,
    )
  } else if (isJSXConditionalExpression(element)) {
    return jsxConditionalExpression(
      element.uid,
      element.condition,
      element.originalConditionString,
      clearJSXElementChildSourceMaps(element.whenTrue),
      clearJSXElementChildSourceMaps(element.whenFalse),
      element.comments,
    )
  } else {
    assertNever(element)
  }
}

export function clearIdentifierSourceMaps(identifier: JSIdentifier): JSIdentifier {
  return jsIdentifier(identifier.name, identifier.uid, null, identifier.comments)
}

export function clearExpressionSourceMaps(attribute: JSExpression): JSExpression {
  switch (attribute.type) {
    case 'JSX_ELEMENT':
      return jsxElement(
        attribute.name,
        attribute.uid,
        clearAttributesSourceMaps(attribute.props),
        attribute.children.map((c) => clearJSXElementChildSourceMaps(c)),
      )
    case 'ATTRIBUTE_VALUE':
      return attribute
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return clearJSXAttributeOtherJavaScriptSourceMaps(attribute)
    case 'ATTRIBUTE_NESTED_ARRAY':
      return jsExpressionNestedArray(
        attribute.content.map((elem) => {
          switch (elem.type) {
            case 'ARRAY_SPREAD':
              return jsxArraySpread(clearExpressionSourceMaps(elem.value), emptyComments)
            case 'ARRAY_VALUE':
              return jsxArrayValue(clearExpressionSourceMaps(elem.value), emptyComments)
            default:
              const _exhaustiveCheck: never = elem
              throw new Error(`Unhandled array element type ${JSON.stringify(elem)}`)
          }
        }),
        emptyComments,
        attribute.uid,
      )
    case 'JS_IDENTIFIER':
      return clearIdentifierSourceMaps(attribute)
    case 'JS_PROPERTY_ACCESS':
      return jsPropertyAccess(
        clearExpressionSourceMaps(attribute.onValue),
        attribute.property,
        attribute.uid,
        null,
        attribute.comments,
        attribute.originalJavascript,
        attribute.optionallyChained,
      )
    case 'JS_ELEMENT_ACCESS':
      return jsElementAccess(
        clearExpressionSourceMaps(attribute.onValue),
        clearExpressionSourceMaps(attribute.element),
        attribute.uid,
        null,
        attribute.comments,
        attribute.originalJavascript,
        attribute.optionallyChained,
      )
    case 'ATTRIBUTE_FUNCTION_CALL':
      return jsExpressionFunctionCall(
        attribute.functionName,
        attribute.parameters.map(clearExpressionSourceMaps),
        attribute.uid,
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      return jsExpressionNestedObject(
        attribute.content.map((prop) => {
          switch (prop.type) {
            case 'SPREAD_ASSIGNMENT':
              return jsxSpreadAssignment(clearExpressionSourceMaps(prop.value), emptyComments)
            case 'PROPERTY_ASSIGNMENT':
              return jsxPropertyAssignment(
                prop.key,
                clearExpressionSourceMaps(prop.value),
                emptyComments,
                emptyComments,
              )
            default:
              const _exhaustiveCheck: never = prop
              throw new Error(`Unhandled property type ${JSON.stringify(prop)}`)
          }
        }),
        emptyComments,
        attribute.uid,
      )
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled attribute ${JSON.stringify(attribute)}`)
  }
}

export function clearIdentifierUniqueIDsAndSourceMaps(identifier: JSIdentifier): JSIdentifier {
  return clearIdentifierUniqueIDs(clearIdentifierSourceMaps(identifier))
}

export function clearAssignmentUniqueIDsAndSourceMaps(assignment: JSAssignment): JSAssignment {
  return {
    type: 'JS_ASSIGNMENT',
    leftHandSide: clearBoundParamUniqueIDsAndSourceMaps(assignment.leftHandSide),
    rightHandSide: clearExpressionUniqueIDsAndSourceMaps(assignment.rightHandSide),
  }
}

export function clearExpressionUniqueIDsAndSourceMaps(expression: JSExpression): JSExpression {
  return clearExpressionUniqueIDs(clearExpressionSourceMaps(expression))
}

export function isJSXAttributeValue(element: JSXElementChild): element is JSExpressionValue<any> {
  return element.type === 'ATTRIBUTE_VALUE'
}

export function isNullJSXAttributeValue(element: JSXElementChild): boolean {
  if (!isJSXAttributeValue(element)) {
    return false
  }
  const value = jsxSimpleAttributeToValue(element)
  return isRight(value) && value.value === null
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

export function modifiableAttributeIsJsxElement(
  attribute: ModifiableAttribute,
): attribute is JSXElement {
  return attribute != null && attribute.type === 'JSX_ELEMENT'
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

export function isRegularJSXAttribute(attribute: ModifiableAttribute): attribute is JSExpression {
  return (
    attribute != null &&
    !modifiableAttributeIsPartOfAttributeValue(attribute) &&
    !modifiableAttributeIsAttributeNotFound(attribute)
  )
}

export function modifiableAttributeToValuePath(
  attribute: ModifiableAttribute,
): Either<string, Array<string | number>> {
  switch (attribute.type) {
    case 'JS_IDENTIFIER':
      return right([attribute.name])
    case 'JS_PROPERTY_ACCESS':
      return mapEither((onValueArray) => {
        return [...onValueArray, attribute.property]
      }, modifiableAttributeToValuePath(attribute.onValue))
    case 'JS_ELEMENT_ACCESS':
      return flatMapEither((onValueArray) => {
        if (isJSExpressionValue(attribute.element)) {
          switch (typeof attribute.element.value) {
            case 'number':
            case 'string':
              return right([...onValueArray, attribute.element.value])
          }
        }
        return left('Unable to handle this element access.')
      }, modifiableAttributeToValuePath(attribute.onValue))
    default:
      return left('Unable to handle this expression type.')
  }
}

export function jsxAttributesEntry(
  key: string | number,
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

export function isJSXAttributesEntry(part: JSXAttributesPart): part is JSXAttributesEntry {
  return part.type === 'JSX_ATTRIBUTES_ENTRY'
}

export function isJSXAttributesSpread(part: JSXAttributesPart): part is JSXAttributesSpread {
  return part.type === 'JSX_ATTRIBUTES_SPREAD'
}

export function jsxAttributesFromMap(map: MapLike<JSExpression>): Array<JSXAttributesEntry> {
  return Object.keys(map).map((objectKey) => {
    return jsxAttributesEntry(objectKey, map[objectKey], emptyComments)
  })
}

export function getJSXAttribute(
  attributes: JSXAttributes,
  key: string | number,
): JSExpression | null {
  for (let index = attributes.length - 1; index >= 0; index--) {
    const attrPart = attributes[index]
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

export function deleteJSXAttribute(attributes: JSXAttributes, key: string | number): JSXAttributes {
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
  key: string | number,
  value: JSExpression,
): JSXAttributes {
  let updatedExistingField: boolean = false
  let result: JSXAttributes = []

  for (const attrPart of attributes) {
    switch (attrPart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        if (attrPart.key === key) {
          result.push(jsxAttributesEntry(key, value, attrPart.comments))
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
    result.push(jsxAttributesEntry(key, value, emptyComments))
  }
  return result
}

const AllowedExternalReferences = ['React', 'utopiaCanvasJSXLookup']

export function attributeReferencesElsewhere(attribute: JSExpression): boolean {
  switch (attribute.type) {
    case 'JSX_ELEMENT':
      return (
        attribute.props.some(jsxAttributesPartReferencesElsewhere) ||
        attribute.children.some(elementReferencesElsewhere)
      )
    case 'ATTRIBUTE_VALUE':
      return false
    case 'JSX_MAP_EXPRESSION':
      return (
        attributeReferencesElsewhere(attribute.valueToMap) ||
        attributeReferencesElsewhere(attribute.mapFunction)
      )
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return (
        attribute.definedElsewhere.filter((r) => !AllowedExternalReferences.includes(r)).length > 0
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      return attribute.content.some((subAttr) => {
        return attributeReferencesElsewhere(subAttr.value)
      })
    case 'ATTRIBUTE_NESTED_ARRAY':
      return attribute.content.some((subAttr) => {
        return attributeReferencesElsewhere(subAttr.value)
      })
    case 'JS_IDENTIFIER':
      return true
    case 'JS_ELEMENT_ACCESS':
      return (
        attributeReferencesElsewhere(attribute.element) ||
        attributeReferencesElsewhere(attribute.onValue)
      )
    case 'JS_PROPERTY_ACCESS':
      return attributeReferencesElsewhere(attribute.onValue)
    case 'ATTRIBUTE_FUNCTION_CALL':
      return true
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
      return (
        element.props.some(jsxAttributesPartReferencesElsewhere) ||
        element.children.some(elementReferencesElsewhere)
      )
    case 'JSX_TEXT_BLOCK':
      return false
    case 'JSX_FRAGMENT':
      return element.children.some(elementReferencesElsewhere)
    case 'JSX_CONDITIONAL_EXPRESSION':
      return (
        elementReferencesElsewhere(element.condition) ||
        elementReferencesElsewhere(element.whenTrue) ||
        elementReferencesElsewhere(element.whenFalse)
      )
    default:
      return attributeReferencesElsewhere(element)
  }
}

export function getElementReferencesElsewherePathsFromProps(
  element: JSXElementChild,
  pathSoFar: PropertyPath,
): PropertyPath[] {
  switch (element.type) {
    case 'JSX_ELEMENT':
      return element.props.flatMap((prop) =>
        prop.type === 'JSX_ATTRIBUTES_SPREAD'
          ? []
          : getElementReferencesElsewherePathsFromProps(
              prop.value,
              PP.append(pathSoFar, PP.create(prop.key)),
            ),
      )
    case 'ATTRIBUTE_NESTED_OBJECT':
      const spreads: JSXSpreadAssignment[] = []
      const assigments: JSXPropertyAssignment[] = []
      element.content.forEach((c) => {
        switch (c.type) {
          case 'PROPERTY_ASSIGNMENT':
            assigments.push(c)
            break
          case 'SPREAD_ASSIGNMENT':
            spreads.push(c)
            break
          default:
            assertNever(c)
        }
      })
      if (spreads.length > 0) {
        return [pathSoFar] // if a spread assignment is present, overwrite the whole prop
      }
      return assigments.flatMap((assignment) =>
        getElementReferencesElsewherePathsFromProps(
          assignment.value,
          PP.append(pathSoFar, PP.create(assignment.key)),
        ),
      )
    case 'JSX_FRAGMENT':
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_TEXT_BLOCK':
      return [] // no props present on these elements
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JS_IDENTIFIER':
      return [pathSoFar] // replace the property corresponding to these values
    case 'JS_PROPERTY_ACCESS':
      return [PP.append(pathSoFar, PP.create(element.property))]
    case 'JS_ELEMENT_ACCESS':
      const onValuePaths = getElementReferencesElsewherePathsFromProps(element.onValue, pathSoFar)
      return onValuePaths.flatMap((onValuePath) => {
        return getElementReferencesElsewherePathsFromProps(element.element, onValuePath)
      })
    default:
      assertNever(element)
  }
}

export function getDefinedElsewhereFromAttribute(attribute: JSExpression): Array<string> {
  switch (attribute.type) {
    case 'JSX_ELEMENT':
      return getDefinedElsewhereFromElement(attribute)
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return attribute.definedElsewhere
    case 'ATTRIBUTE_NESTED_OBJECT':
      return attribute.content.reduce<Array<string>>((working, elem) => {
        return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem.value))
      }, [])
    case 'ATTRIBUTE_NESTED_ARRAY':
      return attribute.content.reduce<Array<string>>((working, elem) => {
        return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem.value))
      }, [])
    case 'JS_PROPERTY_ACCESS':
      return getDefinedElsewhereFromAttribute(attribute.onValue)
    case 'JS_ELEMENT_ACCESS':
      return addAllUniquely(
        getDefinedElsewhereFromAttribute(attribute.onValue),
        getDefinedElsewhereFromAttribute(attribute.element),
      )
    case 'JS_IDENTIFIER':
      return [attribute.name]
    case 'ATTRIBUTE_VALUE':
      return []
    case 'JSX_MAP_EXPRESSION':
      return addAllUniquely(
        getDefinedElsewhereFromAttribute(attribute.valueToMap),
        getDefinedElsewhereFromAttribute(attribute.mapFunction),
      )
    case 'ATTRIBUTE_FUNCTION_CALL':
      return attribute.parameters.reduce<Array<string>>((working, elem) => {
        return addAllUniquely(working, getDefinedElsewhereFromAttribute(elem))
      }, [])
    default:
      assertNever(attribute)
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

export function getDefinedElsewhereFromElementChild(
  working: Array<string>,
  child: JSXElementChild,
): Array<string> {
  switch (child.type) {
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return addAllUniquely(working, child.definedElsewhere)
    case 'JSX_MAP_EXPRESSION':
      return addAllUniquely(working, [
        ...getDefinedElsewhereFromAttribute(child.valueToMap),
        ...getDefinedElsewhereFromAttribute(child.mapFunction),
      ])
    case 'JSX_CONDITIONAL_EXPRESSION':
      const withCondition = getDefinedElsewhereFromElementChild(working, child.condition)
      const withWhenTrue = getDefinedElsewhereFromElementChild(withCondition, child.whenTrue)
      return getDefinedElsewhereFromElementChild(withWhenTrue, child.whenFalse)
    case 'JSX_ELEMENT':
      return addAllUniquely(working, getDefinedElsewhereFromElement(child))
    case 'JSX_TEXT_BLOCK':
    case 'JSX_FRAGMENT':
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return working
    case 'ATTRIBUTE_NESTED_ARRAY':
      return child.content.reduce((innerWorking, contentElement) => {
        switch (contentElement.type) {
          case 'ARRAY_VALUE':
            return getDefinedElsewhereFromElementChild(innerWorking, contentElement.value)
          case 'ARRAY_SPREAD':
            return getDefinedElsewhereFromElementChild(innerWorking, contentElement.value)
          default:
            return assertNever(contentElement)
        }
      }, working)
    case 'ATTRIBUTE_NESTED_OBJECT':
      return child.content.reduce((innerWorking, contentElement) => {
        switch (contentElement.type) {
          case 'PROPERTY_ASSIGNMENT':
            return getDefinedElsewhereFromElementChild(innerWorking, contentElement.value)
          case 'SPREAD_ASSIGNMENT':
            return getDefinedElsewhereFromElementChild(innerWorking, contentElement.value)
          default:
            return assertNever(contentElement)
        }
      }, working)
    case 'JS_IDENTIFIER':
      return [...working, child.name]
    case 'JS_ELEMENT_ACCESS':
      const withOnValue = getDefinedElsewhereFromElementChild(working, child.onValue)
      return getDefinedElsewhereFromElementChild(withOnValue, child.element)
    case 'JS_PROPERTY_ACCESS':
      return getDefinedElsewhereFromElementChild(working, child.onValue)
    default:
      assertNever(child)
  }
}

export function getDefinedElsewhereFromElement(element: JSXElementLike): Array<string> {
  const fromAttributes = isJSXElement(element)
    ? getDefinedElsewhereFromAttributes(element.props)
    : []
  return element.children.reduce(
    (working, child) => getDefinedElsewhereFromElementChild(working, child),
    fromAttributes,
  )
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
          clearExpressionSourceMaps(attribute.value),
          attribute.comments,
        )
      case 'JSX_ATTRIBUTES_SPREAD':
        return jsxAttributesSpread(
          clearExpressionSourceMaps(attribute.spreadValue),
          attribute.comments,
        )
      default:
        const _exhaustiveCheck: never = attribute
        throw new Error(`Unhandled attribute type ${JSON.stringify(attribute)}`)
    }
  })
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

export function jsxElementNameFromString(name: string): JSXElementName {
  const [baseVariable, ...properties] = name.split('.')
  return jsxElementName(baseVariable, properties)
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

export function isSVGElement(name: JSXElementName): boolean {
  return PP.depth(name.propertyPath) === 0 && name.baseVariable === 'svg'
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

export function getJSXElementLikeNameAsString(element: JSXElementLike): string {
  switch (element.type) {
    case 'JSX_ELEMENT':
      return getJSXElementNameAsString(element.name)
    case 'JSX_FRAGMENT':
      return 'Fragment'
    default:
      assertNever(element)
  }
}

export function clearJSXMapExpressionWithoutUIDUniqueIDs(
  mapExpression: JSXMapExpressionWithoutUID,
): JSXMapExpressionWithoutUID {
  const updatedValueToMap = clearExpressionUniqueIDs(mapExpression.valueToMap)
  const updatedMapFunction = clearExpressionUniqueIDs(mapExpression.mapFunction)
  return {
    ...mapExpression,
    valueToMap: updatedValueToMap,
    mapFunction: updatedMapFunction,
  }
}

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

export function hasElementsWithin(e: unknown): e is WithElementsWithin {
  return (e as WithElementsWithin).elementsWithin != null
}

export function jsxTextBlock(text: string, uid: string = UUID()): JSXTextBlock {
  return {
    type: 'JSX_TEXT_BLOCK',
    text: text,
    uid: uid,
  }
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

export function jsxConditionalExpressionConditionOptic(
  condition: 'whenTrue' | 'whenFalse',
): Optic<JSXConditionalExpression, JSXElementChild> {
  return fromField(condition)
}

export function canBeRootElementOfComponent(element: JSXElementChild): boolean {
  if (isJSXElement(element) || isJSXFragment(element) || isJSXConditionalExpression(element)) {
    return true
  }

  if (isJSExpression(element)) {
    if (hasElementsWithin(element)) {
      return Object.keys(element.elementsWithin).length > 0
    }
  }

  return false
}

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

export function isJSXMapExpression(element: JSXElementChild): element is JSXMapExpression {
  return element.type === 'JSX_MAP_EXPRESSION'
}

export function isJSExpressionMapOrOtherJavaScript(
  element: JSXElementChild,
): element is JSExpressionMapOrOtherJavascript {
  return isJSExpressionOtherJavaScript(element) || isJSXMapExpression(element)
}

export function isJSExpression(element: JSXElementChild): element is JSExpression {
  switch (element.type) {
    case 'JSX_ELEMENT':
    case 'JSX_TEXT_BLOCK':
    case 'JSX_FRAGMENT':
    case 'JSX_CONDITIONAL_EXPRESSION':
      return false
    case 'ATTRIBUTE_VALUE':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_IDENTIFIER':
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

interface ElementWithUid {
  uid: string
}

export function isElementWithUid(element: unknown): element is ElementWithUid {
  return (element as ElementWithUid).uid != null
}

export function clearJSXElementLikeUniqueIDs(element: JSXElementLike): JSXElementLike {
  switch (element.type) {
    case 'JSX_ELEMENT':
      return clearJSXElementUniqueIDs(element)
    case 'JSX_FRAGMENT':
      return clearJSXFragmentUniqueIDs(element)
    default:
      assertNever(element)
  }
}

export function clearJSXFragmentUniqueIDs(element: JSXFragment): JSXFragment {
  const updatedChildren: JSXElementChildren = element.children.map(clearJSXElementChildUniqueIDs)
  return {
    ...element,
    children: updatedChildren,
    uid: '',
  }
}

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
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JS_IDENTIFIER':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_ELEMENT_ACCESS':
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

export function jsxElementFromJSXElementWithoutUID(
  element: JSXElementWithoutUID,
  uid: string,
): JSXElement {
  return {
    ...element,
    uid: uid,
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
  functionWrapping: Array<FunctionWrap>,
  params: Array<Param> | null,
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
    functionWrapping: functionWrapping,
    params: params,
    propsUsed: propsUsed,
    rootElement: rootElement,
    arbitraryJSBlock: jsBlock,
    usedInReactDOMRender: usedInReactDOMRender,
    returnStatementComments: returnStatementComments,
  }
}

// FIXME we need to inject data-uids using insertDataUIDsIntoCode
export function arbitraryJSBlock(
  params: Array<Param>,
  javascript: string,
  transpiledJavascript: string,
  definedWithin: Array<string>,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
  statements: Array<JSArbitraryStatement>,
  uid: string = UUID(),
): ArbitraryJSBlock {
  return {
    type: 'ARBITRARY_JS_BLOCK',
    params: params,
    javascript: javascript,
    transpiledJavascript: transpiledJavascript,
    definedWithin: definedWithin,
    definedElsewhere: definedElsewhere,
    sourceMap: sourceMap,
    uid: uid,
    elementsWithin: elementsWithin,
    statements: statements,
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

export function regularParam(
  paramName: string,
  defaultExpression: JSExpression | null,
): RegularParam {
  return {
    type: 'REGULAR_PARAM',
    paramName: paramName,
    defaultExpression: defaultExpression,
  }
}

export function destructuredParamPart(
  propertyName: string | undefined,
  param: Param,
  defaultExpression: JSExpression | null,
): DestructuredParamPart {
  return {
    propertyName: propertyName,
    param: param,
    defaultExpression: defaultExpression,
  }
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

export function isOmittedParam(param: DestructuredArrayPart): param is OmittedParam {
  return param.type === 'OMITTED_PARAM'
}

export function isParam(param: DestructuredArrayPart): param is Param {
  return param.type === 'PARAM'
}

export function destructuredArray(parts: Array<DestructuredArrayPart>): DestructuredArray {
  return {
    type: 'DESTRUCTURED_ARRAY',
    parts: parts,
  }
}

export function isRegularParam(param: BoundParam): param is RegularParam {
  return param.type === 'REGULAR_PARAM'
}

export function isDestructuredObject(param: BoundParam): param is DestructuredObject {
  return param.type === 'DESTRUCTURED_OBJECT'
}

export function isDestructuredArray(param: BoundParam): param is DestructuredArray {
  return param.type === 'DESTRUCTURED_ARRAY'
}

export function functionParam(dotDotDotToken: boolean, boundParam: BoundParam): Param {
  return {
    type: 'PARAM',
    dotDotDotToken: dotDotDotToken,
    boundParam: boundParam,
  }
}

export const defaultPropsParam: Array<Param> = [functionParam(false, regularParam('props', null))]

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

export function propertiesExposedByParams(params: Array<Param>): Array<string> {
  return params.flatMap(propertiesExposedByParam)
}

export function propertiesExposedByParam(param: Param): Array<string> {
  switch (param.boundParam.type) {
    case 'REGULAR_PARAM':
      return [param.boundParam.paramName]
    case 'DESTRUCTURED_ARRAY':
      return param.boundParam.parts.flatMap((part) => {
        switch (part.type) {
          case 'PARAM':
            return propertiesExposedByParam(part)
          case 'OMITTED_PARAM':
            return []
          default:
            return assertNever(part)
        }
      })
    case 'DESTRUCTURED_OBJECT':
      return param.boundParam.parts.flatMap((part) => {
        return propertiesExposedByParam(part.param)
      })
    default:
      assertNever(param.boundParam)
  }
}

export function clearArbitraryJSBlockUniqueIDs(block: ArbitraryJSBlock): ArbitraryJSBlock {
  return {
    ...block,
    statements: block.statements.map(clearJSArbitraryStatementUniqueIDs),
    uid: '',
  }
}

export function clearArbitraryJSBlockSourceMaps(block: ArbitraryJSBlock): ArbitraryJSBlock {
  return {
    ...block,
    statements: block.statements.map(clearJSArbitraryStatementSourceMaps),
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
      : clearExpressionUniqueIDs(paramPart.defaultExpression),
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
          : clearExpressionUniqueIDs(boundParam.defaultExpression),
      )
    default:
      const _exhaustiveCheck: never = boundParam
      throw new Error(`Unhandled element ${JSON.stringify(boundParam)}`)
  }
}

export function clearDestructuredArrayPartSourceMaps(
  arrayPart: DestructuredArrayPart,
): DestructuredArrayPart {
  switch (arrayPart.type) {
    case 'OMITTED_PARAM':
      return arrayPart
    case 'PARAM':
      return clearParamSourceMaps(arrayPart)
    default:
      const _exhaustiveCheck: never = arrayPart
      throw new Error(`Unhandled array part ${JSON.stringify(arrayPart)}`)
  }
}

export function clearDestructuredParamPartSourceMaps(
  paramPart: DestructuredParamPart,
): DestructuredParamPart {
  return {
    propertyName: paramPart.propertyName,
    param: clearParamSourceMaps(paramPart.param),
    defaultExpression:
      paramPart.defaultExpression == null
        ? null
        : clearExpressionSourceMaps(paramPart.defaultExpression),
  }
}

export function clearParamSourceMaps(param: Param): Param {
  return {
    ...param,
    boundParam: clearBoundParamSourceMaps(param.boundParam),
  }
}

export function clearBoundParamSourceMaps(boundParam: BoundParam): BoundParam {
  switch (boundParam.type) {
    case 'DESTRUCTURED_ARRAY':
      return destructuredArray(boundParam.parts.map(clearDestructuredArrayPartSourceMaps))
    case 'DESTRUCTURED_OBJECT':
      return destructuredObject(boundParam.parts.map(clearDestructuredParamPartSourceMaps))
    case 'REGULAR_PARAM':
      return regularParam(
        boundParam.paramName,
        boundParam.defaultExpression == null
          ? null
          : clearExpressionSourceMaps(boundParam.defaultExpression),
      )
    default:
      const _exhaustiveCheck: never = boundParam
      throw new Error(`Unhandled element ${JSON.stringify(boundParam)}`)
  }
}

export function clearBoundParamUniqueIDsAndSourceMaps(boundParam: BoundParam): BoundParam {
  return clearBoundParamSourceMaps(clearBoundParamUniqueIDs(boundParam))
}

export function clearParamUniqueIDs(param: Param): Param {
  return functionParam(param.dotDotDotToken, clearBoundParamUniqueIDs(param.boundParam))
}

export function clearFunctionWrapUniqueIDs(wrap: FunctionWrap): FunctionWrap {
  switch (wrap.type) {
    case 'SIMPLE_FUNCTION_WRAP':
      return simpleFunctionWrap(clearExpressionUniqueIDs(wrap.functionExpression))
  }
}

export function clearFunctionWrapSourceMaps(wrap: FunctionWrap): FunctionWrap {
  switch (wrap.type) {
    case 'SIMPLE_FUNCTION_WRAP':
      return simpleFunctionWrap(clearExpressionSourceMaps(wrap.functionExpression))
  }
}

export function clearFunctionWrapUniqueIDsAndSourceMaps(wrap: FunctionWrap): FunctionWrap {
  return clearFunctionWrapSourceMaps(clearFunctionWrapUniqueIDs(wrap))
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
        functionWrapping: element.functionWrapping.map(clearFunctionWrapUniqueIDsAndSourceMaps),
      }
      if (updatedComponent.arbitraryJSBlock != null) {
        updatedComponent.arbitraryJSBlock = clearArbitraryJSBlockUniqueIDs(
          updatedComponent.arbitraryJSBlock,
        )
      }
      if (updatedComponent.params != null) {
        updatedComponent.params = updatedComponent.params.map(clearParamUniqueIDs)
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

export function clearTopLevelElementSourceMaps(element: UtopiaJSXComponent): UtopiaJSXComponent
export function clearTopLevelElementSourceMaps(element: ArbitraryJSBlock): ArbitraryJSBlock
export function clearTopLevelElementSourceMaps(element: TopLevelElement): TopLevelElement
export function clearTopLevelElementSourceMaps(element: TopLevelElement): TopLevelElement {
  switch (element.type) {
    case 'UTOPIA_JSX_COMPONENT':
      let updatedComponent: UtopiaJSXComponent = {
        ...element,
        rootElement: clearJSXElementChildSourceMaps(element.rootElement),
      }
      if (updatedComponent.arbitraryJSBlock != null) {
        updatedComponent.arbitraryJSBlock = clearArbitraryJSBlockSourceMaps(
          updatedComponent.arbitraryJSBlock,
        )
      }
      if (updatedComponent.params != null) {
        updatedComponent.params = updatedComponent.params.map(clearParamSourceMaps)
      }
      return updatedComponent
    case 'ARBITRARY_JS_BLOCK':
      return clearArbitraryJSBlockSourceMaps(element)
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

export type ElementInstanceMetadataMap = { [key: string]: Readonly<ElementInstanceMetadata> }
export const emptyJsxMetadata: ElementInstanceMetadataMap = {}

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

export interface EarlyReturnVoid {
  type: 'EARLY_RETURN_VOID'
  spiedVariablesDeclaredWithinBlock: VariableData
}

export function earlyReturnVoid(): EarlyReturnVoid {
  return {
    type: 'EARLY_RETURN_VOID',
    spiedVariablesDeclaredWithinBlock: {},
  }
}

export interface EarlyReturnResult {
  type: 'EARLY_RETURN_RESULT'
  result: unknown
  spiedVariablesDeclaredWithinBlock: VariableData
}

export function earlyReturnResult(result: unknown): EarlyReturnResult {
  return {
    type: 'EARLY_RETURN_RESULT',
    result: result,
    spiedVariablesDeclaredWithinBlock: {},
  }
}

export type EarlyReturn = EarlyReturnVoid | EarlyReturnResult

export interface ArbitraryBlockRanToEnd {
  type: 'ARBITRARY_BLOCK_RAN_TO_END'
  scope: MapLike<unknown>
  spiedVariablesDeclaredWithinBlock: VariableData
}

export function arbitraryBlockRanToEnd(
  scope: MapLike<unknown>,
  spiedVariablesDeclaredWithinBlock: VariableData,
): ArbitraryBlockRanToEnd {
  return {
    type: 'ARBITRARY_BLOCK_RAN_TO_END',
    scope: scope,
    spiedVariablesDeclaredWithinBlock: spiedVariablesDeclaredWithinBlock,
  }
}

export type ArbitraryBlockResult = EarlyReturn | ArbitraryBlockRanToEnd

export interface ElementInstanceMetadata {
  elementPath: ElementPath
  element: Either<string, JSXElementChild>
  globalFrame: MaybeInfinityCanvasRectangle | null
  nonRoundedGlobalFrame: MaybeInfinityCanvasRectangle | null
  componentInstance: boolean
  isEmotionOrStyledComponent: boolean
  specialSizeMeasurements: SpecialSizeMeasurements
  computedStyle: ComputedStyle | null
  attributeMetadata: StyleAttributeMetadata | null
  label: string | null
  importInfo: ImportInfo | null
  conditionValue: ConditionValue
  textContent: string | null
  earlyReturn: EarlyReturn | null
  assignedToProp: string | null
}

export interface DomElementMetadata {
  element: Either<string, JSXElementChild>
  globalFrame: MaybeInfinityCanvasRectangle | null
  nonRoundedGlobalFrame: MaybeInfinityCanvasRectangle | null
  specialSizeMeasurements: SpecialSizeMeasurements
  textContent: string | null

  computedStyle: ComputedStyle | null
  attributeMetadata: StyleAttributeMetadata | null
}

export interface ComputedStyleMetadata {
  computedStyle: ComputedStyle
  attributeMetadata: StyleAttributeMetadata
}

export function elementInstanceMetadata(
  elementPath: ElementPath,
  element: Either<string, JSXElementChild>,
  globalFrame: MaybeInfinityCanvasRectangle | null,
  nonRoundedGlobalFrame: MaybeInfinityCanvasRectangle | null,
  componentInstance: boolean,
  isEmotionOrStyledComponent: boolean,
  sizeMeasurements: SpecialSizeMeasurements,
  computedStyle: ComputedStyle | null,
  attributeMetadata: StyleAttributeMetadata | null,
  label: string | null,
  importInfo: ImportInfo | null,
  conditionValue: ConditionValue,
  textContent: string | null,
  earlyReturn: EarlyReturnResult | EarlyReturnVoid | null,
  assignedToProp: string | null,
): ElementInstanceMetadata {
  return {
    elementPath: elementPath,
    element: element,
    globalFrame: globalFrame,
    nonRoundedGlobalFrame: nonRoundedGlobalFrame,
    componentInstance: componentInstance,
    isEmotionOrStyledComponent: isEmotionOrStyledComponent,
    specialSizeMeasurements: sizeMeasurements,
    computedStyle: computedStyle,
    attributeMetadata: attributeMetadata,
    label: label,
    importInfo: importInfo,
    conditionValue: conditionValue,
    textContent: textContent,
    earlyReturn: earlyReturn,
    assignedToProp: assignedToProp,
  }
}

export function domElementMetadata(
  element: Either<string, JSXElementChild>,
  globalFrame: MaybeInfinityCanvasRectangle | null,
  nonRoundedGlobalFrame: MaybeInfinityCanvasRectangle | null,
  sizeMeasurements: SpecialSizeMeasurements,
  textContent: string | null,
): DomElementMetadata {
  return {
    element: element,
    globalFrame: globalFrame,
    nonRoundedGlobalFrame: nonRoundedGlobalFrame,
    specialSizeMeasurements: sizeMeasurements,
    textContent: textContent,

    computedStyle: null,
    attributeMetadata: null,
  }
}

export function computedStyleMetadata(
  computedStyle: ComputedStyle,
  attributeMetadata: StyleAttributeMetadata,
): ComputedStyleMetadata {
  return {
    computedStyle: computedStyle,
    attributeMetadata: attributeMetadata,
  }
}

export function metadataHasPositionAbsoluteOrNull(
  metadata: ElementInstanceMetadata | null | undefined,
): boolean {
  if (metadata == null) {
    return false
  } else {
    return (
      metadata.specialSizeMeasurements.position === 'absolute' ||
      metadata.specialSizeMeasurements.position === null
    )
  }
}

export type SettableLayoutSystem = 'flex' | 'flow' | 'grid' | LayoutSystem

export interface GridPositionValue {
  numericalPosition: number | null
}

export function gridPositionValue(numericalPosition: number | null): GridPositionValue {
  return {
    numericalPosition: numericalPosition,
  }
}

export const validGridPositionKeywords = ['auto']

export type ValidGridPositionKeyword = string // using <string> because valid keywords are also area names we cannot know in advance

export type GridPosition = GridPositionValue | CSSKeyword<ValidGridPositionKeyword>

export const isValidGridPositionKeyword =
  (labels: string[]) =>
  (u: unknown): u is ValidGridPositionKeyword => {
    if (u == null || typeof u !== 'string') {
      return false
    }
    if (validGridPositionKeywords.includes(u)) {
      return true
    }
    return labels.includes(u)
  }

export interface GridRange {
  start: GridPosition
  end: GridPosition | null
}

export function gridRange(start: GridPosition, end: GridPosition | null): GridRange {
  return {
    start: start,
    end: end,
  }
}

export type GridColumnStart = GridPosition
export type GridColumnEnd = GridPosition
export type GridRowStart = GridPosition
export type GridRowEnd = GridPosition

export interface GridAutoOrTemplateFallback {
  type: 'FALLBACK'
  value: string
}

export function gridAutoOrTemplateFallback(value: string): GridAutoOrTemplateFallback {
  return {
    type: 'FALLBACK',
    value: value,
  }
}

export interface GridAutoOrTemplateDimensions {
  type: 'DIMENSIONS'
  dimensions: Array<GridDimension>
}

export function gridAutoOrTemplateDimensions(
  dimensions: Array<GridDimension>,
): GridAutoOrTemplateDimensions {
  return {
    type: 'DIMENSIONS',
    dimensions: dimensions,
  }
}

export type GridAutoOrTemplateBase = GridAutoOrTemplateDimensions | GridAutoOrTemplateFallback

export function isGridAutoOrTemplateDimensions(
  value: GridAutoOrTemplateBase,
): value is GridAutoOrTemplateDimensions {
  return value.type === 'DIMENSIONS'
}

export type GridAuto = GridAutoOrTemplateBase
export type GridTemplate = GridAutoOrTemplateBase

export type GridTemplateColumns = GridTemplate
export type GridTemplateRows = GridTemplate
export type GridAutoColumns = GridAuto
export type GridAutoRows = GridAuto

export interface GridContainerProperties {
  gridTemplateColumns: GridTemplateColumns | null
  gridTemplateRows: GridTemplateRows | null
  gridAutoColumns: GridAutoColumns | null
  gridAutoRows: GridAutoRows | null
  gridAutoFlow: GridAutoFlow | null
}

export function gridContainerProperties(
  gridTemplateColumns: GridTemplateColumns | null,
  gridTemplateRows: GridTemplateRows | null,
  gridAutoColumns: GridAutoColumns | null,
  gridAutoRows: GridAutoRows | null,
  gridAutoFlow: GridAutoFlow | null,
): GridContainerProperties {
  return {
    gridTemplateColumns: gridTemplateColumns,
    gridTemplateRows: gridTemplateRows,
    gridAutoColumns: gridAutoColumns,
    gridAutoRows: gridAutoRows,
    gridAutoFlow: gridAutoFlow,
  }
}

export interface GridElementProperties {
  gridColumnStart: GridColumnStart | null
  gridColumnEnd: GridColumnEnd | null
  gridRowStart: GridRowStart | null
  gridRowEnd: GridRowEnd | null
}

export function gridElementProperties(
  gridColumnStart: GridColumnStart | null,
  gridColumnEnd: GridColumnEnd | null,
  gridRowStart: GridRowStart | null,
  gridRowEnd: GridRowEnd | null,
): GridElementProperties {
  return {
    gridColumnStart: gridColumnStart,
    gridColumnEnd: gridColumnEnd,
    gridRowStart: gridRowStart,
    gridRowEnd: gridRowEnd,
  }
}

export interface SpecialSizeMeasurements {
  offset: LocalPoint
  coordinateSystemBounds: CanvasRectangle | null
  immediateParentBounds: CanvasRectangle | null
  globalFrameWithTextContent: MaybeInfinityCanvasRectangle | null
  textBounds: CanvasRectangle | null
  immediateParentProvidesLayout: boolean
  closestOffsetParentPath: ElementPath | null
  usesParentBounds: boolean
  parentLayoutSystem: DetectedLayoutSystem // TODO make a specific boolean prop that tells us the parent is flex or not
  layoutSystemForChildren: DetectedLayoutSystem | null
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
  computedHugProperty: HugPropertyWidthHeight
  containerGridProperties: GridContainerProperties
  elementGridProperties: GridElementProperties
  containerGridPropertiesFromProps: GridContainerProperties
  elementGridPropertiesFromProps: GridElementProperties
  rowGap: number | null
  columnGap: number | null
}

export function specialSizeMeasurements(
  offset: LocalPoint,
  coordinateSystemBounds: CanvasRectangle | null,
  immediateParentBounds: CanvasRectangle | null,
  globalFrameWithTextContent: MaybeInfinityCanvasRectangle | null,
  immediateParentProvidesLayout: boolean,
  closestOffsetParentPath: ElementPath | null,
  usesParentBounds: boolean,
  parentLayoutSystem: DetectedLayoutSystem,
  layoutSystemForChildren: DetectedLayoutSystem | null,
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
  textBounds: CanvasRectangle | null,
  computedHugProperty: HugPropertyWidthHeight,
  containerGridProperties: GridContainerProperties,
  elementGridProperties: GridElementProperties,
  containerGridPropertiesFromProps: GridContainerProperties,
  elementGridPropertiesFromProps: GridElementProperties,
  rowGap: number | null,
  columnGap: number | null,
): SpecialSizeMeasurements {
  return {
    offset,
    coordinateSystemBounds,
    immediateParentBounds,
    immediateParentProvidesLayout,
    globalFrameWithTextContent,
    textBounds,
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
    gap,
    flexDirection,
    justifyContent,
    alignItems,
    htmlElementName,
    renderedChildrenCount,
    globalContentBoxForChildren,
    float,
    hasPositionOffset,
    parentTextDirection,
    hasTransform,
    borderRadius,
    fontSize,
    fontWeight,
    fontStyle,
    textDecorationLine,
    computedHugProperty,
    containerGridProperties,
    elementGridProperties,
    containerGridPropertiesFromProps,
    elementGridPropertiesFromProps,
    rowGap,
    columnGap,
  }
}

export const emptySpecialSizeMeasurements = specialSizeMeasurements(
  {
    x: 0,
    y: 0,
  } as LocalPoint,
  null,
  zeroCanvasRect,
  null,
  true,
  EP.emptyElementPath,
  false,
  'flow',
  null,
  false,
  'initial',
  null,
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
  null,
  { width: null, height: null },
  {
    gridTemplateColumns: null,
    gridTemplateRows: null,
    gridAutoColumns: null,
    gridAutoRows: null,
    gridAutoFlow: null,
  },
  {
    gridColumnStart: null,
    gridColumnEnd: null,
    gridRowStart: null,
    gridRowEnd: null,
  },
  {
    gridTemplateColumns: null,
    gridTemplateRows: null,
    gridAutoColumns: null,
    gridAutoRows: null,
    gridAutoFlow: null,
  },
  {
    gridColumnStart: null,
    gridColumnEnd: null,
    gridRowStart: null,
    gridRowEnd: null,
  },
  null,
  null,
)

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
        element.props.forEach((prop) => {
          if (prop.type === 'JSX_ATTRIBUTES_ENTRY') {
            walkElement(prop.value, path, depth + 1, forEach)
          }
        })
      }
      break
    case 'JSX_FRAGMENT':
      forEach(element, parentPath, depth)
      fastForEach(element.children, (child) => walkElement(child, parentPath, depth + 1, forEach))
      break
    case 'JSX_TEXT_BLOCK':
      forEach(element, parentPath, depth)
      break
    case 'JSX_MAP_EXPRESSION':
      forEach(element, parentPath, depth)
      walkElement(element.valueToMap, parentPath, depth + 1, forEach)
      walkElement(element.mapFunction, parentPath, depth + 1, forEach)
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
    case 'JS_IDENTIFIER':
      forEach(element, parentPath, depth)
      break
    case 'JS_PROPERTY_ACCESS':
      forEach(element, parentPath, depth)
      walkElement(element.onValue, parentPath, depth + 1, forEach)
      break
    case 'JS_ELEMENT_ACCESS':
      forEach(element, parentPath, depth)
      walkElement(element.onValue, parentPath, depth + 1, forEach)
      walkElement(element.element, parentPath, depth + 1, forEach)
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

export function clearJSAssignmentUniqueIDs(assignment: JSAssignment): JSAssignment {
  return {
    ...assignment,
    leftHandSide: clearBoundParamUniqueIDs(assignment.leftHandSide),
    rightHandSide: clearExpressionUniqueIDs(assignment.rightHandSide),
  }
}

export function clearJSAssignmentSourceMaps(assignment: JSAssignment): JSAssignment {
  return {
    ...assignment,
    leftHandSide: clearBoundParamSourceMaps(assignment.leftHandSide),
    rightHandSide: clearExpressionSourceMaps(assignment.rightHandSide),
  }
}

export function clearJSArbitraryStatementUniqueIDs(
  statement: JSArbitraryStatement,
): JSArbitraryStatement {
  switch (statement.type) {
    case 'JS_ASSIGNMENT_STATEMENT':
      return {
        ...statement,
        assignments: statement.assignments.map(clearJSAssignmentUniqueIDs),
        uid: '',
      }
    case 'JS_OPAQUE_ARBITRARY_STATEMENT':
      return {
        ...statement,
        uid: '',
      }
    default:
      assertNever(statement)
  }
}

export function clearJSArbitraryStatementSourceMaps(
  statement: JSArbitraryStatement,
): JSArbitraryStatement {
  switch (statement.type) {
    case 'JS_ASSIGNMENT_STATEMENT':
      return {
        ...statement,
        assignments: statement.assignments.map(clearJSAssignmentSourceMaps),
      }
    case 'JS_OPAQUE_ARBITRARY_STATEMENT':
      return statement
    default:
      assertNever(statement)
  }
}
