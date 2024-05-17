import * as TS from 'typescript-for-the-editor'
import { SourceNode, SourceMapGenerator } from 'source-map'
import {
  addUniquely,
  dropLast,
  flatMapArray,
  mapDropNulls,
  stripNulls,
} from '../../shared/array-utils'
import { difference } from '../../shared/set-utils'
import type { Either } from '../../shared/either'
import {
  applicative2Either,
  applicative3Either,
  bimapEither,
  flatMapEither,
  foldEither,
  forEachRight,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
  traverseEither,
} from '../../shared/either'
import type {
  ArbitraryJSBlock,
  JSExpression,
  JSXArrayElement,
  JSXAttributes,
  JSXElementChild,
  JSXElementChildren,
  JSXElementName,
  JSXProperty,
  ElementsWithin,
  Comment,
  BlockOrExpression,
  ParsedComments,
  JSExpressionValue,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSExpressionFunctionCall,
  JSXTextBlock,
  JSXMapExpression,
  JSExpressionMapOrOtherJavascript,
  BoundParam,
  DestructuredArrayPart,
  DestructuredParamPart,
  Param,
  JSPropertyAccess,
  JSElementAccess,
  JSIdentifier,
  OptionallyChained,
  JSArbitraryStatement,
  JSOpaqueArbitraryStatement,
  JSAssignment,
  JSAssignmentStatement,
  JSExpressionOtherJavaScript,
} from '../../shared/element-template'
import {
  arbitraryJSBlock,
  isJSExpressionMapOrOtherJavaScript,
  isJSXElement,
  isJSXTextBlock,
  jsxArraySpread,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsExpressionOtherJavaScript,
  jsExpressionValue,
  jsxElement,
  jsxElementName,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  jsxTextBlock,
  jsxFragment,
  jsxElementNameEquals,
  isIntrinsicElement,
  clearAttributesUniqueIDs,
  clearAttributesSourceMaps,
  jsxAttributesEntry,
  setJSXAttributesAttribute,
  jsxAttributesSpread,
  isIntrinsicElementFromString,
  emptyComments,
  parsedComments,
  isJSXFragment,
  jsxConditionalExpression,
  isJSXConditionalExpression,
  clearExpressionSourceMaps,
  clearExpressionUniqueIDs,
  jsxMapExpression,
  destructuredArray,
  destructuredObject,
  destructuredParamPart,
  functionParam,
  isRegularParam,
  omittedParam,
  regularParam,
  jsElementAccess,
  jsPropertyAccess,
  jsIdentifier,
  clearExpressionUniqueIDsAndSourceMaps,
  jsOpaqueArbitraryStatement,
  jsAssignment,
  jsAssignmentStatement,
  clearAssignmentUniqueIDsAndSourceMaps,
} from '../../shared/element-template'
import { maybeToArray } from '../../shared/optional-utils'
import type {
  HighlightBounds,
  Imports,
  PropertyPath,
  PropertyPathPart,
  HighlightBoundsForUids,
} from '../../shared/project-file-types'
import {
  generateConsistentUID,
  getUtopiaIDFromJSXElement,
  parseUID,
  parseUIDFromComments,
} from '../../shared/uid-utils'
import { fastForEach, RETURN_TO_PREPEND } from '../../shared/utils'
import {
  transpileJavascript,
  insertDataUIDsIntoCode,
  wrapAndTranspileJavascript,
} from './parser-printer-transpiling'
import * as PP from '../../shared/property-path'
import type { ElementsWithinInPosition } from './parser-printer-utils'
import { prependToSourceString, getBoundsOfNodes } from './parser-printer-utils'
import { getComments, getLeadingComments, getTrailingComments } from './parser-printer-comments'
import {
  BLOCK_RAN_TO_END_FUNCTION_NAME,
  EARLY_RETURN_RESULT_FUNCTION_NAME,
  EARLY_RETURN_VOID_FUNCTION_NAME,
  JSX_CANVAS_LOOKUP_FUNCTION_NAME,
} from '../../shared/dom-utils'
import { isEmptyString } from '../../shared/string-utils'
import type { RawSourceMap } from '../ts/ts-typings/RawSourceMap'
import { emptySet } from '../../../core/shared/set-utils'
import { getAllUniqueUidsFromAttributes } from '../../../core/model/get-unique-ids'
import type { SteganographyMode } from './parser-printer'
import { hashObject } from '../../shared/hash'

export function parseParams(
  params: TS.NodeArray<TS.ParameterDeclaration>,
  file: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<Array<Param>>> {
  let parsedParams: Array<Param> = []
  let highlightBounds: HighlightBoundsForUids = { ...existingHighlightBounds }
  let propsUsed: Array<string> = []
  for (const param of params) {
    const parseResult = parseParam(
      param,
      file,
      sourceText,
      filename,
      imports,
      topLevelNames,
      highlightBounds,
      existingUIDs,
      applySteganography,
    )
    if (isRight(parseResult)) {
      const parsedParam = parseResult.value
      highlightBounds = {
        ...highlightBounds,
        ...parsedParam.highlightBounds,
      }
      propsUsed = [...propsUsed, ...parsedParam.propsUsed]
      parsedParams.push(parsedParam.value)
    } else {
      return parseResult
    }
  }
  return right(withParserMetadata(parsedParams, highlightBounds, propsUsed, []))
}

export function parseParam(
  param: TS.ParameterDeclaration | TS.BindingElement,
  file: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<Param>> {
  const dotDotDotToken = param.dotDotDotToken != null
  const parsedExpression: Either<
    string,
    WithParserMetadata<JSExpressionMapOrOtherJavascript | undefined>
  > = param.initializer == null
    ? right(withParserMetadata(undefined, existingHighlightBounds, [], []))
    : parseJSExpressionMapOrOtherJavascript(
        file,
        sourceText,
        filename,
        imports,
        topLevelNames,
        null,
        param.initializer,
        existingHighlightBounds,
        existingUIDs,
        applySteganography,
      )
  return flatMapEither((paramExpression) => {
    const parsedBindingName = parseBindingName(
      param.name,
      paramExpression,
      file,
      sourceText,
      filename,
      imports,
      topLevelNames,
      existingHighlightBounds,
      existingUIDs,
      applySteganography,
    )
    return mapEither(
      (bindingName) =>
        withParserMetadata(
          functionParam(dotDotDotToken, bindingName.value),
          bindingName.highlightBounds,
          bindingName.propsUsed,
          [],
        ),
      parsedBindingName,
    )
  }, parsedExpression)
}

function parseBindingName(
  elem: TS.BindingName,
  expression: WithParserMetadata<JSExpression | undefined>,
  file: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<BoundParam>> {
  let highlightBounds: HighlightBoundsForUids = {
    ...existingHighlightBounds,
    ...expression.highlightBounds,
  }
  let propsUsed: Array<string> = [...expression.propsUsed]

  if (TS.isIdentifier(elem)) {
    const parsedParamName = getPropertyNameText(elem, file)
    return mapEither(
      (paramName) =>
        withParserMetadata(
          regularParam(paramName, expression.value ?? null),
          highlightBounds,
          propsUsed,
          [],
        ),
      parsedParamName,
    )
  } else if (TS.isObjectBindingPattern(elem)) {
    let parts: Array<DestructuredParamPart> = []
    for (const element of elem.elements) {
      const parsedPropertyName: Either<string, string | null> =
        element.propertyName == null ? right(null) : getPropertyNameText(element.propertyName, file)
      if (isRight(parsedPropertyName)) {
        const propertyName = parsedPropertyName.value
        const parsedParam = parseParam(
          element,
          file,
          sourceText,
          filename,
          imports,
          topLevelNames,
          highlightBounds,
          existingUIDs,
          applySteganography,
        )
        if (isRight(parsedParam)) {
          const bound = parsedParam.value.value
          highlightBounds = {
            ...highlightBounds,
            ...parsedParam.value.highlightBounds,
          }
          propsUsed = [...propsUsed, ...parsedParam.value.propsUsed]
          if (propertyName == null) {
            if (isRegularParam(bound.boundParam)) {
              parts.push(destructuredParamPart(undefined, bound, null))
            } else {
              return left('Unable to parse bound object parameter with no parameter propertyName')
            }
          } else {
            parts.push(destructuredParamPart(propertyName, bound, null))
          }
        } else {
          return parsedParam
        }
      } else {
        return parsedPropertyName
      }
    }
    return right(withParserMetadata(destructuredObject(parts), highlightBounds, propsUsed, []))
  } else if (TS.isArrayBindingPattern(elem)) {
    let parts: Array<DestructuredArrayPart> = []
    for (const element of elem.elements) {
      if (TS.isOmittedExpression(element)) {
        parts.push(omittedParam())
      } else {
        const parsedParam = parseParam(
          element,
          file,
          sourceText,
          filename,
          imports,
          topLevelNames,
          highlightBounds,
          existingUIDs,
          applySteganography,
        )
        if (isRight(parsedParam)) {
          const bound = parsedParam.value.value
          highlightBounds = {
            ...highlightBounds,
            ...parsedParam.value.highlightBounds,
          }
          propsUsed = [...propsUsed, ...parsedParam.value.propsUsed]
          parts.push(bound)
        } else {
          return parsedParam
        }
      }
    }
    return right(withParserMetadata(destructuredArray(parts), highlightBounds, propsUsed, []))
  } else {
    return left('Unable to parse binding element')
  }
}

function inPositionToElementsWithin(elements: ElementsWithinInPosition): ElementsWithin {
  let result: ElementsWithin = {}
  fastForEach(elements, (element) => {
    result[element.uid] = element.element
  })
  return result
}

function nodeArrayToArray<T extends TS.Node>(nodeArray: TS.NodeArray<T>): Array<T> {
  return nodeArray.slice()
}

export function getPropertyNameText(
  propertyName: TS.PropertyName,
  file: TS.SourceFile,
): Either<string, string> {
  if (TS.isIdentifier(propertyName)) {
    return right(propertyName.getText(file))
  } else if (TS.isStringLiteral(propertyName)) {
    return right(propertyName.text)
  } else if (TS.isNumericLiteral(propertyName)) {
    return right(propertyName.text)
  } else {
    return left(`Cannot handle computed property names.`)
  }
}

export function markedWithKeyword(node: TS.Node, keyword: TS.SyntaxKind): boolean {
  if (node.modifiers == null) {
    return false
  } else {
    return node.modifiers.some((modifier) => modifier.kind === keyword)
  }
}

export function markedAsDefault(node: TS.Node): boolean {
  return markedWithKeyword(node, TS.SyntaxKind.DefaultKeyword)
}

export function markedAsExported(node: TS.Node): boolean {
  return markedWithKeyword(node, TS.SyntaxKind.ExportKeyword)
}

export function isExported(node: TS.Node): boolean {
  if (TS.isExportAssignment(node)) {
    return true
  } else {
    return markedAsExported(node)
  }
}

function parseArrayLiteralExpression(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  literal: TS.ArrayLiteralExpression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSExpression>> {
  let arrayContents: Array<JSXArrayElement> = []
  let highlightBounds = existingHighlightBounds
  let propsUsed: Array<string> = []
  let definedElsewhere: Array<string> = []

  // Get comments attached to the square bracket.
  let openSquareBracketComments: ParsedComments = emptyComments
  const firstChild = literal.getChildAt(0, sourceFile)
  if (firstChild != null && firstChild.kind === TS.SyntaxKind.OpenBracketToken) {
    openSquareBracketComments = getComments(sourceText, firstChild)
  }
  let firstProp: boolean = true

  for (const literalElement of literal.elements) {
    // Capture the comments around the entire entry.
    let elementComments = getComments(sourceText, literalElement)
    if (firstProp) {
      elementComments = parsedComments(
        [...openSquareBracketComments.trailingComments, ...elementComments.leadingComments],
        elementComments.trailingComments,
      )
    }

    if (TS.isSpreadElement(literalElement)) {
      const subExpression = parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        literalElement.expression,
        existingHighlightBounds,
        alreadyExistingUIDs,
        [],
        applySteganography,
        'part-of-expression',
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        highlightBounds = mergeHighlightBounds(highlightBounds, subExpression.value.highlightBounds)
        propsUsed.push(...subExpression.value.propsUsed)
        definedElsewhere.push(...subExpression.value.definedElsewhere)
        arrayContents.push(jsxArraySpread(subExpression.value.value, elementComments))
      }
    } else {
      const subExpression = parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        literalElement,
        highlightBounds,
        alreadyExistingUIDs,
        [],
        applySteganography,
        'part-of-expression',
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        highlightBounds = mergeHighlightBounds(highlightBounds, subExpression.value.highlightBounds)
        propsUsed.push(...subExpression.value.propsUsed)
        definedElsewhere.push(...subExpression.value.definedElsewhere)
        const subExpressionValue: JSExpression = subExpression.value.value
        arrayContents.push(jsxArrayValue(subExpressionValue, elementComments))
      }
    }
  }

  const expressionResult = addBoundsIntoWithParser(
    highlightBounds,
    createExpressionNestedArray(
      sourceFile,
      literal,
      arrayContents,
      emptyComments,
      alreadyExistingUIDs,
      propsUsed,
      definedElsewhere,
    ),
  )
  return right(expressionResult)
}

function parseObjectLiteralExpression(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  literal: TS.ObjectLiteralExpression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSExpression>> {
  let contents: Array<JSXProperty> = []
  let highlightBounds = existingHighlightBounds
  let propsUsed: Array<string> = []
  let definedElsewhere: Array<string> = []

  // Get comments attached to the open brace.
  let openBraceComments: ParsedComments = emptyComments
  const firstChild = literal.getChildAt(0, sourceFile)
  if (firstChild != null && firstChild.kind === TS.SyntaxKind.OpenBraceToken) {
    openBraceComments = getComments(sourceText, firstChild)
  }
  let firstProp: boolean = true

  for (const literalProp of literal.properties) {
    // Capture the comments around the entire entry.
    let propComments = getComments(sourceText, literalProp)
    if (firstProp) {
      propComments = parsedComments(
        [...openBraceComments.trailingComments, ...propComments.leadingComments],
        propComments.trailingComments,
      )
    }

    if (TS.isPropertyAssignment(literalProp) || TS.isShorthandPropertyAssignment(literalProp)) {
      // The colon in the middle might have comments attached to it, we should try to grab those.
      const colonToken = literalProp
        .getChildren(sourceFile)
        .find((node) => node.kind === TS.SyntaxKind.ColonToken)
      const colonTokenComments =
        colonToken == null ? emptyComments : getComments(sourceText, colonToken)

      const initializer = TS.isPropertyAssignment(literalProp)
        ? literalProp.initializer
        : literalProp.name
      const subExpression = parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        initializer,
        highlightBounds,
        alreadyExistingUIDs,
        colonTokenComments.trailingComments,
        applySteganography,
        'part-of-expression',
      )

      if (isLeft(subExpression)) {
        return subExpression
      } else {
        const possibleKey = getPropertyNameText(literalProp.name, sourceFile)
        if (isLeft(possibleKey)) {
          return possibleKey
        } else {
          const key = possibleKey.value
          const subExpressionValue: JSExpression = subExpression.value.value
          const keyComments = getComments(sourceText, literalProp.name)
          contents.push(jsxPropertyAssignment(key, subExpressionValue, propComments, keyComments))
          highlightBounds = mergeHighlightBounds(
            highlightBounds,
            subExpression.value.highlightBounds,
          )
          propsUsed.push(...subExpression.value.propsUsed)
          definedElsewhere.push(...subExpression.value.definedElsewhere)
        }
      }
    } else if (TS.isSpreadAssignment(literalProp)) {
      const subExpression = parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        literalProp.expression,
        highlightBounds,
        alreadyExistingUIDs,
        [],
        applySteganography,
        'part-of-expression',
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        const subExpressionValue = subExpression.value.value
        contents.push(jsxSpreadAssignment(subExpressionValue, propComments))
        highlightBounds = mergeHighlightBounds(highlightBounds, subExpression.value.highlightBounds)
        propsUsed.push(...subExpression.value.propsUsed)
        definedElsewhere.push(...subExpression.value.definedElsewhere)
      }
    }

    // First prop reset after everything has been handled.
    firstProp = false
  }

  const expressionResult = addBoundsIntoWithParser(
    highlightBounds,
    createExpressionNestedObject(
      sourceFile,
      literal,
      contents,
      emptyComments,
      alreadyExistingUIDs,
      propsUsed,
      definedElsewhere,
    ),
  )
  return right(expressionResult)
}

interface PropertyAccessDescriptor {
  identifier: string
  property: PropertyPath
}

export function parsePropertyPathLikeExpression(
  sourceFile: TS.SourceFile,
  expression: TS.Expression,
): Either<string, PropertyAccessDescriptor> {
  function innerParse(
    inner: TS.Expression,
    pathSoFar: Array<PropertyPathPart>,
  ): Either<string, PropertyAccessDescriptor> {
    if (TS.isPropertyAccessExpression(inner)) {
      const prop = inner.name.getText(sourceFile)
      return innerParse(inner.expression, [prop, ...pathSoFar])
    } else if (TS.isElementAccessExpression(inner)) {
      if (TS.isNumericLiteral(inner.argumentExpression)) {
        try {
          const int = Number.parseInt(inner.argumentExpression.getText(sourceFile))
          return innerParse(inner.expression, [int, ...pathSoFar])
        } catch {
          return left(`Index argument wasn't an integer.`)
        }
      } else if (TS.isStringLiteral(inner.argumentExpression)) {
        const prop = inner.argumentExpression.getText(sourceFile)
        return innerParse(inner.expression, [prop, ...pathSoFar])
      } else {
        return left(`Unhandled element access expression.`)
      }
    } else if (TS.isIdentifier(inner)) {
      return right({
        identifier: inner.getText(sourceFile),
        property: PP.createFromArray(pathSoFar),
      })
    } else {
      return left(`Unhandled expression type.`)
    }
  }
  return innerParse(expression, [])
}

function turnCodeSnippetIntoSourceMapNodes(
  fileName: string,
  startLine: number,
  startChar: number,
  sourceCode: string,
  nodeIsExported: boolean,
): typeof SourceNode {
  const LetterOrNumber = /[a-zA-Z0-9]/
  const NewLine = /\n/
  const FunctionStart = /^ *\(/
  let nodes: Array<typeof SourceNode> = []
  let currentLine = startLine
  let currentCol = startChar
  let bufferStartLine = currentLine
  let bufferStartChar = currentCol
  let currentStringBuffer = ''
  let exportFound = false
  let defaultFound = false
  let lastKeywordWasExport = false
  let i = 0
  function flushBuffer() {
    // We should ignore the first "export" and "default" keywords we find if the node is exported
    const strippingExport = nodeIsExported && !exportFound && currentStringBuffer === 'export'
    const strippingDefault =
      lastKeywordWasExport && nodeIsExported && !defaultFound && currentStringBuffer === 'default'
    lastKeywordWasExport =
      strippingExport || (lastKeywordWasExport && currentStringBuffer.trim().length === 0)
    if (strippingExport) {
      exportFound = true
    } else if (strippingDefault) {
      defaultFound = true
    } else {
      // When there's an `export default function()` we strip off the `export default`, but without
      // that it becomes an invalid chunk of JavaScript without a name so add in a default one.
      if (
        exportFound &&
        defaultFound &&
        currentStringBuffer === 'function' &&
        FunctionStart.test(sourceCode.substr(i))
      ) {
        currentStringBuffer = 'function utopia_defaultFunctionName'
      }
      const node = new SourceNode(
        bufferStartLine + 1,
        bufferStartChar + 1,
        fileName,
        currentStringBuffer,
        currentStringBuffer,
      )
      nodes.push(node)
    }
    currentStringBuffer = ''
    bufferStartLine = currentLine
    bufferStartChar = currentCol
  }
  function newLine() {
    currentLine += 1
    currentCol = 0
  }
  for (; i < sourceCode.length; i++) {
    const currentChar = sourceCode[i]
    // if the current char is not a letter or number, make a cut and push the buffer to a source node
    if (
      LetterOrNumber.exec(currentChar) == null ||
      (currentStringBuffer.length === 1 && LetterOrNumber.exec(currentStringBuffer[0]) == null)
    ) {
      flushBuffer()
    }
    currentCol += 1
    if (NewLine.exec(currentChar) != null) {
      newLine()
    }
    currentStringBuffer = currentStringBuffer.concat(currentChar)
  }
  flushBuffer()
  return new SourceNode(startLine + 1, startChar + 1, fileName, [nodes])
}

interface ExpressionAndText<E extends TS.Node> {
  expression: E | undefined
  text: string
  startPos: number
  endPos: number
}

function createExpressionAndText<E extends TS.Node>(
  expression: E | undefined,
  text: string,
  startPos: number,
  endPos: number,
): ExpressionAndText<E> {
  return {
    expression: expression,
    text: text,
    startPos: startPos,
    endPos: endPos,
  }
}

function addBindingNames(
  sourceFile: TS.SourceFile,
  name: TS.BindingName,
  toAddTo: Array<string>,
): void {
  if (TS.isIdentifier(name)) {
    toAddTo.push(name.getText(sourceFile))
  } else if (TS.isObjectBindingPattern(name)) {
    for (const element of name.elements) {
      addBindingNames(sourceFile, element.name, toAddTo)
    }
  } else if (TS.isArrayBindingPattern(name)) {
    for (const element of name.elements) {
      if (TS.isBindingElement(element)) {
        addBindingNames(sourceFile, element.name, toAddTo)
      }
    }
  }
}

function failOnNullResult<T>(result: Either<string, T | null>): Either<string, T> {
  return flatMapEither((r) => {
    if (r == null) {
      return left('Unexpected null result.')
    } else {
      return right(r)
    }
  }, result)
}

function createOpaqueArbitraryStatement(
  sourceFile: TS.SourceFile,
  alreadyExistingUIDs: Set<string>,
  expression: TS.Node,
  originalJavaScript: string,
  expressionDefinedWithin: Array<string>,
  expressionDefinedElsewhere: Array<string>,
): JSOpaqueArbitraryStatement {
  const withoutUID = jsOpaqueArbitraryStatement(
    originalJavaScript,
    expressionDefinedWithin,
    expressionDefinedElsewhere,
    '',
  )
  const uid = generateUIDAndAddToExistingUIDs(
    sourceFile,
    withoutUID,
    alreadyExistingUIDs,
    expression,
  )
  return jsOpaqueArbitraryStatement(
    originalJavaScript,
    expressionDefinedWithin,
    expressionDefinedElsewhere,
    uid,
  )
}

function createAssignmentStatement(
  sourceFile: TS.SourceFile,
  alreadyExistingUIDs: Set<string>,
  declarationKeyword: 'let' | 'const' | 'var',
  assignments: Array<JSAssignment>,
  nodeOrNodes: TS.Node | Array<TS.Node>,
): JSAssignmentStatement {
  const withoutUID = jsAssignmentStatement(
    declarationKeyword,
    assignments.map(clearAssignmentUniqueIDsAndSourceMaps),
    '',
  )
  const uid = generateUIDAndAddToExistingUIDs(
    sourceFile,
    withoutUID,
    alreadyExistingUIDs,
    nodeOrNodes,
  )
  return jsAssignmentStatement(declarationKeyword, assignments, uid)
}

function parseDeclaration(
  sourceFile: TS.SourceFile,
  sourceText: string,
  imports: Imports,
  topLevelNames: Array<string>,
  initialPropsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
  tsDeclaration: TS.VariableDeclaration,
): Either<string, WithParserMetadata<JSAssignment>> {
  if (tsDeclaration.initializer == null) {
    return left('Cannot handle a declaration without an initializer.')
  }
  const comments = getComments(sourceText, tsDeclaration)
  const possibleExpression = parseAttributeExpression(
    sourceFile,
    sourceText,
    sourceFile.fileName,
    imports,
    topLevelNames,
    initialPropsObjectName,
    tsDeclaration.initializer,
    existingHighlightBounds,
    alreadyExistingUIDs,
    [],
    applySteganography,
    'part-of-expression',
  )
  const lhs = flatMapEither((valueExpression) => {
    return parseBindingName(
      tsDeclaration.name,
      valueExpression,
      sourceFile,
      sourceText,
      sourceFile.fileName,
      imports,
      topLevelNames,
      existingHighlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
  }, possibleExpression)

  return applicative2Either(
    (lhsValueWithMetadata, expressionWithMetadata) => {
      return merge2WithParserMetadata(
        lhsValueWithMetadata,
        expressionWithMetadata,
        (lhsValue, expression) => {
          return withParserMetadata(jsAssignment(lhsValue, expression), {}, [], [])
        },
      )
    },
    lhs,
    possibleExpression,
  )
}

function getDeclarationKind(variableStatement: TS.VariableStatement): 'let' | 'const' | 'var' {
  if ((variableStatement.declarationList.flags & TS.NodeFlags.Const) !== 0) {
    return 'const'
  } else if ((variableStatement.declarationList.flags & TS.NodeFlags.Let) !== 0) {
    return 'let'
  } else {
    return 'var'
  }
}

function parseJSArbitraryStatement(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  expressionAndText: ExpressionAndText<TS.Node>,
  imports: Imports,
  topLevelNames: Array<string>,
  initialPropsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  trailingCode: string,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSArbitraryStatement> | null> {
  const parsedExpression = parseOtherJavaScript<JSArbitraryStatement>(
    sourceFile,
    sourceText,
    filename,
    [expressionAndText],
    imports,
    topLevelNames,
    initialPropsObjectName,
    existingHighlightBounds,
    alreadyExistingUIDs,
    trailingCode,
    applySteganography,
    'do-not-parse-statements',
    (_code, expressionDefinedWithin, expressionDefinedElsewhere) => {
      if (expressionAndText.expression == null) {
        return right(null)
      } else if (expressionAndText.expression != null) {
        if (TS.isVariableStatement(expressionAndText.expression)) {
          const variableStatement = expressionAndText.expression
          const possibleDeclarations = traverseEither((tsDeclaration) => {
            return parseDeclaration(
              sourceFile,
              sourceText,
              imports,
              topLevelNames,
              initialPropsObjectName,
              existingHighlightBounds,
              alreadyExistingUIDs,
              applySteganography,
              tsDeclaration,
            )
          }, nodeArrayToArray(variableStatement.declarationList.declarations))
          const possibleAssignmentStatement = mapEither((declarationsAndMetadata) => {
            return createAssignmentStatement(
              sourceFile,
              alreadyExistingUIDs,
              getDeclarationKind(variableStatement),
              declarationsAndMetadata.map((d) => d.value),
              variableStatement,
            )
          }, possibleDeclarations)

          if (isRight(possibleAssignmentStatement)) {
            return possibleAssignmentStatement
          }
        }
      }
      return right(
        createOpaqueArbitraryStatement(
          sourceFile,
          alreadyExistingUIDs,
          expressionAndText.expression,
          expressionAndText.text,
          expressionDefinedWithin,
          expressionDefinedElsewhere,
        ),
      )
    },
  )
  return parsedExpression
}

function parseOtherJavaScript<T extends { uid: string }>(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  expressionsAndTexts: Array<ExpressionAndText<TS.Node>>,
  imports: Imports,
  topLevelNames: Array<string>,
  initialPropsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  trailingCode: string,
  applySteganography: SteganographyMode,
  parseStatements: 'parse-statements' | 'do-not-parse-statements',
  create: (
    code: string,
    definedWithin: Array<string>,
    definedElsewhere: Array<string>,
    fileNode: typeof SourceNode,
    parsedElementsWithin: ElementsWithinInPosition,
    params: Array<Param>,
    statements: Array<JSArbitraryStatement>,
    nodes: Array<TS.Node>,
  ) => Either<string, T | null>,
): Either<string, WithParserMetadata<T> | null> {
  if (expressionsAndTexts.length === 0) {
    throw new Error('Unable to deal with a collection of zero expressions.')
  } else {
    let startLineShift: number = 0
    let startColumnShift: number = 0
    let lastBlockEndLine: number = 1

    let propsObjectName = initialPropsObjectName // nullified if re-defined within
    let definedWithin: Array<string> = []
    let definedElsewhere: Array<string> = []
    let propsUsed: Array<string> = []

    function pushToDefinedWithinIfNotThere(name: string): void {
      if (!definedWithin.includes(name)) {
        definedWithin.push(name)

        if (propsObjectName === name) {
          propsObjectName = null
        }
      }
    }

    function addBindingNameToDefinedWithin(name: TS.BindingName): void {
      addBindingNames(sourceFile, name, definedWithin)
    }

    function pushToDefinedElsewhereIfNotThere(inScope: Array<string>, name: string): void {
      if (!inScope.includes(name) && !definedElsewhere.includes(name)) {
        definedElsewhere.push(name)
      }
    }

    function addIfDefinedElsewhere(
      inScope: Array<string>,
      nodeToCheck: TS.Node,
      nodeIsJSXElement: boolean,
    ): boolean {
      if (TS.isIdentifier(nodeToCheck)) {
        const nameToAdd = nodeToCheck.getText(sourceFile)
        if (nodeIsJSXElement) {
          if (!isIntrinsicElementFromString(nameToAdd)) {
            pushToDefinedElsewhereIfNotThere(inScope, nameToAdd)
            return true
          }
        } else {
          pushToDefinedElsewhereIfNotThere(inScope, nameToAdd)
          return true
        }
      }
      if (
        TS.isObjectLiteralElement(nodeToCheck) &&
        nodeToCheck.name != null &&
        TS.isIdentifier(nodeToCheck.name)
      ) {
        pushToDefinedElsewhereIfNotThere(inScope, nodeToCheck.name.getText(sourceFile))
        return true
      }

      return false
    }

    function pushToPropsUsedIfNotThere(name: string): void {
      if (!propsUsed.includes(name)) {
        propsUsed.push(name)
      }
    }

    function isPropsObject(nodeToCheck: TS.Identifier): boolean {
      return propsObjectName != null && nodeToCheck.getText(sourceFile) === propsObjectName
    }

    function addIfPropsUsed(nodeToCheck: TS.LeftHandSideExpression): void {
      if (TS.isPropertyAccessExpression(nodeToCheck)) {
        if (TS.isIdentifier(nodeToCheck.expression)) {
          if (isPropsObject(nodeToCheck.expression)) {
            const nameToAdd = nodeToCheck.name.getText(sourceFile)
            if (nameToAdd !== 'children') {
              // Exclude children
              pushToPropsUsedIfNotThere(nameToAdd)
            }
          }
        } else {
          addIfPropsUsed(nodeToCheck.expression)
        }
      } else if (TS.isCallExpression(nodeToCheck) || TS.isElementAccessExpression(nodeToCheck)) {
        addIfPropsUsed(nodeToCheck.expression)
      }
    }

    let parsedElementsWithin: ElementsWithinInPosition = []
    let highlightBounds = existingHighlightBounds

    function addToParsedElementsWithin(
      currentScope: Array<string>,
      node: TS.JsxElement | TS.JsxSelfClosingElement,
    ): void {
      const parseResult = parseOutJSXElements(
        sourceFile,
        sourceText,
        filename,
        [node],
        imports,
        topLevelNames,
        propsObjectName,
        highlightBounds,
        alreadyExistingUIDs,
        applySteganography,
      )
      forEachRight(parseResult, (success) => {
        // Be conservative with this for the moment.
        if (success.value.length === 1) {
          const firstChild = success.value[0]
          if (isJSXElement(firstChild.value)) {
            const uid = getUtopiaIDFromJSXElement(firstChild.value)
            parsedElementsWithin.push({
              uid: uid,
              element: firstChild.value,
              startLine: firstChild.startLine - startLineShift,
              startColumn:
                firstChild.startLine - 1 === startLineShift
                  ? firstChild.startColumn - startColumnShift
                  : firstChild.startColumn,
            })
            highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)

            fastForEach(success.definedElsewhere, (val) =>
              pushToDefinedElsewhereIfNotThere(currentScope, val),
            )
          }
        }
      })
    }

    function addToInScope(currentScope: Array<string>, node: TS.Node): Array<string> {
      function addBindingElement(
        currentScopeInner: Array<string>,
        bindingElement: TS.BindingElement,
      ): Array<string> {
        return addUniquely(currentScopeInner, bindingElement.name.getText(sourceFile))
      }

      function addBindingName(
        currentScopeInner: Array<string>,
        bindingName: TS.BindingName,
      ): Array<string> {
        if (TS.isIdentifier(bindingName)) {
          return addUniquely(currentScopeInner, bindingName.getText(sourceFile))
        } else if (TS.isObjectBindingPattern(bindingName)) {
          return bindingName.elements.reduce((working, element) => {
            return addBindingName(working, element.name)
          }, currentScopeInner)
        } else {
          // ArrayBindingPattern.
          return bindingName.elements.reduce((working, element) => {
            if (TS.isBindingElement(element)) {
              return addBindingElement(working, element)
            } else {
              return working
            }
          }, currentScopeInner)
        }
      }

      function addVariableStatement(
        currentScopeInner: Array<string>,
        statement: TS.VariableStatement,
      ): Array<string> {
        return statement.declarationList.declarations.reduce((working, declaration) => {
          return addBindingName(working, declaration.name)
        }, currentScopeInner)
      }

      function addStatement(
        currentScopeInner: Array<string>,
        statement: TS.Statement,
      ): Array<string> {
        if (TS.isFunctionLike(statement) && statement.name != null) {
          return addUniquely(currentScopeInner, statement.name.getText(sourceFile))
        } else if (TS.isVariableStatement(statement)) {
          return addVariableStatement(currentScopeInner, statement)
        } else {
          return currentScopeInner
        }
      }

      if (TS.isBlock(node)) {
        return node.statements.reduce((working, statement) => {
          return addStatement(working, statement)
        }, currentScope)
      } else if (TS.isFunctionLike(node)) {
        return node.parameters.reduce(
          (working, parameter) => addBindingName(working, parameter.name),
          currentScope,
        )
      } else if (TS.isClassDeclaration(node)) {
        return node.members.reduce((working, member) => {
          if (member.name == null) {
            return working
          } else {
            return addUniquely(working, member.name.getText(sourceFile))
          }
        }, currentScope)
      } else if (TS.isCaseClause(node)) {
        return node.statements.reduce((working, statement) => {
          return addStatement(working, statement)
        }, currentScope)
      } else if (
        TS.isForStatement(node) ||
        TS.isForInStatement(node) ||
        TS.isForOfStatement(node)
      ) {
        if (node.initializer == null) {
          return currentScope
        } else {
          if (TS.isVariableDeclarationList(node.initializer)) {
            return node.initializer.declarations.reduce((working, declaration) => {
              return addBindingName(working, declaration.name)
            }, currentScope)
          } else {
            return currentScope
          }
        }
      } else {
        return currentScope
      }
    }

    const transformer = (outermostScope: Array<string>) => {
      return (context: TS.TransformationContext) => {
        const walkTree = (insideJSXElement: boolean, scope: Array<string>) => {
          return (node: TS.Node) => {
            let innerInsideJSXElement = insideJSXElement
            if (TS.isArrayLiteralExpression(node)) {
              fastForEach(node.elements, (e) => addIfDefinedElsewhere(scope, e, false))
            } else if (TS.isAsExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isAssertionExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isAwaitExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isBinaryExpression(node)) {
              addIfDefinedElsewhere(scope, node.left, false)
              addIfDefinedElsewhere(scope, node.right, false)
            } else if (TS.isCallExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
              fastForEach(node.arguments, (a) => addIfDefinedElsewhere(scope, a, false))
            } else if (TS.isConditionalExpression(node)) {
              addIfDefinedElsewhere(scope, node.condition, false)
              addIfDefinedElsewhere(scope, node.whenTrue, false)
              addIfDefinedElsewhere(scope, node.whenFalse, false)
            } else if (TS.isIfStatement(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
              addIfDefinedElsewhere(scope, node.thenStatement, false)
              if (node.elseStatement != null) {
                addIfDefinedElsewhere(scope, node.elseStatement, false)
              }
            } else if (TS.isDecorator(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isDeleteExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isElementAccessExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
              addIfDefinedElsewhere(scope, node.argumentExpression, false)
            } else if (TS.isExpressionStatement(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isJsxExpression(node)) {
              // If there's some JSX, then it's implied we will transform this
              // into code that will need React.
              pushToDefinedElsewhereIfNotThere(scope, 'React')
              if (node.expression != null) {
                addIfDefinedElsewhere(scope, node.expression, false)
              }
            } else if (TS.isJsxElement(node)) {
              // Since this function will also walk inside this element,
              // prevent this from being reapplied within this element.
              if (!innerInsideJSXElement) {
                addToParsedElementsWithin(scope, node)
                innerInsideJSXElement = true
              }
              // If there's some JSX, then it's implied we will transform this
              // into code that will need React.
              pushToDefinedElsewhereIfNotThere(scope, 'React')
              addIfDefinedElsewhere(scope, node.openingElement.tagName, true)
            } else if (TS.isJsxSelfClosingElement(node)) {
              // Since this function will also walk inside this element,
              // prevent this from being reapplied within this element.
              if (!innerInsideJSXElement) {
                addToParsedElementsWithin(scope, node)
                innerInsideJSXElement = true
              }
              // If there's some JSX, then it's implied we will transform this
              // into code that will need React.
              pushToDefinedElsewhereIfNotThere(scope, 'React')
              addIfDefinedElsewhere(scope, node.tagName, true)
            } else if (TS.isJsxOpeningElement(node)) {
              // If there's some JSX, then it's implied we will transform this
              // into code that will need React.
              pushToDefinedElsewhereIfNotThere(scope, 'React')
              addIfDefinedElsewhere(scope, node.tagName, true)
            } else if (TS.isNewExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
              if (node.arguments != null) {
                fastForEach(node.arguments, (a) => addIfDefinedElsewhere(scope, a, false))
              }
            } else if (TS.isNonNullExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isObjectLiteralExpression(node)) {
              fastForEach(node.properties, (p) => {
                if (TS.isPropertyAssignment(p)) {
                  addIfDefinedElsewhere(scope, p.initializer, false)
                } else if (TS.isShorthandPropertyAssignment(p)) {
                  addIfDefinedElsewhere(scope, p, false)
                }
              })
            } else if (TS.isParenthesizedExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isPostfixUnaryExpression(node)) {
              addIfDefinedElsewhere(scope, node.operand, false)
            } else if (TS.isPrefixUnaryExpression(node)) {
              addIfDefinedElsewhere(scope, node.operand, false)
            } else if (TS.isPropertyAccessExpression(node)) {
              // This is the one we want
              const expressionDefinedElsewhere = addIfDefinedElsewhere(
                scope,
                node.expression,
                false,
              )
              if (expressionDefinedElsewhere) {
                addIfPropsUsed(node)
              }
            } else if (
              TS.isSpreadAssignment(node) ||
              TS.isSpreadElement(node) ||
              TS.isJsxSpreadAttribute(node)
            ) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isTaggedTemplateExpression(node)) {
              addIfDefinedElsewhere(scope, node, false)
            } else if (TS.isTemplateExpression(node)) {
              fastForEach(node.templateSpans, (s) =>
                addIfDefinedElsewhere(scope, s.expression, false),
              )
            } else if (TS.isTypeOfExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isVariableDeclaration(node) && node.initializer != null) {
              addIfDefinedElsewhere(scope, node.initializer, false)
            } else if (TS.isVoidExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isYieldExpression(node)) {
              if (node.expression != null) {
                addIfDefinedElsewhere(scope, node.expression, false)
              }
            } else if (TS.isArrowFunction(node)) {
              addIfDefinedElsewhere(scope, node.body, false)
            } else if (TS.isReturnStatement(node) && node.expression != null) {
              addIfDefinedElsewhere(scope, node.expression, false)
            }
            const newScope = addToInScope(scope, node)
            TS.visitEachChild(node, walkTree(innerInsideJSXElement, newScope), context)

            return node
          }
        }
        return (n: TS.Node) => TS.visitNode(n, walkTree(false, outermostScope))
      }
    }

    let expressionsText: Array<string> = []
    let expressionsNodes: Array<typeof SourceNode> = []
    let params: Either<string, WithParserMetadata<Array<Param>> | null> = right(null)
    for (const expressionAndText of expressionsAndTexts) {
      // Update the code offsets used when locating elements within
      const startPosition = TS.getLineAndCharacterOfPosition(sourceFile, expressionAndText.startPos)
      const endPosition = TS.getLineAndCharacterOfPosition(sourceFile, expressionAndText.endPos)
      const shiftedBlockStartLine = startPosition.line - lastBlockEndLine
      const column = startPosition.character - 1
      if (shiftedBlockStartLine > 0) {
        startLineShift += shiftedBlockStartLine
      }
      startColumnShift = column
      lastBlockEndLine = endPosition.line

      const expression = expressionAndText.expression
      if (expression != null) {
        // Handle the parameters, as this is a single expression and if it's a function.
        if (expressionsAndTexts.length === 1) {
          if (TS.isArrowFunction(expression) || TS.isFunctionExpression(expression)) {
            params = parseParams(
              expression.parameters,
              sourceFile,
              sourceText,
              filename,
              imports,
              topLevelNames,
              existingHighlightBounds,
              alreadyExistingUIDs,
              applySteganography,
            )
          }
        }
        addIfDefinedElsewhere([], expression, false)
        const expressionText = expressionAndText.text
        if (expressionText.length > 0) {
          const { line, character } = TS.getLineAndCharacterOfPosition(
            sourceFile,
            expressionAndText.startPos,
          )
          const expressionNode = turnCodeSnippetIntoSourceMapNodes(
            sourceFile.fileName,
            line,
            character,
            expressionAndText.text,
            isExported(expression),
          )
          expressionsNodes.push(expressionNode)
        }
        expressionsText.push(expressionText)
        if (TS.isFunctionLike(expression)) {
          if (expression.name != null) {
            pushToDefinedWithinIfNotThere(expression.name.getText(sourceFile))
          }
        } else if (TS.isVariableStatement(expression)) {
          for (const declaration of expression.declarationList.declarations) {
            addBindingNameToDefinedWithin(declaration.name)
          }
        } else if (TS.isClassDeclaration(expression)) {
          if (expression.name != null) {
            pushToDefinedWithinIfNotThere(expression.name.getText(sourceFile))
          }
        }
        TS.transform(expression, [transformer(definedWithin)])
      }
    }
    expressionsText.push(trailingCode)

    let statements: Array<JSArbitraryStatement> = []
    if (parseStatements === 'parse-statements') {
      for (const expressionAndText of expressionsAndTexts) {
        if (expressionAndText.expression != null) {
          const parsedStatement = parseJSArbitraryStatement(
            sourceFile,
            sourceText,
            filename,
            expressionAndText,
            imports,
            topLevelNames,
            initialPropsObjectName,
            existingHighlightBounds,
            // This is here so that the parsed statements _can_ re-use the UIDs of the expressions
            // that are being parsed in the main arbitrary blocks, otherwise they cause even elements
            // with defined or overriden UIDs to be regenerated in those arbitrary blocks.
            new Set(),
            '',
            applySteganography,
          )
          forEachRight(parsedStatement, (parsedStatementSuccess) => {
            if (parsedStatementSuccess != null) {
              statements.push(parsedStatementSuccess.value)
              // TODO: Handle the additional values?
            }
          })
        }
      }
    }

    let nodes: Array<TS.Node> = []
    for (const { expression } of expressionsAndTexts) {
      if (expression != null) {
        nodes.push(expression)
      }
    }

    // Helpfully it appears that in JSX elements the start and end are
    // offset by 1, meaning that if we use them to get the text
    // the string is total nonsense.
    const code = expressionsText.join('')

    const fileNode = new SourceNode(null, null, sourceFile.fileName, expressionsNodes)
    fileNode.setSourceContent(sourceFile.fileName, sourceFile.text)

    // Filter definedElsewhere to exclude anything in definedWithin as for example
    // one arbitrary block may use something from another arbitrary block in the
    // same chunk of arbitrary nodes.
    definedElsewhere = definedElsewhere.filter((e) => {
      return !definedWithin.includes(e)
    })

    // Filter propsUsed to exclude anything in definedWithin as for example
    // one arbitrary block may use something from another arbitrary block in the
    // same chunk of arbitrary nodes.
    propsUsed = propsUsed.filter((e) => {
      return !definedWithin.includes(e)
    })

    const paramsToUse = foldEither(
      () => {
        return []
      },
      (paramsSuccess) => {
        if (paramsSuccess == null) {
          return []
        } else {
          return paramsSuccess.value
        }
      },
      params,
    )

    return mapEither((created) => {
      // Add in the bounds for the entire value.
      highlightBounds = addToHighlightBounds(
        highlightBounds,
        buildHighlightBoundsForExpressionsAndText(sourceFile, expressionsAndTexts, created.uid),
      )
      return withParserMetadata(created, highlightBounds, propsUsed, definedElsewhere)
    }, failOnNullResult(create(code, definedWithin, definedElsewhere, fileNode, parsedElementsWithin, paramsToUse, statements, nodes)))
  }
}

function getCommentsOnExpression(sourceText: string, expression: TS.Node): ParsedComments {
  if (TS.isJsxExpression(expression)) {
    return expression.expression == null
      ? emptyComments
      : getComments(sourceText, expression.expression)
  } else {
    return getComments(sourceText, expression)
  }
}

interface MapExpressionParts {
  valueToMap: JSExpression
  mapFunction: JSExpression
  valuesInScopeFromParameters: JSXMapExpression['valuesInScopeFromParameters']
}

function parseOutMapExpressionParts(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  expression: TS.Node,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<MapExpressionParts>> {
  const jsxExpressionStripped =
    TS.isJsxExpression(expression) && expression.expression != null
      ? expression.expression
      : expression
  if (
    TS.isCallExpression(jsxExpressionStripped) &&
    TS.isPropertyAccessExpression(jsxExpressionStripped.expression)
  ) {
    const propertyAccessExpression: TS.PropertyAccessExpression = jsxExpressionStripped.expression
    if (propertyAccessExpression.name.getText(sourceFile) === 'map') {
      const firstArgument = jsxExpressionStripped.arguments[0]
      if (TS.isArrowFunction(firstArgument)) {
        let valuesInScopeFromParameters: JSXMapExpression['valuesInScopeFromParameters'] = []
        for (const parameter of firstArgument.parameters) {
          addBindingNames(sourceFile, parameter.name, valuesInScopeFromParameters)
        }

        const possibleValueToMap = parseAttributeExpression(
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          propertyAccessExpression.expression,
          existingHighlightBounds,
          alreadyExistingUIDs,
          [],
          applySteganography,
          'part-of-expression',
        )
        const possibleMapFunction = parseAttributeExpression(
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          firstArgument,
          existingHighlightBounds,
          alreadyExistingUIDs,
          [],
          applySteganography,
          'part-of-expression',
        )
        return applicative2Either(
          (valueToMapWithMetadata, mapFunctionWithMetadata) => {
            return merge2WithParserMetadata(
              valueToMapWithMetadata,
              mapFunctionWithMetadata,
              (valueToMap, mapFunction) => {
                return withParserMetadata(
                  {
                    valueToMap: valueToMap,
                    mapFunction: mapFunction,
                    valuesInScopeFromParameters: valuesInScopeFromParameters,
                  },
                  {},
                  [],
                  [],
                )
              },
            )
          },
          possibleValueToMap,
          possibleMapFunction,
        )
      }
    }
  }
  return left('Not a map expression.')
}

export function parseJSExpressionMapOrOtherJavascript(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  jsxExpression: TS.Node,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSExpressionMapOrOtherJavascript>> {
  const expression = TS.isJsxExpression(jsxExpression) ? jsxExpression.expression : jsxExpression
  const expressionFullText = expression == null ? '' : expression.getText(sourceFile)
  const expressionForLocation = expression ?? jsxExpression
  const expressionAndText = createExpressionAndText(
    expression,
    expressionFullText,
    expressionForLocation.getFullStart(),
    expressionForLocation.getEnd(),
  )

  const firstToken = jsxExpression.getFirstToken(sourceFile)
  const lastToken = jsxExpression.getLastToken(sourceFile)
  const commentsOnFirstToken = firstToken == null ? [] : getTrailingComments(sourceText, firstToken)
  const commentsOnLastToken = lastToken == null ? [] : getLeadingComments(sourceText, lastToken)
  const commentsOnExpression = getCommentsOnExpression(sourceText, jsxExpression)
  const comments = parsedComments(
    [...commentsOnFirstToken, ...commentsOnExpression.leadingComments],
    [...commentsOnExpression.trailingComments, ...commentsOnLastToken],
  )

  const alreadyExistingUIDsSnapshot: Set<string> = new Set(alreadyExistingUIDs)
  const possibleMapExpressionParts = parseOutMapExpressionParts(
    sourceFile,
    sourceText,
    filename,
    imports,
    topLevelNames,
    propsObjectName,
    jsxExpression,
    existingHighlightBounds,
    alreadyExistingUIDs,
    applySteganography,
  )
  const possibleMapExpressionResult = mapEither((rightValue) => {
    const mapWithMetadata = mapParserMetadata((value) => {
      return createMapExpression(
        sourceFile,
        value.valueToMap,
        value.mapFunction,
        value.valuesInScopeFromParameters,
        comments,
        alreadyExistingUIDs,
        jsxExpression,
      )
    }, rightValue)
    return {
      ...mapWithMetadata,
      highlightBounds: {
        ...mapWithMetadata.highlightBounds,
        ...buildHighlightBoundsForUids(sourceFile, jsxExpression, mapWithMetadata.value.uid),
      },
    }
  }, possibleMapExpressionParts)

  if (isRight(possibleMapExpressionResult)) {
    return possibleMapExpressionResult
  } else {
    alreadyExistingUIDs.clear()
    alreadyExistingUIDsSnapshot.forEach((uid) => alreadyExistingUIDs.add(uid))
  }

  return failOnNullResult(
    parseOtherJavaScript<JSExpressionMapOrOtherJavascript>(
      sourceFile,
      sourceText,
      filename,
      [expressionAndText],
      imports,
      topLevelNames,
      propsObjectName,
      existingHighlightBounds,
      alreadyExistingUIDs,
      '',
      applySteganography,
      'parse-statements',
      (
        code,
        _definedWithin,
        definedElsewhere,
        fileSourceNode,
        parsedElementsWithin,
        params,
        statements,
      ) => {
        if (code === '') {
          return right(
            createExpressionOtherJavaScript(
              sourceFile,
              jsxExpression,
              params,
              expressionFullText,
              expressionFullText,
              'return undefined',
              definedElsewhere,
              null,
              inPositionToElementsWithin(parsedElementsWithin),
              comments,
              alreadyExistingUIDs,
              existingHighlightBounds,
              imports,
            ),
          )
        } else {
          const { map } = fileSourceNode.toStringWithSourceMap({ file: filename })
          const rawMap = map.toJSON()

          const dataUIDFixed = insertDataUIDsIntoCode(
            filename,
            sourceText,
            expressionFullText,
            rawMap,
            parsedElementsWithin,
            false,
            sourceFile.fileName,
          )
          return flatMapEither((dataUIDFixResult) => {
            const transpileEither = wrapAndTranspileJavascript(
              sourceFile.fileName,
              sourceFile.text,
              dataUIDFixResult.code,
              dataUIDFixResult.sourceMap,
              parsedElementsWithin,
              applySteganography,
            )

            return mapEither((transpileResult) => {
              const returnPrepended = prependToSourceString(
                sourceFile.fileName,
                sourceFile.text,
                transpileResult.code,
                transpileResult.sourceMap,
                RETURN_TO_PREPEND,
                '',
              )
              // Sneak the function in here if something needs to use it to display
              // the element on the canvas.
              let innerDefinedElsewhere = definedElsewhere
              if (Object.keys(parsedElementsWithin).length > 0) {
                innerDefinedElsewhere = [...innerDefinedElsewhere, JSX_CANVAS_LOOKUP_FUNCTION_NAME]
              }
              return createExpressionOtherJavaScript(
                sourceFile,
                jsxExpression,
                params,
                expressionFullText,
                dataUIDFixResult.code,
                returnPrepended.code,
                innerDefinedElsewhere,
                returnPrepended.sourceMap,
                inPositionToElementsWithin(parsedElementsWithin),
                comments,
                alreadyExistingUIDs,
                existingHighlightBounds,
                imports,
              )
            }, transpileEither)
          }, dataUIDFixed)
        }
      },
    ),
  )
}

function generateUIDAndAddToExistingUIDs(
  sourceFile: TS.SourceFile,
  value: any,
  alreadyExistingUIDs: Set<string>,
  nodeOrNodes: TS.Node | Array<TS.Node>,
): string {
  const hash = hashObject({
    fileName: sourceFile.fileName,
    value: value,
    bounds: getBoundsOfNodes(sourceFile, nodeOrNodes),
  })
  return generateConsistentUID(hash)
}

function createRawExpressionValue(
  sourceFile: TS.SourceFile,
  value: any,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
  nodeOrNodes: TS.Node | Array<TS.Node>,
): JSExpressionValue<any> {
  const uid = generateUIDAndAddToExistingUIDs(sourceFile, value, alreadyExistingUIDs, nodeOrNodes)
  return jsExpressionValue(value, comments, uid)
}

function createExpressionValue(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  value: any,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<JSExpressionValue<any>> {
  const expression = createRawExpressionValue(
    sourceFile,
    value,
    comments,
    alreadyExistingUIDs,
    node,
  )
  return withParserMetadata(
    expression,
    buildHighlightBoundsForUids(sourceFile, node, expression.uid),
    [],
    [],
  )
}

function createMapExpression(
  sourceFile: TS.SourceFile,
  valueToMap: JSExpression,
  mapFunction: JSExpression,
  valuesInScopeFromParameters: Array<string>,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
  nodeOrNodes: TS.Node | Array<TS.Node>,
): JSXMapExpression {
  const withoutUID = jsxMapExpression(
    clearExpressionUniqueIDs(valueToMap),
    clearExpressionUniqueIDs(mapFunction),
    comments,
    valuesInScopeFromParameters,
    '',
  )
  const uid = getUIDFromCommentsOrValue(
    comments,
    sourceFile,
    withoutUID,
    alreadyExistingUIDs,
    nodeOrNodes,
  )
  return jsxMapExpression(valueToMap, mapFunction, comments, valuesInScopeFromParameters, uid)
}

function createExpressionOtherJavaScript(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  params: Array<Param>,
  originalJavascript: string,
  javascriptWithUIDs: string,
  transpiledJavascript: string,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  imports: Imports,
): JSExpressionOtherJavaScript {
  // Ideally the value we hash is stable regardless of location, so exclude the SourceMap value from here and provide an empty UID.
  const withoutUID = jsExpressionOtherJavaScript(
    params,
    originalJavascript,
    javascriptWithUIDs,
    transpiledJavascript,
    definedElsewhere,
    null,
    elementsWithin,
    comments,
    '',
  )
  const { uid } = makeNewUIDFromOriginatingElement(
    sourceFile,
    node,
    null,
    [jsxAttributesEntry('expression', withoutUID, emptyComments)],
    existingHighlightBounds,
    alreadyExistingUIDs,
    comments,
    imports,
  )
  return jsExpressionOtherJavaScript(
    params,
    originalJavascript,
    javascriptWithUIDs,
    transpiledJavascript,
    definedElsewhere,
    sourceMap,
    elementsWithin,
    comments,
    uid,
  )
}

function createExpressionNestedArray(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  arrayContents: Array<JSXArrayElement>,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
  propsUsed: Array<string>,
  definedElsewhere: Array<string>,
): WithParserMetadata<JSExpressionNestedArray> {
  const value = jsExpressionNestedArray(arrayContents, comments, '')
  const uid = generateUIDAndAddToExistingUIDs(sourceFile, value, alreadyExistingUIDs, node)
  const expression = jsExpressionNestedArray(arrayContents, comments, uid)
  return withParserMetadata(
    expression,
    buildHighlightBoundsForUids(sourceFile, node, uid),
    propsUsed,
    definedElsewhere,
  )
}

function createExpressionNestedObject(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  objectContents: Array<JSXProperty>,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
  propsUsed: Array<string>,
  definedElsewhere: Array<string>,
): WithParserMetadata<JSExpressionNestedObject> {
  const value = jsExpressionNestedObject(objectContents, comments, '')
  const uid = generateUIDAndAddToExistingUIDs(sourceFile, value, alreadyExistingUIDs, node)
  const expression = jsExpressionNestedObject(objectContents, comments, uid)
  return withParserMetadata(
    expression,
    buildHighlightBoundsForUids(sourceFile, node, uid),
    propsUsed,
    definedElsewhere,
  )
}

function createExpressionFunctionCall(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  functionName: string,
  parameters: Array<JSExpression>,
  alreadyExistingUIDs: Set<string>,
  propsUsed: Array<string>,
  definedElsewhere: Array<string>,
): WithParserMetadata<JSExpressionFunctionCall> {
  const value = jsExpressionFunctionCall(functionName, parameters, '')
  const uid = generateUIDAndAddToExistingUIDs(sourceFile, value, alreadyExistingUIDs, node)
  const expression = jsExpressionFunctionCall(functionName, parameters, uid)
  return withParserMetadata(
    expression,
    buildHighlightBoundsForUids(sourceFile, node, uid),
    propsUsed,
    definedElsewhere,
  )
}

function getUIDFromCommentsOrValue(
  comments: ParsedComments,
  sourceFile: TS.SourceFile,
  value: JSExpression,
  alreadyExistingUIDs: Set<string>,
  nodeOrNodes: TS.Node | Array<TS.Node>,
): string {
  const parsedUID = parseUIDFromComments(comments)
  return foldEither(
    () => {
      return generateUIDAndAddToExistingUIDs(sourceFile, value, alreadyExistingUIDs, nodeOrNodes)
    },
    (uidFromComments) => {
      return uidFromComments
    },
    parsedUID,
  )
}

function createNodeSourceMap(sourceFile: TS.SourceFile, node: TS.Node): RawSourceMap {
  const sourceMapGenerator = new SourceMapGenerator()
  const originalPosition = TS.getLineAndCharacterOfPosition(sourceFile, node.getStart(sourceFile))
  sourceMapGenerator.addMapping({
    generated: {
      line: 1,
      column: 1,
    },
    source: sourceFile.fileName,
    original: {
      line: originalPosition.line + 1,
      column: originalPosition.character + 1,
    },
  })
  sourceMapGenerator.setSourceContent(sourceFile.fileName, sourceFile.getFullText(sourceFile))
  return sourceMapGenerator.toJSON()
}

function isOptionallyChained(
  expression: TS.ElementAccessExpression | TS.PropertyAccessExpression,
): OptionallyChained {
  if (expression.questionDotToken == null) {
    return 'not-optionally-chained'
  } else {
    return 'optionally-chained'
  }
}

function createJSElementAccess(
  sourceFile: TS.SourceFile,
  node: TS.ElementAccessExpression,
  onValue: JSExpression,
  element: JSExpression,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<JSElementAccess> {
  const originalJavascript = node.getText(sourceFile)
  const optionallyChained = isOptionallyChained(node)
  const value = jsElementAccess(
    clearExpressionUniqueIDsAndSourceMaps(onValue),
    clearExpressionUniqueIDsAndSourceMaps(element),
    '',
    null,
    comments,
    originalJavascript,
    optionallyChained,
  )
  const uid = getUIDFromCommentsOrValue(comments, sourceFile, value, alreadyExistingUIDs, node)
  const valueWithUID = jsElementAccess(
    onValue,
    element,
    uid,
    createNodeSourceMap(sourceFile, node),
    comments,
    originalJavascript,
    optionallyChained,
  )
  return withParserMetadata(
    valueWithUID,
    buildHighlightBoundsForUids(sourceFile, node, uid),
    [],
    [],
  )
}

function createJSPropertyAccess(
  sourceFile: TS.SourceFile,
  node: TS.PropertyAccessExpression,
  onValue: JSExpression,
  property: string,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<JSPropertyAccess> {
  const originalJavascript = node.getText(sourceFile)
  const optionallyChained = isOptionallyChained(node)
  const value = jsPropertyAccess(
    clearExpressionUniqueIDsAndSourceMaps(onValue),
    property,
    '',
    null,
    comments,
    originalJavascript,
    optionallyChained,
  )
  const uid = getUIDFromCommentsOrValue(comments, sourceFile, value, alreadyExistingUIDs, node)
  const valueWithUID = jsPropertyAccess(
    onValue,
    property,
    uid,
    createNodeSourceMap(sourceFile, node),
    comments,
    originalJavascript,
    optionallyChained,
  )
  return withParserMetadata(
    valueWithUID,
    buildHighlightBoundsForUids(sourceFile, node, uid),
    [],
    [],
  )
}

function createJSIdentifier(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  identifierName: string,
  comments: ParsedComments,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<JSIdentifier> {
  const value = jsIdentifier(identifierName, '', null, comments)
  const uid = getUIDFromCommentsOrValue(comments, sourceFile, value, alreadyExistingUIDs, node)
  const valueWithUID = jsIdentifier(
    identifierName,
    uid,
    createNodeSourceMap(sourceFile, node),
    comments,
  )
  return withParserMetadata(
    valueWithUID,
    buildHighlightBoundsForUids(sourceFile, node, uid),
    [],
    [],
  )
}

function createArbitraryJSBlock(
  sourceFile: TS.SourceFile,
  params: Array<Param>,
  javascript: string,
  transpiledJavascript: string,
  definedWithin: Array<string>,
  definedElsewhere: Array<string>,
  sourceMap: RawSourceMap | null,
  elementsWithin: ElementsWithin,
  alreadyExistingUIDs: Set<string>,
  statements: Array<JSArbitraryStatement>,
  nodeOrNodes: TS.Node | Array<TS.Node>,
): ArbitraryJSBlock {
  const value = arbitraryJSBlock(
    params,
    javascript,
    transpiledJavascript,
    definedWithin,
    definedElsewhere,
    null,
    elementsWithin,
    statements,
    '',
  )
  const uid = generateUIDAndAddToExistingUIDs(sourceFile, value, alreadyExistingUIDs, nodeOrNodes)
  return arbitraryJSBlock(
    params,
    javascript,
    transpiledJavascript,
    definedWithin,
    definedElsewhere,
    sourceMap,
    elementsWithin,
    statements,
    uid,
  )
}

function createJSXTextBlock(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  text: string,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<JSXTextBlock> {
  const uid = generateUIDAndAddToExistingUIDs(sourceFile, text, alreadyExistingUIDs, node)
  const block = jsxTextBlock(text, uid)
  return withParserMetadata(block, buildHighlightBoundsForUids(sourceFile, node, uid), [], [])
}

export function parseAttributeExpression(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  expression: TS.Expression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  trailingCommentsFromPriorToken: Array<Comment>,
  applySteganography: SteganographyMode,
  partOfExpression: PartOfExpression,
): Either<string, WithParserMetadata<JSExpression>> {
  let comments = getComments(sourceText, expression)
  if (trailingCommentsFromPriorToken.length > 0) {
    comments = parsedComments(
      [...trailingCommentsFromPriorToken, ...comments.leadingComments],
      comments.trailingComments,
    )
  }
  if (TS.isArrayLiteralExpression(expression)) {
    return parseArrayLiteralExpression(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
  } else if (TS.isCallExpression(expression)) {
    // Parse the case that an attribute invokes a special case function.
    // Then we parse out the parameters passed to the function.
    // Commented out as the style of this is likely to be re-used but we're not using it right now.
    if (TS.isPropertyAccessExpression(expression.expression)) {
      const propertyAccess = expression.expression
      const leftHandSide = propertyAccess.expression
      const identifier = propertyAccess.name
      if (leftHandSide.getText(sourceFile) === 'UtopiaUtils') {
        let highlightBounds = existingHighlightBounds
        let parsedArgumentAttributes: Array<JSExpression> = []
        let propsUsed: Array<string> = []
        let definedElsewhere: Array<string> = []
        for (const argument of expression.arguments) {
          const parsedArgument = parseAttributeExpression(
            sourceFile,
            sourceText,
            filename,
            imports,
            topLevelNames,
            propsObjectName,
            argument,
            highlightBounds,
            alreadyExistingUIDs,
            [],
            applySteganography,
            'part-of-expression',
          )
          if (isLeft(parsedArgument)) {
            return left(`Error parsing function expression: ${parsedArgument.value}`)
          } else {
            parsedArgumentAttributes.push(parsedArgument.value.value)
            highlightBounds = mergeHighlightBounds(
              highlightBounds,
              parsedArgument.value.highlightBounds,
            )
            propsUsed.push(...parsedArgument.value.propsUsed)
            definedElsewhere.push(...parsedArgument.value.definedElsewhere)
          }
        }
        const fnCall = createExpressionFunctionCall(
          sourceFile,
          identifier,
          identifier.getText(sourceFile),
          parsedArgumentAttributes,
          alreadyExistingUIDs,
          propsUsed,
          definedElsewhere,
        )
        highlightBounds = mergeHighlightBounds(highlightBounds, fnCall.highlightBounds)
        propsUsed.push(...fnCall.propsUsed)
        definedElsewhere.push(...fnCall.definedElsewhere)
        return right(withParserMetadata(fnCall.value, highlightBounds, propsUsed, definedElsewhere))
      }
    }
    return parseJSExpressionMapOrOtherJavascript(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
  } else if (TS.isElementAccessExpression(expression)) {
    return parseElementAccessExpression(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
      trailingCommentsFromPriorToken,
      comments,
      partOfExpression,
      applySteganography,
    )
  } else if (TS.isPropertyAccessExpression(expression)) {
    return parsePropertyAccessExpression(
      expression,
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      existingHighlightBounds,
      alreadyExistingUIDs,
      trailingCommentsFromPriorToken,
      comments,
      partOfExpression,
      applySteganography,
    )
  } else if (
    TS.isIdentifier(expression) &&
    expression.originalKeywordKind === TS.SyntaxKind.UndefinedKeyword
  ) {
    return right(
      createExpressionValue(sourceFile, expression, undefined, comments, alreadyExistingUIDs),
    )
  } else if (TS.isIdentifier(expression)) {
    return parseIdentifier(sourceFile, expression, comments, partOfExpression, alreadyExistingUIDs)
  } else if (TS.isNumericLiteral(expression)) {
    return right(
      createExpressionValue(
        sourceFile,
        expression,
        Number.parseFloat(expression.getText(sourceFile)),
        comments,
        alreadyExistingUIDs,
      ),
    )
  } else if (TS.isObjectLiteralExpression(expression)) {
    return parseObjectLiteralExpression(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
  } else if (TS.isPrefixUnaryExpression(expression)) {
    // Cater for negative numbers, because of course they're done in a weird way.
    const operator = expression.operator
    if (operator === TS.SyntaxKind.MinusToken) {
      const operand = expression.operand
      if (TS.isNumericLiteral(operand)) {
        return right(
          createExpressionValue(
            sourceFile,
            operand,
            Number.parseFloat(operand.getText(sourceFile)) * -1,
            comments,
            alreadyExistingUIDs,
          ),
        )
      }
    }
    return parseJSExpressionMapOrOtherJavascript(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
  } else if (TS.isStringLiteral(expression)) {
    return right(
      createExpressionValue(sourceFile, expression, expression.text, comments, alreadyExistingUIDs),
    )
  } else {
    switch (expression.kind) {
      case TS.SyntaxKind.TrueKeyword:
        return right(
          createExpressionValue(sourceFile, expression, true, comments, alreadyExistingUIDs),
        )
      case TS.SyntaxKind.FalseKeyword:
        return right(
          createExpressionValue(sourceFile, expression, false, comments, alreadyExistingUIDs),
        )
      case TS.SyntaxKind.NullKeyword:
        return right(
          createExpressionValue(sourceFile, expression, null, comments, alreadyExistingUIDs),
        )
      default:
        return parseJSExpressionMapOrOtherJavascript(
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          expression,
          existingHighlightBounds,
          alreadyExistingUIDs,
          applySteganography,
        )
    }
  }
}

function parseIdentifier(
  sourceFile: TS.SourceFile,
  expression: TS.Identifier,
  comments: ParsedComments,
  partOfExpression: PartOfExpression,
  alreadyExistingUIDs: Set<string>,
): Either<string, WithParserMetadata<JSIdentifier>> {
  return right(
    createJSIdentifier(
      sourceFile,
      expression,
      expression.text,
      partOfExpression === 'outermost-expression' ? comments : emptyComments,
      alreadyExistingUIDs,
    ),
  )
}

function parseElementAccessExpression(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  expression: TS.ElementAccessExpression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  trailingCommentsFromPriorToken: Array<Comment>,
  comments: ParsedComments,
  partOfExpression: PartOfExpression,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSElementAccess | JSExpressionMapOrOtherJavascript>> {
  const parsedOnValue = parseAttributeExpression(
    sourceFile,
    sourceText,
    filename,
    imports,
    topLevelNames,
    propsObjectName,
    expression.expression,
    existingHighlightBounds,
    alreadyExistingUIDs,
    trailingCommentsFromPriorToken,
    applySteganography,
    'part-of-expression',
  )
  const parsedElement = parseAttributeExpression(
    sourceFile,
    sourceText,
    filename,
    imports,
    topLevelNames,
    propsObjectName,
    expression.argumentExpression,
    existingHighlightBounds,
    alreadyExistingUIDs,
    trailingCommentsFromPriorToken,
    applySteganography,
    'part-of-expression',
  )
  return applicative2Either(
    (onValue, element) => {
      return merge2WithParserMetadata(onValue, element, (onValueValue, elementValue) => {
        return createJSElementAccess(
          sourceFile,
          expression,
          onValueValue,
          elementValue,
          partOfExpression === 'outermost-expression' ? comments : emptyComments,
          alreadyExistingUIDs,
        )
      })
    },
    parsedOnValue,
    parsedElement,
  )
}

function parsePropertyAccessExpression(
  expression: TS.PropertyAccessExpression,
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  trailingCommentsFromPriorToken: Array<Comment>,
  comments: ParsedComments,
  partOfExpression: PartOfExpression,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSPropertyAccess | JSExpressionMapOrOtherJavascript>> {
  const propertyName = expression.name.text
  const parsedOnValue = parseAttributeExpression(
    sourceFile,
    sourceText,
    filename,
    imports,
    topLevelNames,
    propsObjectName,
    expression.expression,
    existingHighlightBounds,
    alreadyExistingUIDs,
    trailingCommentsFromPriorToken,
    applySteganography,
    'part-of-expression',
  )
  return mapEither((onValue) => {
    const propertyAccessResult = createJSPropertyAccess(
      sourceFile,
      expression,
      onValue.value,
      propertyName,
      partOfExpression === 'outermost-expression' ? comments : emptyComments,
      alreadyExistingUIDs,
    )

    // Ensure that `propsUsed` gets populated when we have a `props.something` kind of situation.
    let propsUsedForProperty: Array<string> = []
    if (onValue.value.type === 'JS_IDENTIFIER' && onValue.value.name === propsObjectName) {
      propsUsedForProperty.push(propertyName)
    }

    return withParserMetadata(
      propertyAccessResult.value,
      mergeHighlightBounds(propertyAccessResult.highlightBounds, onValue.highlightBounds),
      [...propertyAccessResult.propsUsed, ...onValue.propsUsed, ...propsUsedForProperty],
      [...propertyAccessResult.definedElsewhere, ...onValue.definedElsewhere],
    )
  }, parsedOnValue)
}

function getAttributeExpression(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  initializer: TS.StringLiteral | TS.JsxExpression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSExpression> | null> {
  if (TS.isStringLiteral(initializer)) {
    const comments = getComments(sourceText, initializer)
    return right(
      createExpressionValue(
        sourceFile,
        initializer,
        initializer.text,
        comments,
        alreadyExistingUIDs,
      ),
    )
  } else if (TS.isJsxExpression(initializer)) {
    // Need to handle trailing comments on the open brace,
    // passing them down to be the handled elsewhere.
    let openBraceComments: ParsedComments = emptyComments
    const firstChild = initializer.getChildAt(0, sourceFile)
    if (firstChild != null && firstChild.kind === TS.SyntaxKind.OpenBraceToken) {
      openBraceComments = getComments(sourceText, firstChild)
    }

    // Handle the expression itself.
    if (initializer.expression == null) {
      const comments = getComments(sourceText, initializer)

      const withoutParserMetadata = createExpressionOtherJavaScript(
        sourceFile,
        initializer,
        [],
        'null',
        'null',
        'null',
        [],
        null,
        {},
        comments,
        alreadyExistingUIDs,
        existingHighlightBounds,
        imports,
      )

      return right(
        withParserMetadata(
          withoutParserMetadata,
          buildHighlightBoundsForUids(sourceFile, initializer, withoutParserMetadata.uid),
          [],
          [],
        ),
      )
    } else if (
      TS.isJsxElement(initializer.expression) ||
      TS.isJsxSelfClosingElement(initializer.expression)
    ) {
      const parseResult = parseOutJSXElements(
        sourceFile,
        sourceText,
        filename,
        [initializer.expression],
        imports,
        topLevelNames,
        propsObjectName,
        existingHighlightBounds,
        alreadyExistingUIDs,
        applySteganography,
      )

      if (parseResult.type === 'LEFT') {
        return parseResult
      }

      const parsedElements = parseResult.value.value

      if (parsedElements.length !== 1 || parsedElements[0].value.type !== 'JSX_ELEMENT') {
        return left('Cannot parse jsx element')
      }

      return right(
        withParserMetadata(
          parsedElements[0].value,
          parseResult.value.highlightBounds,
          parseResult.value.propsUsed,
          parseResult.value.definedElsewhere,
        ),
      )
    } else {
      return parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        initializer.expression,
        existingHighlightBounds,
        alreadyExistingUIDs,
        openBraceComments.trailingComments,
        applySteganography,
        'outermost-expression',
      )
    }
  } else {
    throw new Error(`Unhandled initializer case: ${initializer}`)
  }
}

function parseElementProps(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  attributes: TS.JsxAttributes,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  leadingCommentsAgainstClosingToken: Array<Comment>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<JSXAttributes>> {
  let result: JSXAttributes = []
  let highlightBounds = existingHighlightBounds
  let propsUsed = []
  let definedElsewhere = []
  // Maintain this so that we can still use early returns.
  let propIndex: number = 0
  for (const prop of attributes?.properties ?? []) {
    let propComments = getComments(sourceText, prop)
    if (propIndex === attributes.properties.length - 1) {
      propComments = parsedComments(propComments.leadingComments, [
        ...propComments.trailingComments,
        ...leadingCommentsAgainstClosingToken,
      ])
    }
    if (TS.isJsxSpreadAttribute(prop)) {
      const attributeResult = parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        prop.expression,
        highlightBounds,
        alreadyExistingUIDs,
        [],
        applySteganography,
        'part-of-expression',
      )
      if (isLeft(attributeResult)) {
        return attributeResult
      } else {
        if (attributeResult.value == null) {
          return left('Null returned.')
        }
        result.push(jsxAttributesSpread(attributeResult.value.value, propComments))
        highlightBounds = mergeHighlightBounds(
          highlightBounds,
          attributeResult.value.highlightBounds,
        )
        propsUsed.push(...attributeResult.value.propsUsed)
        definedElsewhere.push(...attributeResult.value.definedElsewhere)
      }
    } else if (TS.isJsxAttribute(prop)) {
      if (prop.initializer == null) {
        const expression = createExpressionValue(
          sourceFile,
          prop.name,
          true,
          emptyComments,
          alreadyExistingUIDs,
        )
        result.push(
          jsxAttributesEntry(prop.name.getText(sourceFile), expression.value, propComments),
        )
        highlightBounds = mergeHighlightBounds(highlightBounds, expression.highlightBounds)
        propsUsed.push(...expression.propsUsed)
        definedElsewhere.push(...expression.definedElsewhere)
      } else {
        const attributeResult = getAttributeExpression(
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          prop.initializer,
          highlightBounds,
          alreadyExistingUIDs,
          applySteganography,
        )
        if (isLeft(attributeResult)) {
          return attributeResult
        } else {
          if (attributeResult.value != null) {
            result.push(
              jsxAttributesEntry(
                prop.name.getText(sourceFile),
                attributeResult.value.value,
                propComments,
              ),
            )
            highlightBounds = mergeHighlightBounds(
              highlightBounds,
              attributeResult.value.highlightBounds,
            )
            propsUsed.push(...attributeResult.value.propsUsed)
            definedElsewhere.push(...attributeResult.value.definedElsewhere)
          }
        }
      }
    } else {
      return left(`Invalid attribute found.`)
    }

    propIndex += 1
  }
  return right(withParserMetadata(result, highlightBounds, propsUsed, definedElsewhere))
}

type TSTextOrExpression = TS.JsxText | TS.JsxExpression | TS.ConditionalExpression
type TSJSXElement = TS.JsxElement | TS.JsxSelfClosingElement | TS.JsxFragment
type ElementsToParse = Array<TSJSXElement | TSTextOrExpression>

type LiteralLikeTypes =
  | TS.StringLiteral
  | TS.NumericLiteral
  | TS.BigIntLiteral
  | TS.BooleanLiteral
  | TS.NullLiteral

export function isTrueLiteral(node: TS.Node): node is TS.TrueLiteral {
  return node.kind === TS.SyntaxKind.TrueKeyword
}

export function isFalseLiteral(node: TS.Node): node is TS.FalseLiteral {
  return node.kind === TS.SyntaxKind.FalseKeyword
}

export function isNullLiteral(node: TS.Node): node is TS.NullLiteral {
  return node.kind === TS.SyntaxKind.NullKeyword
}

function pullOutElementsToParse(nodes: Array<TS.Node>): Either<string, ElementsToParse> {
  let result: ElementsToParse = []
  for (let index = 0; index < nodes.length; index++) {
    const node = nodes[index]
    if (
      TS.isJsxElement(node) ||
      TS.isJsxSelfClosingElement(node) ||
      TS.isJsxText(node) ||
      TS.isJsxExpression(node) ||
      TS.isJsxFragment(node) ||
      TS.isConditionalExpression(node)
    ) {
      result.push(node)
    } else {
      return left(`Invalid content in JSX.`)
    }
  }
  return right(result)
}

export interface WithParserMetadata<T> {
  value: T
  highlightBounds: Readonly<HighlightBoundsForUids>
  propsUsed: Array<string>
  definedElsewhere: Array<string>
}

export function mapParserMetadata<T, U>(
  transform: (value: T) => U,
  toTransform: WithParserMetadata<T>,
): WithParserMetadata<U> {
  return {
    ...toTransform,
    value: transform(toTransform.value),
  }
}

interface SuccessfullyParsedElement {
  value: JSXElementChild
  startLine: number
  startColumn: number
}

function successfullyParsedElement(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  value: JSXElementChild,
): SuccessfullyParsedElement {
  const startPosition = TS.getLineAndCharacterOfPosition(
    sourceFile,
    originatingElement.getStart(sourceFile, false),
  )
  return {
    value: value,
    startLine: startPosition.line,
    startColumn: startPosition.character,
  }
}

export function withParserMetadata<T>(
  value: T,
  highlightBounds: Readonly<HighlightBoundsForUids>,
  propsUsed: Array<string>,
  definedElsewhere: Array<string>,
): WithParserMetadata<T> {
  return {
    value: value,
    highlightBounds: highlightBounds,
    propsUsed: propsUsed,
    definedElsewhere: definedElsewhere,
  }
}

type ParseElementsResult = Either<string, WithParserMetadata<Array<SuccessfullyParsedElement>>>

function isSpacingTextBlock(element: JSXElementChild): boolean {
  if (isJSXTextBlock(element)) {
    return element.text.trim().length === 0
  } else {
    return false
  }
}

function neighbourCandidateForIgnoring(element: SuccessfullyParsedElement | undefined): boolean {
  return (
    element === undefined ||
    isJSXElement(element.value) ||
    isJSExpressionMapOrOtherJavaScript(element.value)
  )
}

function clearUnnecessarySpacingElements(
  elements: Array<SuccessfullyParsedElement>,
): Array<SuccessfullyParsedElement> {
  let result: Array<SuccessfullyParsedElement> = []
  for (let index = 0; index < elements.length; index++) {
    const element = elements[index]
    let includeElement: boolean = true
    if (isSpacingTextBlock(element.value)) {
      const onLeft = elements[index - 1]
      const onRight = elements[index + 1]
      if (neighbourCandidateForIgnoring(onLeft) && neighbourCandidateForIgnoring(onRight)) {
        includeElement = false
      }
    }
    if (includeElement) {
      result.push(element)
    }
  }
  return result
}

function parseJSXElementName(
  sourceFile: TS.SourceFile,
  tagName: TS.JsxTagNameExpression,
): Either<string, JSXElementName | null> {
  if (tagName == null) {
    return right(null)
  }
  if (TS.isIdentifier(tagName)) {
    return right(jsxElementName(tagName.getText(sourceFile), []))
  } else if (TS.isPropertyAccessExpression(tagName)) {
    let expressionParts: Array<string> = []
    function walkTree(expr: TS.JsxTagNameExpression | TS.PrivateIdentifier): void {
      if (TS.isIdentifier(expr)) {
        expressionParts.push(expr.getText(sourceFile))
      } else if (TS.isPropertyAccessExpression(expr)) {
        walkTree(expr.expression)
        walkTree(expr.name)
      }
    }
    walkTree(tagName)
    if (expressionParts.length === 0) {
      return left('Unable to parse JSX property access.')
    } else {
      const [baseVariable, ...remainder] = expressionParts
      return right(jsxElementName(baseVariable, remainder))
    }
  } else {
    return left('Unable to handle ThisExpression.')
  }
}

function buildHighlightBounds(
  sourceFile: TS.SourceFile,
  boundingElements: TS.Node | Array<TS.Node>,
  uid: string,
): HighlightBounds | null {
  const bounds = getBoundsOfNodes(sourceFile, boundingElements)
  if (bounds == null) {
    return null
  } else {
    return {
      startCol: bounds.start.character,
      startLine: bounds.start.line,
      endCol: bounds.end.character,
      endLine: bounds.end.line,
      uid: uid,
    }
  }
}

function buildHighlightBoundsForExpressionsAndText(
  sourceFile: TS.SourceFile,
  expressions: Array<ExpressionAndText<TS.Node>>,
  uid: string,
): HighlightBounds {
  // Default to using this first.
  const baseHighlightBounds = buildHighlightBounds(
    sourceFile,
    mapDropNulls((e) => e.expression, expressions),
    uid,
  )
  if (baseHighlightBounds == null) {
    // Find the bounds of the bounds.
    let lowestStart: number | null = null
    let highestEnd: number | null = null
    for (const expression of expressions) {
      lowestStart = Math.min(lowestStart ?? expression.startPos, expression.startPos)
      highestEnd = Math.max(highestEnd ?? expression.endPos, expression.endPos)
    }
    if (lowestStart == null || highestEnd == null) {
      // In this case fail outright as the bounds cannot be produced from an empty array.
      throw new Error(`Unable to construct bounds for ${uid} with nothing to construct them from.`)
    } else {
      // Build the bounds value itself.
      const start = TS.getLineAndCharacterOfPosition(sourceFile, lowestStart)
      const end = TS.getLineAndCharacterOfPosition(sourceFile, highestEnd)
      return {
        startCol: start.character,
        startLine: start.line,
        endCol: end.character,
        endLine: end.line,
        uid: uid,
      }
    }
  } else {
    return baseHighlightBounds
  }
}

function buildHighlightBoundsForUids(
  sourceFile: TS.SourceFile,
  boundingElements: TS.Node | Array<TS.Node>,
  uid: string,
): HighlightBoundsForUids {
  const highlightBounds = buildHighlightBounds(sourceFile, boundingElements, uid)
  if (highlightBounds == null) {
    return {}
  } else {
    return {
      [uid]: highlightBounds,
    }
  }
}

function mergeHighlightBounds(
  ...multipleBounds: Array<HighlightBoundsForUids>
): Readonly<HighlightBoundsForUids> {
  let result: HighlightBoundsForUids = {}
  for (const bounds of multipleBounds) {
    Object.assign(result, bounds)
  }
  return result
}

function merge2WithParserMetadata<T1, T2, U>(
  first: WithParserMetadata<T1>,
  second: WithParserMetadata<T2>,
  combine: (t1: T1, t2: T2) => WithParserMetadata<U>,
): WithParserMetadata<U> {
  const combinedResult = combine(first.value, second.value)
  const highlightBounds = mergeHighlightBounds(
    combinedResult.highlightBounds,
    first.highlightBounds,
    second.highlightBounds,
  )
  const propsUsed = [...combinedResult.propsUsed, ...first.propsUsed, ...second.propsUsed]
  const definedElsewhere = [
    ...combinedResult.definedElsewhere,
    ...first.definedElsewhere,
    ...second.definedElsewhere,
  ]
  return withParserMetadata(combinedResult.value, highlightBounds, propsUsed, definedElsewhere)
}

function addBoundsIntoWithParser<T>(
  bounds: HighlightBoundsForUids,
  withParser: WithParserMetadata<T>,
): WithParserMetadata<T> {
  return {
    ...withParser,
    highlightBounds: mergeHighlightBounds(bounds, withParser.highlightBounds),
  }
}

function addToHighlightBounds(
  existing: Readonly<HighlightBoundsForUids>,
  toAdd: HighlightBounds | null,
): Readonly<HighlightBoundsForUids> {
  if (toAdd == null) {
    return existing
  } else {
    let result: HighlightBoundsForUids = { ...existing }
    result[toAdd.uid] = toAdd
    return result
  }
}

interface UpdateUIDResult {
  uid: string
  attributes: WithParserMetadata<JSXAttributes | null>
}

function getUIDBasedOnElement(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  elementName: JSXElementName | string | null,
  props: JSXAttributes | JSExpression | null,
  alreadyExistingUIDs: Set<string>,
): string {
  let cleansedProps: typeof props
  if (props == null) {
    cleansedProps = null
  } else if (Array.isArray(props)) {
    cleansedProps = clearAttributesSourceMaps(clearAttributesUniqueIDs(props))
  } else {
    cleansedProps = clearExpressionSourceMaps(clearExpressionUniqueIDs(props))
  }
  const hash = hashObject({
    fileName: sourceFile.fileName,
    bounds: getBoundsOfNodes(sourceFile, originatingElement),
    name: elementName,
    props: cleansedProps,
  })
  return generateConsistentUID(hash)
}

function forciblyUpdateDataUID(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  elementName: JSXElementName | string | null,
  props: JSXAttributes | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  isFragment: boolean,
): UpdateUIDResult {
  const uid = getUIDBasedOnElement(
    sourceFile,
    originatingElement,
    elementName,
    props,
    alreadyExistingUIDs,
  )
  const uidExpression = createRawExpressionValue(
    sourceFile,
    uid,
    emptyComments,
    alreadyExistingUIDs,
    originatingElement,
  )
  const updatedProps =
    props == null ? null : setJSXAttributesAttribute(props, 'data-uid', uidExpression)
  let highlightBoundsResult: HighlightBoundsForUids = mergeHighlightBounds(
    existingHighlightBounds,
    buildHighlightBoundsForUids(sourceFile, originatingElement, uid),
  )
  // Remove any UIDs that have been eliminated as a result of the update.
  if (props != null && updatedProps != null) {
    const attributeUniqueUIDsBefore = new Set(getAllUniqueUidsFromAttributes(props).uniqueIDs)
    const attributeUniqueUIDsAfter = new Set(getAllUniqueUidsFromAttributes(updatedProps).uniqueIDs)
    const uidsToRemove = difference(attributeUniqueUIDsBefore, attributeUniqueUIDsAfter)
    uidsToRemove.forEach((uidToRemove) => {
      delete highlightBoundsResult[uidToRemove]
    })
  }

  // Include the newly added expression in the highlight bounds,
  // but only if this isn't a fragment as we wont be putting the props in that as it doesn't have any.
  if (!isFragment) {
    highlightBoundsResult = mergeHighlightBounds(
      highlightBoundsResult,
      buildHighlightBoundsForUids(sourceFile, originatingElement, uidExpression.uid),
    )
  }
  return {
    uid: uid,
    attributes: withParserMetadata(updatedProps, highlightBoundsResult, [], []),
  }
}

function makeNewUIDFromOriginatingElement(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  name: JSXElementName | null,
  props: JSXAttributes | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  comments: ParsedComments,
  imports: Imports,
): UpdateUIDResult {
  const isShortHandFragment = name == null
  const isFragment = isShortHandFragment || isReactFragmentName(name, imports)
  const dataUIDAttribute: Either<string, string> =
    props == null ? left('Is a fragment.') : parseUID(props, comments)
  return foldEither(
    (_) => {
      return forciblyUpdateDataUID(
        sourceFile,
        originatingElement,
        name,
        props,
        existingHighlightBounds,
        alreadyExistingUIDs,
        isFragment,
      )
    },
    (uid) => {
      // This implies a duplicate UID, so we should replace it.
      if (uid in existingHighlightBounds || alreadyExistingUIDs.has(uid)) {
        return forciblyUpdateDataUID(
          sourceFile,
          originatingElement,
          name,
          props,
          existingHighlightBounds,
          alreadyExistingUIDs,
          isFragment,
        )
      } else {
        alreadyExistingUIDs.add(uid)
        return {
          uid: uid,
          attributes: withParserMetadata(
            props,
            mergeHighlightBounds(
              existingHighlightBounds,
              buildHighlightBoundsForUids(sourceFile, originatingElement, uid),
            ),
            [],
            [],
          ),
        }
      }
    },
    dataUIDAttribute,
  )
}

function createJSXElementOrFragmentAllocatingUID(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  name: JSXElementName | null, // if name is null we create a fragment
  props: JSXAttributes,
  children: JSXElementChildren,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  imports: Imports,
): WithParserMetadata<SuccessfullyParsedElement> {
  const isShortHandFragment = name == null
  const isFragment = isShortHandFragment || isReactFragmentName(name, imports)
  const { uid: newUID, attributes: updatedProps } = makeNewUIDFromOriginatingElement(
    sourceFile,
    originatingElement,
    name,
    isShortHandFragment ? null : props,
    existingHighlightBounds,
    alreadyExistingUIDs,
    emptyComments,
    imports,
  )

  const startPosition = TS.getLineAndCharacterOfPosition(
    sourceFile,
    originatingElement.getStart(sourceFile, false),
  )

  // Guard against these two values becoming inconsistent with each other.
  if (isShortHandFragment && updatedProps.value != null) {
    throw new Error(
      `Have props for a fragment: ${JSON.stringify(name)}, ${JSON.stringify(updatedProps.value)}`,
    )
  }
  if (!isShortHandFragment && updatedProps.value == null) {
    throw new Error(`Have no props for an element.`)
  }

  return withParserMetadata(
    {
      value: isFragment
        ? jsxFragment(newUID, children, !isShortHandFragment)
        : jsxElement(name, newUID, updatedProps.value ?? [], children),
      startLine: startPosition.line,
      startColumn: startPosition.character,
    },
    updatedProps.highlightBounds,
    [],
    [],
  )
}

type PartOfExpression = 'part-of-expression' | 'outermost-expression'

export function parseOutJSXElements(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  nodesToParse: Array<TS.Node>,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): ParseElementsResult {
  let highlightBounds: HighlightBoundsForUids = existingHighlightBounds
  let propsUsed: Array<string> = []
  let definedElsewhere: Array<string> = []
  function innerParse(nodes: Array<TS.Node>): Either<string, Array<SuccessfullyParsedElement>> {
    // First parse to extract the nodes we really want into a sensible form
    // and fail if there's anything unexpected.
    const validNodes = pullOutElementsToParse(nodes)
    return flatMapEither((toParse) => {
      // Handle the two different cases of either an element or a bunch of JSX inner content.
      let parsedNodes: Array<SuccessfullyParsedElement> = []
      let fragmentDepth: number = 0
      let fragmentChildren: { [key: number]: Array<JSXElementChild> } = {}

      function addParsedElement(addElement: SuccessfullyParsedElement): void {
        if (fragmentDepth === 0) {
          parsedNodes.push(addElement)
        } else {
          let fragmentArray: Array<JSXElementChild> = fragmentChildren[fragmentDepth]
          if (fragmentArray == null) {
            fragmentArray = [addElement.value]
            fragmentChildren[fragmentDepth] = fragmentArray
          } else {
            fragmentArray.push(addElement.value)
          }
        }
      }

      function handleConditionalExpression(
        expression: TS.ConditionalExpression,
        comments: ParsedComments,
      ): Either<string, SuccessfullyParsedElement> {
        const possibleConditional = produceConditionalFromExpression(expression, comments)
        return mapEither((success) => {
          highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)
          propsUsed.push(...success.propsUsed)
          definedElsewhere.push(...success.definedElsewhere)

          return success.value
        }, possibleConditional)
      }

      function handleIdentifier(
        expression: TS.Identifier,
        comments: ParsedComments,
        partOfExpression: PartOfExpression,
      ): Either<string, SuccessfullyParsedElement> {
        const possibleIdentifier = parseIdentifier(
          sourceFile,
          expression,
          comments,
          partOfExpression,
          alreadyExistingUIDs,
        )
        return mapEither((success) => {
          highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)
          propsUsed.push(...success.propsUsed)
          definedElsewhere.push(...success.definedElsewhere)

          return successfullyParsedElement(sourceFile, expression, success.value)
        }, possibleIdentifier)
      }

      function handlePropertyAccessExpression(
        expression: TS.PropertyAccessExpression,
        comments: ParsedComments,
        partOfExpression: PartOfExpression,
      ): Either<string, SuccessfullyParsedElement> {
        const possiblePropertyAccessExpression = parsePropertyAccessExpression(
          expression,
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          existingHighlightBounds,
          alreadyExistingUIDs,
          [],
          comments,
          partOfExpression,
          applySteganography,
        )
        return mapEither((success) => {
          highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)
          propsUsed.push(...success.propsUsed)
          definedElsewhere.push(...success.definedElsewhere)

          return successfullyParsedElement(sourceFile, expression, success.value)
        }, possiblePropertyAccessExpression)
      }

      function handleElementAccessExpression(
        expression: TS.ElementAccessExpression,
        comments: ParsedComments,
        partOfExpression: PartOfExpression,
      ): Either<string, SuccessfullyParsedElement> {
        const possiblePropertyAccessExpression = parseElementAccessExpression(
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          expression,
          existingHighlightBounds,
          alreadyExistingUIDs,
          [],
          comments,
          partOfExpression,
          applySteganography,
        )
        return mapEither((success) => {
          highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)
          propsUsed.push(...success.propsUsed)
          definedElsewhere.push(...success.definedElsewhere)

          return successfullyParsedElement(sourceFile, expression, success.value)
        }, possiblePropertyAccessExpression)
      }

      function getConditionalExpressionComments(
        expression: TS.ConditionalExpression,
      ): ParsedComments {
        return {
          leadingComments: getLeadingComments(sourceText, expression.condition),
          trailingComments: getTrailingComments(sourceText, expression),
          questionTokenComments: {
            leadingComments: getTrailingComments(sourceText, expression.condition),
            trailingComments: [],
          },
        }
      }

      function getConditionalElementComments(elem: TS.JsxExpression): ParsedComments {
        if (elem.expression == null || !TS.isConditionalExpression(elem.expression)) {
          return emptyComments
        }
        const comments = getConditionalExpressionComments(elem.expression)
        const childrenOfExpression = elem.getChildren(sourceFile)
        const lastChild = childrenOfExpression[childrenOfExpression.length - 1]
        comments.trailingComments.push(...getLeadingComments(sourceText, lastChild))

        return comments
      }

      for (const elem of toParse) {
        switch (elem.kind) {
          case TS.SyntaxKind.JsxFragment:
          case TS.SyntaxKind.JsxElement:
          case TS.SyntaxKind.JsxSelfClosingElement: {
            const possibleElement = produceElementOrFragmentFromTSElement(elem)
            if (isLeft(possibleElement)) {
              return possibleElement
            } else {
              addParsedElement(possibleElement.value)
            }
            break
          }
          case TS.SyntaxKind.ConditionalExpression: {
            const possibleCondition = handleConditionalExpression(
              elem,
              getConditionalExpressionComments(elem),
            )
            if (isLeft(possibleCondition)) {
              return possibleCondition
            } else {
              addParsedElement(possibleCondition.value)
            }
            break
          }
          case TS.SyntaxKind.JsxExpression: {
            let parseResult: Either<string, SuccessfullyParsedElement> =
              left('Expression fallback.')
            // Handle ternaries.
            if (elem.expression != null && TS.isConditionalExpression(elem.expression)) {
              parseResult = handleConditionalExpression(
                elem.expression,
                getConditionalElementComments(elem),
              )
            }
            if (
              elem.expression != null &&
              TS.isIdentifier(elem.expression) &&
              elem.expression.originalKeywordKind !== TS.SyntaxKind.UndefinedKeyword
            ) {
              parseResult = handleIdentifier(
                elem.expression,
                getComments(sourceText, elem.expression),
                'outermost-expression',
              )
            }
            if (elem.expression != null && TS.isPropertyAccessExpression(elem.expression)) {
              parseResult = handlePropertyAccessExpression(
                elem.expression,
                getComments(sourceText, elem.expression),
                'outermost-expression',
              )
            }
            if (elem.expression != null && TS.isElementAccessExpression(elem.expression)) {
              parseResult = handleElementAccessExpression(
                elem.expression,
                getComments(sourceText, elem.expression),
                'outermost-expression',
              )
            }
            // Fallback to arbitrary block parsing.
            if (isLeft(parseResult)) {
              parseResult = produceArbitraryBlockFromExpression(elem)
            }

            if (isRight(parseResult)) {
              addParsedElement(parseResult.value)
            } else {
              return parseResult
            }
            break
          }
          case TS.SyntaxKind.JsxText: {
            const possibleText = produceTextFromJsxText(elem)
            addParsedElement(possibleText)
            break
          }
          default:
            const _exhaustiveCheck: never = elem
            throw new Error(`Unhandled elem type ${JSON.stringify(elem)}`)
        }
      }

      if (fragmentDepth === 0) {
        let highlightBoundsToRemove: Set<string> = emptySet()
        for (const parsedNode of parsedNodes) {
          highlightBoundsToRemove.add(parsedNode.value.uid)
        }
        const withoutUnnecessarySpacingElements = clearUnnecessarySpacingElements(parsedNodes)
        for (const parsedNode of withoutUnnecessarySpacingElements) {
          highlightBoundsToRemove.delete(parsedNode.value.uid)
        }
        highlightBoundsToRemove.forEach((toRemove) => {
          delete highlightBounds[toRemove]
        })
        return right(withoutUnnecessarySpacingElements)
      } else {
        return left('Not enough closed fragments.')
      }
    }, validNodes)
  }

  function produceTextFromJsxText(tsText: TS.JsxText): SuccessfullyParsedElement {
    const block = createJSXTextBlock(sourceFile, tsText, tsText.text, alreadyExistingUIDs)
    highlightBounds = mergeHighlightBounds(highlightBounds, block.highlightBounds)
    propsUsed.push(...block.propsUsed)
    definedElsewhere.push(...block.definedElsewhere)
    return successfullyParsedElement(sourceFile, tsText, block.value)
  }

  function produceArbitraryBlockFromExpression(
    tsExpression: TS.Expression | LiteralLikeTypes,
  ): Either<string, SuccessfullyParsedElement> {
    const result = parseJSExpressionMapOrOtherJavascript(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      tsExpression,
      highlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
    return bimapEither(
      (failure) => failure,
      (success) => {
        highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)
        propsUsed.push(...success.propsUsed)
        definedElsewhere.push(...success.definedElsewhere)
        return successfullyParsedElement(sourceFile, tsExpression, success.value)
      },
      result,
    )
  }

  function getChildrenWithoutWhitespaceOnlyText(
    children: Array<JSXElementChild>,
  ): Array<JSXElementChild> {
    let result: Array<JSXElementChild> = []
    for (const childElem of children) {
      if (isJSXTextBlock(childElem) && isEmptyString(childElem.text)) {
        delete highlightBounds[childElem.uid]
      } else {
        result.push(childElem)
      }
    }
    return result
  }

  function produceElementOrFragmentFromTSElement(
    tsElement: TSJSXElement,
  ): Either<string, SuccessfullyParsedElement> {
    let attributes: TS.JsxAttributes
    let tagName: TS.JsxTagNameExpression
    let children: Either<string, Array<JSXElementChild>> = right([])
    let commentsFromAfterAttributes: ParsedComments = emptyComments
    switch (tsElement.kind) {
      case TS.SyntaxKind.JsxElement:
        attributes = tsElement.openingElement.attributes
        tagName = tsElement.openingElement.tagName
        // Parse the children.
        children = mapEither((parsedChildren) => {
          return parsedChildren.map((c) => c.value)
        }, innerParse(nodeArrayToArray(tsElement.children)))
        // Capture comments against '>' as that should follow the attributes.
        tsElement.openingElement.getChildren(sourceFile).forEach((child) => {
          if (child.kind === TS.SyntaxKind.GreaterThanToken) {
            commentsFromAfterAttributes = getComments(sourceText, child)
          }
        })

        // empty element fix: if the element contains only fragments or conditionals, remove any empty text blocks
        // that may have been introduced in between due to the code formatting
        if (isRight(children)) {
          const nonEmptyTextBlockChildren = getChildrenWithoutWhitespaceOnlyText(children.value)
          const shouldRemoveEmptyTextBlocks =
            nonEmptyTextBlockChildren.length > 0 &&
            nonEmptyTextBlockChildren.every(
              (e) => isJSXFragment(e) || isJSXConditionalExpression(e),
            )
          if (shouldRemoveEmptyTextBlocks) {
            children = right(nonEmptyTextBlockChildren)
          }
        }
        break
      case TS.SyntaxKind.JsxFragment:
        children = mapEither((parsedChildren) => {
          return parsedChildren.map((c) => c.value)
        }, innerParse(nodeArrayToArray(tsElement.children)))
        // Capture comments against '>' as that should follow the attributes.
        tsElement.openingFragment.getChildren(sourceFile).forEach((child) => {
          if (child.kind === TS.SyntaxKind.GreaterThanToken) {
            commentsFromAfterAttributes = getComments(sourceText, child)
          }
        })
        break
      case TS.SyntaxKind.JsxSelfClosingElement:
        attributes = tsElement.attributes
        tagName = tsElement.tagName
        // Capture comments against '/' as that should follow the attributes.
        tsElement.getChildren(sourceFile).forEach((child) => {
          if (child.kind === TS.SyntaxKind.SlashToken) {
            commentsFromAfterAttributes = getComments(sourceText, child)
          }
        })
        break
      default:
        const _exhaustiveCheck: never = tsElement
        throw new Error(`Unhandled element type ${JSON.stringify(tsElement)}`)
    }
    return flatMapEither((childElems) => {
      // Attributes of the current element.
      const parsedAttributes = parseElementProps(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        attributes,
        highlightBounds,
        alreadyExistingUIDs,
        commentsFromAfterAttributes.leadingComments,
        applySteganography,
      )
      // Construct the element.
      return flatMapEither((attrs) => {
        highlightBounds = mergeHighlightBounds(highlightBounds, attrs.highlightBounds)
        propsUsed.push(...attrs.propsUsed)
        definedElsewhere.push(...attrs.definedElsewhere)
        const isFragment = TS.isJsxFragment(tsElement)
        return flatMapEither((elementName) => {
          if (
            (isFragment && elementName == null) ||
            (!isFragment &&
              elementName != null &&
              isJsxNameKnown(elementName, topLevelNames, imports))
          ) {
            const childrenMinusWhitespaceOnlyTexts: Array<JSXElementChild> =
              getChildrenWithoutWhitespaceOnlyText(childElems)

            const parsedElement = createJSXElementOrFragmentAllocatingUID(
              sourceFile,
              tsElement,
              elementName,
              attrs.value,
              childrenMinusWhitespaceOnlyTexts,
              highlightBounds,
              alreadyExistingUIDs,
              imports,
            )
            // In this case, don't merge the highlight bounds as the logic within the above function may have to
            // remove an entry from the highlight bounds.
            highlightBounds = parsedElement.highlightBounds
            propsUsed.push(...parsedElement.propsUsed)
            definedElsewhere.push(...parsedElement.definedElsewhere)
            return right(parsedElement.value)
          } else {
            return left(`Unknown JSX element name: ${JSON.stringify(elementName)}`)
          }
        }, parseJSXElementName(sourceFile, tagName))
      }, parsedAttributes)
    }, children)
  }

  function produceConditionalFromExpression(
    expression: TS.ConditionalExpression,
    comments: ParsedComments,
  ): Either<string, WithParserMetadata<SuccessfullyParsedElement>> {
    function parseAttribute(
      attributeExpression: TS.Expression,
    ): Either<string, WithParserMetadata<JSExpression>> {
      return parseAttributeExpression(
        sourceFile,
        sourceText,
        filename,
        imports,
        topLevelNames,
        propsObjectName,
        attributeExpression,
        existingHighlightBounds,
        alreadyExistingUIDs,
        [],
        applySteganography,
        'outermost-expression',
      )
    }

    function elementOrWithParserMetadataIsParserMetadata<T>(
      value: SuccessfullyParsedElement | WithParserMetadata<T>,
    ): value is WithParserMetadata<T> {
      return (value as any as WithParserMetadata<T>).highlightBounds !== undefined
    }

    function parseClause(
      clauseExpression: TS.Expression,
    ): Either<string, SuccessfullyParsedElement | WithParserMetadata<JSExpression>> {
      let previouslyExistingUIDs: Set<string> = new Set(alreadyExistingUIDs)
      const elementParseResult = mapEither((arr) => arr[0], innerParse([clauseExpression]))
      if (isRight(elementParseResult)) {
        return elementParseResult
      } else {
        // `innerParse` will have modified the UIDs so these need resetting.
        alreadyExistingUIDs.clear()
        for (const uid of previouslyExistingUIDs) {
          alreadyExistingUIDs.add(uid)
        }
        return parseAttribute(clauseExpression)
      }
    }

    const innerWhenTrue = TS.isParenthesizedExpression(expression.whenTrue)
      ? expression.whenTrue.expression
      : expression.whenTrue
    const whenTrueBlock = parseClause(innerWhenTrue)
    const innerWhenFalse = TS.isParenthesizedExpression(expression.whenFalse)
      ? expression.whenFalse.expression
      : expression.whenFalse
    const whenFalseBlock = parseClause(innerWhenFalse)

    // We use the leadingComments of expression.condition to provide overrides for the conditional
    // as a whole, meaning that we can't therefore provide overrides specifically for only the
    // expression.condition part in the same way as we do with other JS Expressions. Because of this,
    // we need to create the UID here before parsing expression.condition, incase we are stealing
    // an overridden one specified in the comment attached to the expression.condition
    const originalConditionString = expression.condition.getText(sourceFile).trim() // getText does not include comments
    const { uid: conditionalUID } = makeNewUIDFromOriginatingElement(
      sourceFile,
      expression,
      null,
      [
        jsxAttributesEntry(
          'condition',
          jsExpressionValue(originalConditionString, emptyComments),
          emptyComments,
        ),
      ],
      existingHighlightBounds,
      alreadyExistingUIDs,
      comments,
      imports,
    )

    return applicative3Either<
      string,
      WithParserMetadata<JSExpression>,
      SuccessfullyParsedElement | WithParserMetadata<JSExpression>,
      SuccessfullyParsedElement | WithParserMetadata<JSExpression>,
      WithParserMetadata<SuccessfullyParsedElement>
    >(
      (condition, whenTrue, whenFalse) => {
        const conditionalExpression = jsxConditionalExpression(
          conditionalUID,
          condition.value,
          expression.condition.getText(sourceFile).trim(), // getText does not include comments
          whenTrue.value,
          whenFalse.value,
          comments,
        )
        const conditionalHighlightBounds: HighlightBoundsForUids = {
          ...buildHighlightBoundsForUids(sourceFile, expression, conditionalUID),
          ...condition.highlightBounds,
          ...(elementOrWithParserMetadataIsParserMetadata(whenTrue)
            ? whenTrue.highlightBounds
            : {}),
          ...(elementOrWithParserMetadataIsParserMetadata(whenFalse)
            ? whenFalse.highlightBounds
            : {}),
        }
        const conditionalPropsUsed = [
          ...condition.propsUsed,
          ...(elementOrWithParserMetadataIsParserMetadata(whenTrue) ? whenTrue.propsUsed : []),
          ...(elementOrWithParserMetadataIsParserMetadata(whenFalse) ? whenFalse.propsUsed : []),
        ]
        const conditionalDefinedElsewhere = [
          ...condition.definedElsewhere,
          ...(elementOrWithParserMetadataIsParserMetadata(whenTrue)
            ? whenTrue.definedElsewhere
            : []),
          ...(elementOrWithParserMetadataIsParserMetadata(whenFalse)
            ? whenFalse.definedElsewhere
            : []),
        ]
        return withParserMetadata(
          successfullyParsedElement(sourceFile, expression, conditionalExpression),
          conditionalHighlightBounds,
          conditionalPropsUsed,
          conditionalDefinedElsewhere,
        )
      },
      parseAttribute(expression.condition),
      whenTrueBlock,
      whenFalseBlock,
    )
  }

  const flattened = flatMapArray(
    (nodeToParse) => flattenOutAnnoyingContainers(sourceFile, nodeToParse),
    nodesToParse,
  )

  let result: Either<string, Array<SuccessfullyParsedElement>>
  if (flattened.length === 0) {
    result = innerParse(nodesToParse)
  } else {
    result = innerParse(flattened)
  }
  return mapEither((elements) => {
    return withParserMetadata(elements, highlightBounds, propsUsed, definedElsewhere)
  }, result)
}

export const CanvasMetadataName = 'canvasMetadata'

function isJsxNameKnown(
  name: JSXElementName,
  knownElements: Array<string>,
  imports: Imports,
): boolean {
  if (isIntrinsicElement(name)) {
    return true
  } else {
    const importsArray = Object.values(imports)
    const knownImportedNames = stripNulls(
      flatMapArray(
        (imp) => [
          imp.importedWithName,
          imp.importedAs,
          ...imp.importedFromWithin.map((i) => i.alias),
        ],
        importsArray,
      ),
    )
    const knownNames = knownElements.concat(knownImportedNames as string[])
    const result = [...knownNames, 'Fragment'].includes(name.baseVariable)
    return result
  }
}

function isReactFragmentName(name: JSXElementName, imports: Imports): boolean {
  if (imports == null) {
    return false
  }
  const possibleReactImport = imports['react']
  if (possibleReactImport == null) {
    return false
  } else {
    if (possibleReactImport.importedAs != null) {
      if (
        jsxElementNameEquals(name, jsxElementName(possibleReactImport.importedAs, ['Fragment']))
      ) {
        return true
      }
    }
    if (possibleReactImport.importedWithName != null) {
      if (
        jsxElementNameEquals(
          name,
          jsxElementName(possibleReactImport.importedWithName, ['Fragment']),
        )
      ) {
        return true
      }
    }
    const fromWithin = possibleReactImport.importedFromWithin.find(
      (within) => within.name === 'Fragment',
    )
    if (fromWithin != null) {
      if (jsxElementNameEquals(name, jsxElementName(fromWithin.alias, []))) {
        return true
      }
    }
    return false
  }
}

export function flattenOutAnnoyingContainers(
  sourceFile: TS.SourceFile,
  node: TS.Node,
): Array<TS.Node> {
  let nodesToReturn: Array<TS.Node> = []
  if (node.kind === TS.SyntaxKind.SyntaxList || node.kind === TS.SyntaxKind.Block) {
    nodesToReturn = node.getChildren(sourceFile)
  } else if (TS.isParenthesizedExpression(node)) {
    nodesToReturn = [node.expression]
  } else if (TS.isReturnStatement(node)) {
    nodesToReturn = maybeToArray(node.expression)
  } else if (TS.isNewExpression(node)) {
    nodesToReturn = [node.expression]
  } else {
    // Escape out here.
    return [node]
  }
  return flatMapArray(
    (nodeToReturn) => flattenOutAnnoyingContainers(sourceFile, nodeToReturn),
    nodesToReturn,
  )
}

export function parseArbitraryNodes(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  arbitraryNodes: Array<TS.Node>,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  rootLevel: boolean,
  trailingCode: string,
  useFullText: boolean,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<ArbitraryJSBlock> | null> {
  const expressionsAndTexts = arbitraryNodes.map((node) => {
    return createExpressionAndText(
      node,
      useFullText ? node.getFullText(sourceFile) : node.getText(sourceFile),
      useFullText ? node.getFullStart() : node.getStart(sourceFile, false),
      node.getEnd(),
    )
  })
  return parseOtherJavaScript(
    sourceFile,
    sourceText,
    filename,
    expressionsAndTexts,
    imports,
    topLevelNames,
    propsObjectName,
    existingHighlightBounds,
    alreadyExistingUIDs,
    trailingCode,
    applySteganography,
    'parse-statements',
    (
      code,
      definedWithin,
      definedElsewhere,
      fileSourceNode,
      parsedElementsWithin,
      params,
      statements,
      nodes,
    ) => {
      // No need to create an arbitrary block that basically contains no code.
      if (code.trim() === '') {
        return right(null)
      }

      const { map } = fileSourceNode.toStringWithSourceMap({ file: filename })
      const rawMap = map.toJSON()

      const transpileEither = transpileJavascript(
        sourceFile.fileName,
        sourceFile.text,
        fileSourceNode,
        // Separately some logic was fixed which caused these elements within to maintain their assigned UIDs.
        // As that logic would then find the elements within, the generated code changed to using `utopiaCanvasJSXLookup`,
        // which resulted in the `data-uid` and `data-path` being created as if they were generated elements.
        // In those cases that is incorrect as they are just regularly used elements which were in a class component (for example).
        rootLevel ? [] : parsedElementsWithin,
        applySteganography,
      )
      const dataUIDFixed = insertDataUIDsIntoCode(
        filename,
        sourceText,
        code,
        rawMap,
        parsedElementsWithin,
        rootLevel,
        sourceFile.fileName,
      )
      return applicative2Either(
        (transpileResult, dataUIDFixResult) => {
          const transpiled = `return ${transpileResult.code}`
          // FIXME Pass in the dataUIDFixResult below
          return createArbitraryJSBlock(
            sourceFile,
            params,
            code,
            transpiled,
            definedWithin,
            [
              ...definedElsewhere,
              JSX_CANVAS_LOOKUP_FUNCTION_NAME,
              BLOCK_RAN_TO_END_FUNCTION_NAME,
              EARLY_RETURN_RESULT_FUNCTION_NAME,
              EARLY_RETURN_VOID_FUNCTION_NAME,
            ],
            transpileResult.sourceMap,
            inPositionToElementsWithin(parsedElementsWithin),
            alreadyExistingUIDs,
            statements,
            nodes,
          )
        },
        transpileEither,
        dataUIDFixed,
      )
    },
  )
}

export interface FunctionContents {
  arbitraryJSBlock: ArbitraryJSBlock | null
  blockOrExpression: BlockOrExpression
  elements: Array<SuccessfullyParsedElement>
  returnStatementComments: ParsedComments
}

function functionContents(
  jsBlock: ArbitraryJSBlock | null,
  blockOrExpression: BlockOrExpression,
  elements: Array<SuccessfullyParsedElement>,
  returnStatementComments: ParsedComments,
): FunctionContents {
  return {
    arbitraryJSBlock: jsBlock,
    blockOrExpression: blockOrExpression,
    elements: elements,
    returnStatementComments: returnStatementComments,
  }
}

export function expressionTypeForExpression(expression: TS.Expression): BlockOrExpression {
  return TS.isParenthesizedExpression(expression) ? 'parenthesized-expression' : 'expression'
}

export function liftParsedElementsIntoFunctionContents(
  blockOrExpression: BlockOrExpression,
  parsedElements: ParseElementsResult,
): Either<string, WithParserMetadata<FunctionContents>> {
  return mapEither((parsed) => {
    return withParserMetadata(
      functionContents(null, blockOrExpression, parsed.value, emptyComments),
      parsed.highlightBounds,
      parsed.propsUsed,
      parsed.definedElsewhere,
    )
  }, parsedElements)
}

export function parseOutFunctionContents(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  params: Array<Param>,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  arrowFunctionBody: TS.ConciseBody,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  applySteganography: SteganographyMode,
): Either<string, WithParserMetadata<FunctionContents>> {
  let highlightBounds = existingHighlightBounds
  if (TS.isBlock(arrowFunctionBody)) {
    if (arrowFunctionBody.statements.length === 0) {
      return left('No body for component.')
    } else {
      const possibleBlockExpressions: TS.Node[] = dropLast(
        nodeArrayToArray(arrowFunctionBody.statements),
      )
      const possibleElement = arrowFunctionBody.statements[arrowFunctionBody.statements.length - 1]!
      let jsBlock: ArbitraryJSBlock | null
      let propsUsed: Array<string> = []
      let definedElsewhere: Array<string> = []

      const returnStatementComments = parsedComments(
        [],
        getTrailingComments(sourceText, possibleElement),
      )
      const returnStatementPrefixCode = extractPrefixedCode(possibleElement, sourceFile)
      if (possibleBlockExpressions.length > 0) {
        const parseResult = parseArbitraryNodes(
          sourceFile,
          sourceText,
          filename,
          possibleBlockExpressions,
          imports,
          topLevelNames,
          propsObjectName,
          highlightBounds,
          alreadyExistingUIDs,
          false,
          returnStatementPrefixCode,
          true,
          applySteganography,
        )
        if (isLeft(parseResult)) {
          return parseResult
        } else {
          if (parseResult.value == null) {
            jsBlock = null
          } else {
            highlightBounds = mergeHighlightBounds(
              highlightBounds,
              parseResult.value.highlightBounds,
            )
            jsBlock = parseResult.value.value
            propsUsed = parseResult.value.propsUsed
            definedElsewhere = parseResult.value.definedElsewhere
          }
        }
      } else {
        jsBlock = null
      }

      let declared: Array<string> = [...topLevelNames]
      if (jsBlock != null) {
        declared.push(...jsBlock.definedWithin)
      }

      const parsedElements = parseOutJSXElements(
        sourceFile,
        sourceText,
        filename,
        [possibleElement],
        imports,
        declared,
        propsObjectName,
        highlightBounds,
        alreadyExistingUIDs,
        applySteganography,
      )
      return foldEither(
        () => {
          // If we aren't able to parse out the individual JSX elements (because they don't form part of a simple return statement)
          // we attempt to parse the entire function body as arbitrary JS
          const parsedAsArbitrary = parseJSExpressionMapOrOtherJavascript(
            sourceFile,
            sourceText,
            filename,
            imports,
            topLevelNames,
            propsObjectName,
            possibleElement,
            highlightBounds,
            alreadyExistingUIDs,
            applySteganography,
          )

          return bimapEither(
            (failure) => failure,
            (success) => {
              highlightBounds = mergeHighlightBounds(highlightBounds, success.highlightBounds)
              const elem = successfullyParsedElement(sourceFile, possibleElement, success.value)
              return withParserMetadata(
                functionContents(jsBlock, 'block', [elem], returnStatementComments),
                highlightBounds,
                propsUsed.concat(success.propsUsed),
                definedElsewhere.concat(success.definedElsewhere),
              )
            },
            parsedAsArbitrary,
          )
        },
        (parsed) => {
          highlightBounds = mergeHighlightBounds(highlightBounds, parsed.highlightBounds)
          return right(
            withParserMetadata(
              functionContents(jsBlock, 'block', parsed.value, returnStatementComments),
              highlightBounds,
              propsUsed.concat(parsed.propsUsed),
              definedElsewhere.concat(parsed.definedElsewhere),
            ),
          )
        },
        parsedElements,
      )
    }
  } else {
    const parsedElements = parseOutJSXElements(
      sourceFile,
      sourceText,
      filename,
      [arrowFunctionBody],
      imports,
      topLevelNames,
      propsObjectName,
      highlightBounds,
      alreadyExistingUIDs,
      applySteganography,
    )
    return liftParsedElementsIntoFunctionContents(
      expressionTypeForExpression(arrowFunctionBody),
      parsedElements,
    )
  }
}

export function extractPrefixedCode(node: TS.Node, sourceFile: TS.SourceFile): string {
  // Attempt to capture everything between this and the last node
  const nodeText = node.getText(sourceFile) ?? ''
  const nodeFullText = node.getFullText(sourceFile) ?? ''
  const prefixedText = nodeFullText.slice(0, nodeFullText.lastIndexOf(nodeText))
  return prefixedText
}
