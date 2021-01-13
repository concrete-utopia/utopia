import * as TS from 'typescript'
import { SourceNode } from 'source-map'
import {
  addUniquely,
  dropLast,
  flatMapArray,
  stripNulls,
  traverseArray,
} from '../../shared/array-utils'
import { intrinsicHTMLElementNamesAsStrings } from '../../shared/dom-utils'
import {
  applicative2Either,
  bimapEither,
  Either,
  flatMapEither,
  foldEither,
  forEachRight,
  isLeft,
  left,
  mapEither,
  right,
  traverseEither,
} from '../../shared/either'
import {
  ArbitraryJSBlock,
  arbitraryJSBlock,
  isJSXArbitraryBlock,
  isJSXAttributeValue,
  isJSXElement,
  isJSXTextBlock,
  JSXArbitraryBlock,
  jsxArbitraryBlock,
  JSXArrayElement,
  jsxArraySpread,
  jsxArrayValue,
  JSXAttribute,
  jsxAttributeFunctionCall,
  jsxAttributeNestedArray,
  jsxAttributeNestedObject,
  jsxAttributeOtherJavaScript,
  JSXAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributeValue,
  jsxElement,
  JSXElementChild,
  JSXElementChildren,
  JSXElementName,
  jsxElementName,
  JSXProperty,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  jsxTextBlock,
  ElementsWithin,
  jsxFragment,
  jsxElementNameEquals,
  isIntrinsicElement,
  clearAttributesUniqueIDs,
  clearAttributesSourceMaps,
  Comment,
  WithComments,
  BlockOrExpression,
} from '../../shared/element-template'
import { maybeToArray, forceNotNull } from '../../shared/optional-utils'
import {
  HighlightBounds,
  Imports,
  PropertyPath,
  PropertyPathPart,
  HighlightBoundsForUids,
} from '../../shared/project-file-types'
import {
  generateConsistentUID,
  generateUID,
  getUtopiaIDFromJSXElement,
  parseUID,
} from '../../shared/uid-utils'
import { fastForEach, RETURN_TO_PREPEND } from '../../shared/utils'
import {
  transpileJavascriptFromCode,
  transpileJavascript,
  insertDataUIDsIntoCode,
} from './parser-printer-transpiling'
import * as PP from '../../shared/property-path'
import {
  prependToSourceString,
  ElementsWithinInPosition,
  JSX_CANVAS_LOOKUP_FUNCTION_NAME,
} from './parser-printer-utils'
import * as Hash from 'object-hash'
import {
  emptyComments,
  getComments,
  mergeParsedComments,
  parsedComments,
  ParsedComments,
} from './parser-printer-comments'

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

export function isExported(node: TS.Node): boolean {
  if (TS.isExportAssignment(node)) {
    return true
  } else if (node.modifiers == null) {
    return false
  } else {
    return node.modifiers.some((modifier) => modifier.kind === TS.SyntaxKind.ExportKeyword)
  }
}

export function isDefaultExport(node: TS.Node): boolean {
  if (node.modifiers == null) {
    return false
  } else {
    return node.modifiers.some((modifier) => modifier.kind === TS.SyntaxKind.DefaultKeyword)
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
): Either<string, WithParserMetadata<JSXAttribute>> {
  let arrayContents: Array<JSXArrayElement> = []
  let simpleArrayContents: Array<any> = []
  let highlightBounds = existingHighlightBounds
  let propsUsed: Array<string> = []
  let definedElsewhere: Array<string> = []
  for (const literalElement of literal.elements) {
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
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        highlightBounds = subExpression.value.highlightBounds
        propsUsed.push(...subExpression.value.propsUsed)
        definedElsewhere.push(...subExpression.value.definedElsewhere)
        arrayContents.push(jsxArraySpread(subExpression.value.value, emptyComments))
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
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        highlightBounds = subExpression.value.highlightBounds
        propsUsed.push(...subExpression.value.propsUsed)
        definedElsewhere.push(...subExpression.value.definedElsewhere)
        const subExpressionValue: JSXAttribute = subExpression.value.value
        if (isJSXAttributeValue(subExpressionValue)) {
          simpleArrayContents.push(subExpressionValue.value)
        }
        arrayContents.push(jsxArrayValue(subExpressionValue, emptyComments))
      }
    }
  }
  // If every value is a JSXAttributeValue, we can simplify.
  if (simpleArrayContents.length === arrayContents.length) {
    return right(
      withParserMetadata(
        jsxAttributeValue(simpleArrayContents, emptyComments),
        highlightBounds,
        propsUsed,
        definedElsewhere,
      ),
    )
  } else {
    return right(
      withParserMetadata(
        jsxAttributeNestedArray(arrayContents, emptyComments),
        highlightBounds,
        propsUsed,
        definedElsewhere,
      ),
    )
  }
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
): Either<string, WithParserMetadata<JSXAttribute>> {
  let contents: Array<JSXProperty> = []
  let simpleContents: {
    [key: string]: any
  } = {}
  let simpleContentsCount = 0
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
    const propComments = getComments(sourceText, literalProp)
    if (TS.isPropertyAssignment(literalProp) || TS.isShorthandPropertyAssignment(literalProp)) {
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
        firstProp ? openBraceComments.trailingComments : [],
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        const possibleKey = getPropertyNameText(literalProp.name, sourceFile)
        if (isLeft(possibleKey)) {
          return possibleKey
        } else {
          const key = possibleKey.value
          const subExpressionValue: JSXAttribute = subExpression.value.value
          contents.push(jsxPropertyAssignment(key, subExpressionValue, emptyComments))
          highlightBounds = subExpression.value.highlightBounds
          propsUsed.push(...subExpression.value.propsUsed)
          definedElsewhere.push(...subExpression.value.definedElsewhere)
          if (isJSXAttributeValue(subExpressionValue)) {
            simpleContents[key] = subExpressionValue.value
            simpleContentsCount++
          }
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
      )
      if (isLeft(subExpression)) {
        return subExpression
      } else {
        const subExpressionValue = subExpression.value.value
        contents.push(jsxSpreadAssignment(subExpressionValue, emptyComments))
        highlightBounds = subExpression.value.highlightBounds
        propsUsed.push(...subExpression.value.propsUsed)
        definedElsewhere.push(...subExpression.value.definedElsewhere)
      }
    }

    // First prop reset after everything has been handled.
    firstProp = false
  }
  // If every value is a JSXAttributeValue, we can simplify.
  if (contents.length === simpleContentsCount) {
    return right(
      withParserMetadata(
        jsxAttributeValue(simpleContents, emptyComments),
        highlightBounds,
        propsUsed,
        definedElsewhere,
      ),
    )
  } else {
    return right(
      withParserMetadata(
        jsxAttributeNestedObject(contents, emptyComments),
        highlightBounds,
        propsUsed,
        definedElsewhere,
      ),
    )
  }
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
        property: PP.create(pathSoFar),
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
  let nodes: Array<typeof SourceNode> = []
  let currentLine = startLine
  let currentCol = startChar
  let bufferStartLine = currentLine
  let bufferStartChar = currentCol
  let currentStringBuffer = ''
  let exportFound = false
  let defaultFound = false
  let lastKeywordWasExport = false
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
  for (let i = 0; i < sourceCode.length; i++) {
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
  fullText: string
  startPos: number
}

function createExpressionAndText<E extends TS.Node>(
  expression: E | undefined,
  text: string,
  fullText: string,
  startPos: number,
): ExpressionAndText<E> {
  return {
    expression: expression,
    text: text,
    fullText: fullText,
    startPos: startPos,
  }
}

function parseOtherJavaScript<E extends TS.Node, T>(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  expressionsAndTexts: Array<ExpressionAndText<E>>,
  imports: Imports,
  topLevelNames: Array<string>,
  initialPropsObjectName: string | null,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  create: (
    code: string,
    fullCode: string,
    definedWithin: Array<string>,
    definedElsewhere: Array<string>,
    fileNode: typeof SourceNode,
    parsedElementsWithin: ElementsWithinInPosition,
  ) => Either<string, T>,
): Either<string, WithParserMetadata<T>> {
  if (expressionsAndTexts.length === 0) {
    throw new Error('Unable to deal with a collection of zero expressions.')
  } else {
    let startLineShift: number = 0
    let startColumnShift: number = 0
    fastForEach(expressionsAndTexts, (expressionAndText) => {
      if (expressionAndText.expression != null) {
        const startPosition = TS.getLineAndCharacterOfPosition(
          sourceFile,
          expressionAndText.startPos,
        )
        const line = startPosition.line - 1
        const column = startPosition.character - 1
        if (line > startLineShift) {
          startLineShift = line
          startColumnShift = column
        } else if (line === startLineShift) {
          if (column > startColumnShift) {
            startColumnShift = column
          }
        }
      }
    })

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
      if (TS.isIdentifier(name)) {
        pushToDefinedWithinIfNotThere(name.getText(sourceFile))
      } else if (TS.isObjectBindingPattern(name)) {
        for (const element of name.elements) {
          addBindingNameToDefinedWithin(element.name)
        }
      } else if (TS.isArrayBindingPattern(name)) {
        for (const element of name.elements) {
          if (TS.isBindingElement(element)) {
            addBindingNameToDefinedWithin(element.name)
          }
        }
      }
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
          if (!intrinsicHTMLElementNamesAsStrings.includes(nameToAdd)) {
            pushToDefinedElsewhereIfNotThere(inScope, nameToAdd)
            return true
          }
        } else {
          pushToDefinedElsewhereIfNotThere(inScope, nameToAdd)
          return true
        }
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
            highlightBounds = success.highlightBounds

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
        if (TS.isFunctionDeclaration(statement) && statement.name != null) {
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
      } else if (TS.isArrowFunction(node) || TS.isFunctionDeclaration(node)) {
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
              fastForEach(node.properties, (p) => addIfDefinedElsewhere(scope, p, false))
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
            } else if (TS.isVoidExpression(node)) {
              addIfDefinedElsewhere(scope, node.expression, false)
            } else if (TS.isYieldExpression(node)) {
              if (node.expression != null) {
                addIfDefinedElsewhere(scope, node.expression, false)
              }
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
    let expressionsFullText: Array<string> = []
    let expressionsNodes: Array<typeof SourceNode> = []
    for (const expressionAndText of expressionsAndTexts) {
      const expression = expressionAndText.expression
      if (expression != null) {
        addIfDefinedElsewhere([], expression, false)
        const expressionText = expressionAndText.text
        const expressionFullText = expressionAndText.fullText
        if (expressionFullText.length > 0) {
          const { line, character } = TS.getLineAndCharacterOfPosition(sourceFile, expression.pos)
          const expressionNode = turnCodeSnippetIntoSourceMapNodes(
            sourceFile.fileName,
            line,
            character,
            expressionAndText.fullText,
            isExported(expression),
          )
          expressionsNodes.push(expressionNode)
        }
        expressionsText.push(expressionText)
        expressionsFullText.push(expressionFullText) //removeLeadingNewlineChar(expressionFullText))
        if (TS.isFunctionDeclaration(expression)) {
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

    // Helpfully it appears that in JSX elements the start and end are
    // offset by 1, meaning that if we use them to get the text
    // the string is total nonsense.
    const code = expressionsText.join('\n')
    const fullCode = expressionsFullText.join('') // ('\n')

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

    return mapEither(
      (created) => withParserMetadata(created, highlightBounds, propsUsed, definedElsewhere),
      create(code, fullCode, definedWithin, definedElsewhere, fileNode, parsedElementsWithin),
    )
  }
}

export function parseAttributeOtherJavaScript(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  expression: TS.Expression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
): Either<string, WithParserMetadata<JSXAttributeOtherJavaScript>> {
  const expressionAndText = createExpressionAndText(
    expression,
    expression.getText(sourceFile),
    expression.getFullText(sourceFile),
    expression.getStart(sourceFile, false),
  )
  return parseOtherJavaScript(
    sourceFile,
    sourceText,
    filename,
    [expressionAndText],
    imports,
    topLevelNames,
    propsObjectName,
    existingHighlightBounds,
    alreadyExistingUIDs,
    (_code, fullCode, _definedWithin, definedElsewhere, fileSourceNode, _parsedElementsWithin) => {
      const { code: codeFromFile, map } = fileSourceNode.toStringWithSourceMap({ file: filename })
      const rawMap = JSON.parse(map.toString())
      const transpileEither = transpileJavascriptFromCode(
        sourceFile.fileName,
        sourceFile.text,
        codeFromFile,
        rawMap,
        [],
        true,
      )
      return mapEither((transpileResult) => {
        const prependedWithReturn = prependToSourceString(
          sourceFile.fileName,
          sourceFile.text,
          transpileResult.code,
          transpileResult.sourceMap,
          RETURN_TO_PREPEND,
          '',
        )
        return jsxAttributeOtherJavaScript(
          fullCode,
          prependedWithReturn.code,
          definedElsewhere,
          prependedWithReturn.sourceMap,
        )
      }, transpileEither)
    },
  )
}

function parseJSXArbitraryBlock(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  jsxExpression: TS.JsxExpression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
): Either<string, WithParserMetadata<JSXArbitraryBlock>> {
  const expressionFullText = jsxExpression.getFullText(sourceFile)
  // Remove the braces around the expression and trim off the whitespace within those.
  const codeWithComments = expressionFullText.slice(1, -1)
  const expressionAndText = createExpressionAndText(
    jsxExpression.expression,
    codeWithComments,
    jsxExpression.expression == null ? '' : jsxExpression.expression.getFullText(sourceFile),
    jsxExpression.getFullStart() + 1,
  )

  return parseOtherJavaScript(
    sourceFile,
    sourceText,
    filename,
    [expressionAndText],
    imports,
    topLevelNames,
    propsObjectName,
    existingHighlightBounds,
    alreadyExistingUIDs,
    (_code, fullCode, _definedWithin, definedElsewhere, _fileSourceNode, parsedElementsWithin) => {
      if (fullCode === '') {
        return right(
          jsxArbitraryBlock(
            fullCode,
            '',
            'return undefined',
            definedElsewhere,
            null,
            inPositionToElementsWithin(parsedElementsWithin),
          ),
        )
      } else {
        const dataUIDFixed = insertDataUIDsIntoCode(
          codeWithComments,
          parsedElementsWithin,
          true,
          false,
          sourceFile.fileName,
        )
        return flatMapEither((dataUIDFixResult) => {
          const transpileEither = transpileJavascriptFromCode(
            sourceFile.fileName,
            sourceFile.text,
            dataUIDFixResult.code,
            dataUIDFixResult.sourceMap,
            parsedElementsWithin,
            true,
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
            return jsxArbitraryBlock(
              codeWithComments,
              dataUIDFixResult.code,
              returnPrepended.code,
              innerDefinedElsewhere,
              returnPrepended.sourceMap,
              inPositionToElementsWithin(parsedElementsWithin),
            )
          }, transpileEither)
        }, dataUIDFixed)
      }
    },
  )
}

function parseAttributeExpression(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  expression: TS.Expression,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
  trailingCommentsFromContainer: Array<Comment>,
): Either<string, WithParserMetadata<JSXAttribute>> {
  let comments = getComments(sourceText, expression)
  if (trailingCommentsFromContainer.length > 0) {
    comments = parsedComments(
      [...trailingCommentsFromContainer, ...comments.leadingComments],
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
    )
  } else if (TS.isCallExpression(expression)) {
    // Parse the case that an attribute invokes a special case function.
    // Then we parse out the parameters passed to the function.
    // Commented out as the style of this is likely to be re-used but we're not using it right now.
    if (TS.isPropertyAccessExpression(expression.expression)) {
      // if (TS.isIdentifier(expression.expression)) {
      const propertyAccess = expression.expression
      const leftHandSide = propertyAccess.expression
      const identifier = propertyAccess.name
      if (leftHandSide.getText(sourceFile) === 'UtopiaUtils') {
        let highlightBounds = existingHighlightBounds
        let parsedArgumentAttributes: Array<JSXAttribute> = []
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
          )
          if (isLeft(parsedArgument)) {
            return left(`Error parsing function expression: ${parsedArgument.value}`)
          } else {
            parsedArgumentAttributes.push(parsedArgument.value.value)
            highlightBounds = parsedArgument.value.highlightBounds
            propsUsed.push(...parsedArgument.value.propsUsed)
            definedElsewhere.push(...parsedArgument.value.definedElsewhere)
          }
        }
        return right(
          withParserMetadata(
            jsxAttributeFunctionCall(identifier.getText(sourceFile), parsedArgumentAttributes),
            highlightBounds,
            propsUsed,
            definedElsewhere,
          ),
        )
      }
    }
    return parseAttributeOtherJavaScript(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
    )
  } else if (
    TS.isElementAccessExpression(expression) ||
    TS.isPropertyAccessExpression(expression)
  ) {
    return parseAttributeOtherJavaScript(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
    )
  } else if (
    TS.isIdentifier(expression) &&
    expression.originalKeywordKind === TS.SyntaxKind.UndefinedKeyword
  ) {
    return right(
      withParserMetadata(jsxAttributeValue(undefined, comments), existingHighlightBounds, [], []),
    )
  } else if (TS.isNumericLiteral(expression)) {
    return right(
      withParserMetadata(
        jsxAttributeValue(Number.parseFloat(expression.getText(sourceFile)), comments),
        existingHighlightBounds,
        [],
        [],
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
    )
  } else if (TS.isPrefixUnaryExpression(expression)) {
    // Cater for negative numbers, because of course they're done in a weird way.
    const operator = expression.operator
    if (operator === TS.SyntaxKind.MinusToken) {
      const operand = expression.operand
      if (TS.isNumericLiteral(operand)) {
        return right(
          withParserMetadata(
            jsxAttributeValue(Number.parseFloat(operand.getText(sourceFile)) * -1, comments),
            existingHighlightBounds,
            [],
            [],
          ),
        )
      }
    }
    return parseAttributeOtherJavaScript(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      expression,
      existingHighlightBounds,
      alreadyExistingUIDs,
    )
  } else if (TS.isStringLiteral(expression)) {
    return right(
      withParserMetadata(
        jsxAttributeValue(expression.text, comments),
        existingHighlightBounds,
        [],
        [],
      ),
    )
  } else {
    switch (expression.kind) {
      case TS.SyntaxKind.TrueKeyword:
        return right(
          withParserMetadata(jsxAttributeValue(true, comments), existingHighlightBounds, [], []),
        )
      case TS.SyntaxKind.FalseKeyword:
        return right(
          withParserMetadata(jsxAttributeValue(false, comments), existingHighlightBounds, [], []),
        )
      case TS.SyntaxKind.NullKeyword:
        return right(
          withParserMetadata(jsxAttributeValue(null, comments), existingHighlightBounds, [], []),
        )
      default:
        return parseAttributeOtherJavaScript(
          sourceFile,
          sourceText,
          filename,
          imports,
          topLevelNames,
          propsObjectName,
          expression,
          existingHighlightBounds,
          alreadyExistingUIDs,
        )
    }
  }
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
): Either<string, WithParserMetadata<JSXAttribute>> {
  if (TS.isStringLiteral(initializer)) {
    const comments = getComments(sourceText, initializer)
    return right(
      withParserMetadata(
        jsxAttributeValue(initializer.text, comments),
        existingHighlightBounds,
        [],
        [],
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
      return right(
        withParserMetadata(
          jsxAttributeOtherJavaScript('null', 'null', [], null),
          existingHighlightBounds,
          [],
          [],
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
): Either<string, WithParserMetadata<JSXAttributes>> {
  let result: JSXAttributes = {}
  let highlightBounds = existingHighlightBounds
  let propsUsed = []
  let definedElsewhere = []
  for (const prop of attributes.properties) {
    if (TS.isJsxAttribute(prop)) {
      if (prop.initializer == null) {
        result[prop.name.getText(sourceFile)] = jsxAttributeValue(true, emptyComments)
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
        )
        if (isLeft(attributeResult)) {
          return attributeResult
        } else {
          result[prop.name.getText(sourceFile)] = attributeResult.value.value
          highlightBounds = attributeResult.value.highlightBounds
          propsUsed.push(...attributeResult.value.propsUsed)
          definedElsewhere.push(...attributeResult.value.definedElsewhere)
        }
      }
    } else {
      return left(`Invalid attribute found.`)
    }
  }
  return right(withParserMetadata(result, highlightBounds, propsUsed, definedElsewhere))
}

type TSTextOrExpression = TS.JsxText | TS.JsxExpression
type TSJSXElement = TS.JsxElement | TS.JsxSelfClosingElement
type ElementsToParse = Array<
  TSJSXElement | TSTextOrExpression | TS.JsxOpeningFragment | TS.JsxClosingFragment | TS.JsxFragment
>

function pullOutElementsToParse(nodes: Array<TS.Node>): Either<string, ElementsToParse> {
  let result: ElementsToParse = []
  for (let index = 0; index < nodes.length; index++) {
    const node = nodes[index]
    if (
      TS.isJsxElement(node) ||
      TS.isJsxSelfClosingElement(node) ||
      TS.isJsxText(node) ||
      TS.isJsxExpression(node) ||
      TS.isJsxFragment(node)
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
  return element === undefined || isJSXElement(element.value) || isJSXArbitraryBlock(element.value)
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
): Either<string, JSXElementName> {
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
  boundingElement: TS.Node,
  uid: string,
): HighlightBounds {
  const startPosition = TS.getLineAndCharacterOfPosition(
    sourceFile,
    boundingElement.getStart(sourceFile, false),
  )
  const endPosition = TS.getLineAndCharacterOfPosition(sourceFile, boundingElement.getEnd())
  return {
    startCol: startPosition.character,
    startLine: startPosition.line,
    endCol: endPosition.character,
    endLine: endPosition.line,
    uid: uid,
  }
}

function forciblyUpdateDataUID(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  name: JSXElementName | string,
  props: JSXAttributes,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<JSXAttributes> {
  const hash = Hash({
    name: name,
    props: clearAttributesSourceMaps(clearAttributesUniqueIDs(props)),
  })
  const uid = generateConsistentUID(alreadyExistingUIDs, hash)
  alreadyExistingUIDs.add(uid)
  const updatedProps = {
    ...props,
    ['data-uid']: jsxAttributeValue(uid, emptyComments),
  }
  return withParserMetadata(
    updatedProps,
    {
      ...existingHighlightBounds,
      [uid]: buildHighlightBounds(sourceFile, originatingElement, uid),
    },
    [],
    [],
  )
}

function createJSXElementAllocatingUID(
  sourceFile: TS.SourceFile,
  originatingElement: TS.Node,
  name: JSXElementName | string,
  props: JSXAttributes,
  children: JSXElementChildren,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
): WithParserMetadata<SuccessfullyParsedElement> {
  const dataUIDAttribute = parseUID(props)
  const updatedProps = foldEither(
    (_) =>
      forciblyUpdateDataUID(
        sourceFile,
        originatingElement,
        name,
        props,
        existingHighlightBounds,
        alreadyExistingUIDs,
      ),
    (uid) => {
      // This implies a duplicate UID, so we should replace it.
      if (uid in existingHighlightBounds) {
        return forciblyUpdateDataUID(
          sourceFile,
          originatingElement,
          name,
          props,
          existingHighlightBounds,
          alreadyExistingUIDs,
        )
      } else {
        return withParserMetadata(
          props,
          {
            ...existingHighlightBounds,
            [uid]: buildHighlightBounds(sourceFile, originatingElement, uid),
          },
          [],
          [],
        )
      }
    },
    dataUIDAttribute,
  )
  const startPosition = TS.getLineAndCharacterOfPosition(
    sourceFile,
    originatingElement.getStart(sourceFile, false),
  )
  return withParserMetadata(
    {
      value: jsxElement(name, updatedProps.value, children),
      startLine: startPosition.line,
      startColumn: startPosition.character,
    },
    updatedProps.highlightBounds,
    [],
    [],
  )
}

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
): ParseElementsResult {
  let highlightBounds = existingHighlightBounds
  let propsUsed: Array<string> = []
  let definedElsewhere: Array<string> = []
  function innerParse(nodes: Array<TS.Node>): Either<string, Array<SuccessfullyParsedElement>> {
    // First parse to extract the nodes we really want into a sensible form
    // and fail if there's anything unexpected.
    return flatMapEither((toParse) => {
      // Handle the two different cases of either an element or a bunch of JSX inner content.
      let parsedNodes: Array<SuccessfullyParsedElement> = []
      let fragmentDepth: number = 0
      let fragmentChildren: { [key: number]: Array<JSXElementChild> } = {}
      let fragmentStart: { [key: number]: TS.JsxOpeningFragment } = {}

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

      for (const elem of toParse) {
        switch (elem.kind) {
          case TS.SyntaxKind.JsxFragment: {
            const possibleFragment = produceFragmentFromJsxFragment(elem)
            if (isLeft(possibleFragment)) {
              return possibleFragment
            } else {
              addParsedElement(possibleFragment.value)
            }
            break
          }
          case TS.SyntaxKind.JsxElement: {
            const possibleElement = produceElementFromTSElement(elem)
            if (isLeft(possibleElement)) {
              return possibleElement
            } else {
              const parsedElement = possibleElement.value.value
              if (isJSXElement(parsedElement) && isReactFragmentName(parsedElement.name, imports)) {
                addParsedElement({
                  ...possibleElement.value,
                  value: jsxFragment(parsedElement.children, true),
                })
              } else {
                addParsedElement(possibleElement.value)
              }
            }
            break
          }
          case TS.SyntaxKind.JsxSelfClosingElement: {
            const possibleElement = produceElementFromTSElement(elem)
            if (isLeft(possibleElement)) {
              return possibleElement
            } else {
              const parsedElement = possibleElement.value.value
              if (isJSXElement(parsedElement) && isReactFragmentName(parsedElement.name, imports)) {
                addParsedElement({
                  ...possibleElement.value,
                  value: jsxFragment(parsedElement.children, true),
                })
              } else {
                addParsedElement(possibleElement.value)
              }
            }
            break
          }
          case TS.SyntaxKind.JsxExpression: {
            const possibleExpression = produceArbitraryBlockFromJsxExpression(elem)
            if (isLeft(possibleExpression)) {
              return possibleExpression
            } else {
              addParsedElement(possibleExpression.value)
            }
            break
          }
          case TS.SyntaxKind.JsxText: {
            const possibleText = produceTextFromJsxText(elem)
            if (isLeft(possibleText)) {
              return possibleText
            } else {
              addParsedElement(possibleText.value)
            }
            break
          }
          case TS.SyntaxKind.JsxOpeningFragment: {
            fragmentDepth += 1
            fragmentStart[fragmentDepth] = elem
            break
          }
          case TS.SyntaxKind.JsxClosingFragment: {
            if (fragmentDepth === 0) {
              return left('Too many closed fragments.')
            } else {
              const childrenOfFragment: Array<JSXElementChild> =
                fragmentChildren[fragmentDepth] ?? []
              const start = forceNotNull(
                'Fragment start should exist.',
                fragmentStart[fragmentDepth],
              )
              delete fragmentChildren[fragmentDepth]
              delete fragmentStart[fragmentDepth]
              fragmentDepth -= 1
              addParsedElement(
                successfullyParsedElement(
                  sourceFile,
                  start,
                  jsxFragment(childrenOfFragment, false),
                ),
              )
            }
            break
          }
          default:
            const _exhaustiveCheck: never = elem
            throw new Error(`Unhandled elem type ${JSON.stringify(elem)}`)
        }
      }

      if (fragmentDepth === 0) {
        return right(clearUnnecessarySpacingElements(parsedNodes))
      } else {
        return left('Not enough closed fragments.')
      }
    }, pullOutElementsToParse(nodes))
  }

  function produceFragmentFromJsxFragment(
    fragment: TS.JsxFragment,
  ): Either<string, SuccessfullyParsedElement> {
    // Parse the children.
    const parsedChildren = mapEither((children) => {
      return children.map((c) => c.value)
    }, innerParse(nodeArrayToArray(fragment.children)))
    // Create the containing fragment.
    return mapEither((children) => {
      return successfullyParsedElement(sourceFile, fragment, jsxFragment(children, false))
    }, parsedChildren)
  }

  function produceTextFromJsxText(tsText: TS.JsxText): Either<string, SuccessfullyParsedElement> {
    return right(successfullyParsedElement(sourceFile, tsText, jsxTextBlock(tsText.text)))
  }

  function produceArbitraryBlockFromJsxExpression(
    tsExpression: TS.JsxExpression,
  ): Either<string, SuccessfullyParsedElement> {
    const result = parseJSXArbitraryBlock(
      sourceFile,
      sourceText,
      filename,
      imports,
      topLevelNames,
      propsObjectName,
      tsExpression,
      highlightBounds,
      alreadyExistingUIDs,
    )
    return bimapEither(
      (failure) => failure,
      (success) => {
        highlightBounds = success.highlightBounds
        propsUsed.push(...success.propsUsed)
        definedElsewhere.push(...success.definedElsewhere)
        return successfullyParsedElement(sourceFile, tsExpression, success.value)
      },
      result,
    )
  }

  function produceElementFromTSElement(
    tsElement: TSJSXElement,
  ): Either<string, SuccessfullyParsedElement> {
    let attributes: TS.JsxAttributes
    let tagName: TS.JsxTagNameExpression
    let children: Either<string, Array<JSXElementChild>> = right([])
    switch (tsElement.kind) {
      case TS.SyntaxKind.JsxElement:
        attributes = tsElement.openingElement.attributes
        tagName = tsElement.openingElement.tagName
        // Parse the children.
        children = mapEither((parsedChildren) => {
          return parsedChildren.map((c) => c.value)
        }, innerParse(nodeArrayToArray(tsElement.children)))
        break
      case TS.SyntaxKind.JsxSelfClosingElement:
        attributes = tsElement.attributes
        tagName = tsElement.tagName
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
      )
      // Construct the element.
      return flatMapEither((attrs) => {
        highlightBounds = attrs.highlightBounds
        propsUsed.push(...attrs.propsUsed)
        definedElsewhere.push(...attrs.definedElsewhere)
        return flatMapEither((elementName) => {
          if (isJsxNameKnown(elementName, topLevelNames, imports)) {
            const parsedElement = createJSXElementAllocatingUID(
              sourceFile,
              tsElement,
              elementName,
              attrs.value,
              childElems,
              highlightBounds,
              alreadyExistingUIDs,
            )
            highlightBounds = parsedElement.highlightBounds
            return right(parsedElement.value)
          } else {
            return left(`Unknown JSX element name: ${JSON.stringify(elementName)}`)
          }
        }, parseJSXElementName(sourceFile, tagName))
      }, parsedAttributes)
    }, children)
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
    const result = knownNames.includes(name.baseVariable)
    return result
  }
}

function isReactFragmentName(name: JSXElementName, imports: Imports): boolean {
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
  comments: ParsedComments,
  useFullCode: boolean,
): Either<string, WithParserMetadata<ArbitraryJSBlock>> {
  const expressionsAndTexts = arbitraryNodes.map((node) => {
    return createExpressionAndText(
      node,
      node.getText(sourceFile),
      node.getFullText(sourceFile),
      node.getStart(sourceFile, false),
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
    (code, fullCode, definedWithin, definedElsewhere, fileSourceNode, parsedElementsWithin) => {
      const codeToUse = useFullCode ? fullCode : code
      const definedWithinFields = definedWithin.map((within) => `${within}: ${within}`).join(', ')
      const definedWithCode = `return { ${definedWithinFields} };`

      const transpileEither = transpileJavascript(
        sourceFile.fileName,
        sourceFile.text,
        fileSourceNode,
        [],
        false,
      )
      const dataUIDFixed = insertDataUIDsIntoCode(
        codeToUse,
        parsedElementsWithin,
        false,
        rootLevel,
        sourceFile.fileName,
      )
      return applicative2Either(
        (transpileResult, dataUIDFixResult) => {
          const transpiled = `${transpileResult.code}\n${definedWithCode}`
          return arbitraryJSBlock(
            codeToUse,
            transpiled,
            definedWithin,
            definedElsewhere,
            transpileResult.sourceMap,
            comments,
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

export function extractPrefixedCode(node: TS.Node, sourceFile: TS.SourceFile): string {
  // Attempt to capture everything between this and the last node
  const nodeText = node.getText(sourceFile) || ''
  const nodeFullText = node.getFullText(sourceFile) || ''
  const fullPrefixedText = nodeFullText.slice(0, nodeFullText.lastIndexOf(nodeText))
  const prefixedText = fullPrefixedText
  return prefixedText
}

export function parseOutFunctionContents(
  sourceFile: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  propsObjectName: string | null,
  arrowFunctionBody: TS.ConciseBody,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  alreadyExistingUIDs: Set<string>,
): Either<string, WithParserMetadata<FunctionContents>> {
  let highlightBounds = existingHighlightBounds
  if (TS.isBlock(arrowFunctionBody)) {
    if (arrowFunctionBody.statements.length === 0) {
      return left('No body for component.')
    } else {
      const possibleElement = arrowFunctionBody.statements[arrowFunctionBody.statements.length - 1]!
      let jsBlock: ArbitraryJSBlock | null = null
      let propsUsed: Array<string> = []
      let definedElsewhere: Array<string> = []
      const returnStatementComments = getComments(sourceText, possibleElement)
      const returnStatementPrefixText = extractPrefixedCode(possibleElement, sourceFile)

      const possibleBlockExpressions = dropLast(nodeArrayToArray(arrowFunctionBody.statements))
      if (possibleBlockExpressions.length > 0) {
        const comments = possibleBlockExpressions.reduce(
          (working: ParsedComments, next: TS.Statement) =>
            mergeParsedComments(working, getComments(sourceText, next)),
          emptyComments,
        )

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
          comments,
          true,
        )
        if (isLeft(parseResult)) {
          return parseResult
        } else {
          let parsedJSBlock = parseResult.value.value
          parsedJSBlock.javascript = `${parsedJSBlock.javascript}${returnStatementPrefixText}`
          parsedJSBlock.transpiledJavascript = `${parsedJSBlock.transpiledJavascript}${returnStatementPrefixText}`

          highlightBounds = parseResult.value.highlightBounds
          jsBlock = parsedJSBlock
          propsUsed = parseResult.value.propsUsed
          definedElsewhere = parseResult.value.definedElsewhere
        }
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
      )
      return mapEither((parsed) => {
        return withParserMetadata(
          functionContents(jsBlock, 'block', parsed.value, returnStatementComments),
          parsed.highlightBounds,
          propsUsed.concat(parsed.propsUsed),
          definedElsewhere.concat(parsed.definedElsewhere),
        )
      }, parsedElements)
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
    )
    return liftParsedElementsIntoFunctionContents(
      expressionTypeForExpression(arrowFunctionBody),
      parsedElements,
    )
  }
}
