import * as Babel from '@babel/standalone'
import type * as BabelTraverse from '@babel/traverse'
import * as BabelTypes from '@babel/types'
import ReactSyntaxPlugin from 'babel-plugin-syntax-jsx'
import ReactTransformPlugin from 'babel-plugin-transform-react-jsx'
import type { SourceNode } from 'source-map'
import type { Either } from '../../shared/either'
import { isRight, left, right } from '../../shared/either'
import type { JSXElement, JSXElementLike } from '../../shared/element-template'
import {
  getDefinedElsewhereFromElement,
  getJSXElementLikeNameAsString,
  getJSXElementNameAsString,
  isJSXElement,
} from '../../shared/element-template'
import { getUtopiaIDFromJSXElement } from '../../shared/uid-utils'
import { fastForEach } from '../../shared/utils'
import type { RawSourceMap } from '../ts/ts-typings/RawSourceMap'
import infiniteLoopPrevention from './transform-prevent-infinite-loops'
import type { ElementsWithinInPosition, CodeWithMap } from './parser-printer-utils'
import {
  wrapCodeInAnonFunctionWithMap,
  wrapCodeInParens,
  wrapCodeInParensWithMap,
} from './parser-printer-utils'
import {
  BLOCK_RAN_TO_END_FUNCTION_NAME,
  EARLY_RETURN_RESULT_FUNCTION_NAME,
  EARLY_RETURN_VOID_FUNCTION_NAME,
  JSX_CANVAS_LOOKUP_FUNCTION_NAME,
} from '../../shared/dom-utils'
import type { SteganoTextData } from '../../shared/stegano-text'
import { cleanSteganoTextData, encodeSteganoData } from '../../shared/stegano-text'
import { SourceMapConsumer } from 'source-map'
import type { SteganographyMode } from './parser-printer'

interface TranspileResult {
  code: string
  sourceMap: RawSourceMap
}

function addIdentifier(identifiers: Array<string>, toAdd: BabelTypes.Identifier): void {
  identifiers.push(toAdd.name)
}

function getIdentifiersFromFunctionParam(
  param: BabelTypes.Identifier | BabelTypes.Pattern | BabelTypes.RestElement,
): Array<string> {
  function fromObjectPattern(objectPattern: BabelTypes.ObjectPattern): Array<string> {
    let identifiers: Array<string> = []
    for (const property of objectPattern.properties) {
      if (BabelTypes.isIdentifier(property)) {
        addIdentifier(identifiers, property)
      } else if (BabelTypes.isObjectProperty(property)) {
        if (BabelTypes.isIdentifier(property.value)) {
          addIdentifier(identifiers, property.value)
        } else if (BabelTypes.isObjectPattern(property.value)) {
          identifiers.push(...fromObjectPattern(property.value))
        }
      }
    }
    return identifiers
  }

  function fromArrayPattern(arrayPattern: BabelTypes.ArrayPattern): Array<string> {
    let identifiers: Array<string> = []
    for (const element of arrayPattern.elements) {
      if (element != null) {
        if (BabelTypes.isIdentifier(element)) {
          addIdentifier(identifiers, element)
        } else if (BabelTypes.isPattern(element)) {
          identifiers.push(...getIdentifiersFromFunctionParam(element))
        }
      }
    }
    return identifiers
  }

  function fromAssignmentPattern(assignmentPattern: BabelTypes.AssignmentPattern): Array<string> {
    if (BabelTypes.isIdentifier(assignmentPattern.left)) {
      return [assignmentPattern.left.name]
    } else if (BabelTypes.isObjectPattern(assignmentPattern.left)) {
      return fromObjectPattern(assignmentPattern.left)
    } else if (BabelTypes.isArrayPattern(assignmentPattern.left)) {
      return fromArrayPattern(assignmentPattern.left)
    } else if (BabelTypes.isMemberExpression(assignmentPattern.left)) {
      if (BabelTypes.isIdentifier(assignmentPattern.left.property)) {
        return [assignmentPattern.left.property.name]
      } else {
        return []
      }
    } else {
      return []
    }
  }

  if (BabelTypes.isIdentifier(param)) {
    return [param.name]
  } else if (BabelTypes.isPattern(param)) {
    if (BabelTypes.isAssignmentPattern(param)) {
      return fromAssignmentPattern(param)
    } else if (BabelTypes.isObjectPattern(param)) {
      return fromObjectPattern(param)
    } else if (BabelTypes.isArrayPattern(param)) {
      return fromArrayPattern(param)
    } else {
      return []
    }
  } else if (BabelTypes.isRestElement(param)) {
    // Not needed to be handled I don't believe.
    return []
  } else {
    return []
  }
}

function identifyFunctionParameters(
  path: BabelTraverse.NodePath<BabelTraverse.Node>,
): Array<string> {
  let result: Array<string> = []
  if (path.parentPath != null) {
    result.push(...identifyFunctionParameters(path.parentPath))
  }
  if (
    BabelTypes.isArrowFunctionExpression(path.node) ||
    BabelTypes.isFunctionExpression(path.node)
  ) {
    for (const param of path.node.params) {
      result.push(...getIdentifiersFromFunctionParam(param))
    }
  }
  return result
}

/**
 * Here we transform the code originating from an arbitrary JSX block.
 * The elements passed when identified in the code will be replaced
 * with calls to a function that is specially injected by the
 * canvas to return an invocation of the canvas element renderer.
 */
function babelRewriteJSExpressionCode(
  elementsWithin: ElementsWithinInPosition,
  insertCanvasRenderCall: boolean,
): () => {
  visitor: BabelTraverse.Visitor
} {
  function transformForElementWithin(
    path: BabelTraverse.NodePath<BabelTypes.JSXElement>,
    elementWithin: JSXElementLike,
    uid: string,
  ): void {
    if (insertCanvasRenderCall) {
      const functionIdentifier = BabelTypes.identifier(JSX_CANVAS_LOOKUP_FUNCTION_NAME)

      let objectExpressionProperties: Array<
        [BabelTypes.Identifier, BabelTypes.ObjectProperty['value']]
      > = []

      function addOrReplaceInExpressionProperties(
        identifier: BabelTypes.Identifier,
        value: BabelTypes.ObjectProperty['value'],
      ): void {
        objectExpressionProperties = objectExpressionProperties.filter(([entryIdentifier, _]) => {
          return entryIdentifier.name !== identifier.name
        })
        objectExpressionProperties.push([identifier, value])
      }

      const identifiersOfArrowExpressions = identifyFunctionParameters(path)
      for (const identifierOfArrowExpression of identifiersOfArrowExpressions) {
        addOrReplaceInExpressionProperties(
          BabelTypes.identifier(identifierOfArrowExpression),
          BabelTypes.identifier(identifierOfArrowExpression),
        )
      }

      const definedElsewhere = getDefinedElsewhereFromElement(elementWithin)
      for (const definedElsewhereName of definedElsewhere) {
        addOrReplaceInExpressionProperties(
          BabelTypes.identifier(definedElsewhereName),
          BabelTypes.identifier(definedElsewhereName),
        )
      }
      addOrReplaceInExpressionProperties(
        BabelTypes.identifier('callerThis'),
        BabelTypes.thisExpression(),
      )
      const callArguments = [
        BabelTypes.stringLiteral(uid),
        BabelTypes.objectExpression(
          objectExpressionProperties.map(([identifier, value]) => {
            return BabelTypes.objectProperty(identifier, value)
          }),
        ),
      ]
      // Weird cast because even though they're the same type there's a weird
      // disagreement between BabelTypes.Node and BabelTraverse.Node.
      const callExpression: BabelTraverse.Node = BabelTypes.callExpression(
        functionIdentifier,
        callArguments,
      ) as any
      path.replaceWith(callExpression)
    } else {
      const originalNode = path.node
      const originalOpeningElement = originalNode.openingElement
      const originalAttributes = originalOpeningElement.attributes
      let newAttributes: Array<BabelTypes.JSXAttribute | BabelTypes.JSXSpreadAttribute> = []
      let hasDataUID: boolean = false
      fastForEach(originalAttributes, (attribute) => {
        if (BabelTypes.isJSXAttribute(attribute)) {
          if (BabelTypes.isJSXIdentifier(attribute.name)) {
            if (attribute.name.name === 'data-uid') {
              hasDataUID = true
            }
          }
        }
        newAttributes.push(attribute)
      })

      // Don't replace this if it already has a `data-uid` attribute.
      if (!hasDataUID) {
        newAttributes.push(
          BabelTypes.jsxAttribute(
            BabelTypes.jsxIdentifier('data-uid'),
            BabelTypes.stringLiteral(uid),
          ),
        )
        const newOpeningElement = BabelTypes.jsxOpeningElement(
          originalOpeningElement.name,
          newAttributes,
          originalOpeningElement.selfClosing,
        )
        const newElement = BabelTypes.jsxElement(
          newOpeningElement,
          originalNode.closingElement,
          originalNode.children,
          originalNode.selfClosing,
        )
        path.replaceWith(newElement)
      }
    }
  }
  function handleStringLiteral(
    path: BabelTraverse.NodePath<BabelTypes.JSXElement>,
    value: unknown,
  ): void {
    if (BabelTypes.isNode(value) && BabelTypes.isStringLiteral(value)) {
      const uid = value.value
      const foundElementWithin = elementsWithin.find((e) => e.uid === uid)
      if (foundElementWithin != null) {
        transformForElementWithin(path, foundElementWithin.element, uid)
      }
    }
  }
  function tagNameForNodeName(
    nodeName:
      | BabelTypes.JSXIdentifier
      | BabelTypes.JSXMemberExpression
      | BabelTypes.JSXNamespacedName,
  ): string {
    if (BabelTypes.isJSXIdentifier(nodeName)) {
      return nodeName.name
    } else if (BabelTypes.isJSXMemberExpression(nodeName)) {
      return nodeName.property.name
    } else {
      return `${nodeName.namespace.name}.${nodeName.name}`
    }
  }
  function handleByPositionOrName(path: BabelTraverse.NodePath<BabelTypes.JSXElement>): void {
    const pathLocation: {
      line: number
      column: number
    } = path.node.loc?.start ?? { line: -1, column: -1 }

    const tagName = tagNameForNodeName(path.node.openingElement.name)

    const foundByLocation = elementsWithin.find((e) => {
      return e.startLine === pathLocation.line && e.startColumn === pathLocation.column
    })

    const foundElementWithin =
      foundByLocation ??
      elementsWithin.find((e) => {
        return tagName === getJSXElementLikeNameAsString(e.element)
      })

    if (foundElementWithin != null) {
      transformForElementWithin(
        path,
        foundElementWithin.element,
        getUtopiaIDFromJSXElement(foundElementWithin.element),
      )
    }
  }
  return () => {
    return {
      visitor: {
        JSXElement(path) {
          const node = path.node
          let foundByID: boolean = false
          for (const attribute of node.openingElement.attributes) {
            if (BabelTypes.isJSXAttribute(attribute)) {
              if (BabelTypes.isJSXIdentifier(attribute.name)) {
                if (attribute.name.name === 'data-uid') {
                  foundByID = true
                  if (BabelTypes.isJSXExpressionContainer(attribute.value)) {
                    handleStringLiteral(path, attribute.value.expression)
                  } else {
                    handleStringLiteral(path, attribute.value)
                  }
                }
              }
            }
          }
          if (!foundByID) {
            handleByPositionOrName(path)
          }
        },
      },
    }
  }
}

function rewriteBlockToCaptureEarlyReturns(source: string): () => {
  visitor: BabelTraverse.Visitor
} {
  const returnStatementVisitor: BabelTraverse.Visitor = {
    ArrowFunctionExpression(path) {
      path.skip()
    },
    FunctionExpression(path) {
      path.skip()
    },
    FunctionDeclaration(path) {
      path.skip()
    },
    ClassDeclaration(path) {
      path.skip()
    },
    ReturnStatement(path) {
      if (path.node.argument == null) {
        const newReturnArgument = BabelTypes.callExpression(
          BabelTypes.identifier(EARLY_RETURN_VOID_FUNCTION_NAME),
          [],
        )
        const newReturnStatement = BabelTypes.returnStatement(newReturnArgument)
        path.replaceWith(newReturnStatement)
      } else {
        const newReturnArgument = BabelTypes.callExpression(
          BabelTypes.identifier(EARLY_RETURN_RESULT_FUNCTION_NAME),
          [path.node.argument],
        )
        const newReturnStatement = BabelTypes.returnStatement(newReturnArgument)
        path.replaceWith(newReturnStatement)
      }
      path.skip()
    },
  }

  return () => ({
    visitor: {
      // This is the arrow function expression that we wrap around the arbitrary block
      // so that Babel will parse it without an error.
      ArrowFunctionExpression(path) {
        // Check that this is the topmost arrow function as it should be.
        const firstParentPath = path.parentPath
        const firstParent = firstParentPath.node
        if (!BabelTypes.isCallExpression(firstParent)) {
          return
        }
        const secondParentPath = firstParentPath.parentPath
        const secondParent = secondParentPath?.node
        if (!BabelTypes.isExpressionStatement(secondParent)) {
          return
        }
        const thirdParentPath = secondParentPath?.parentPath
        const thirdParent = thirdParentPath?.node
        if (!BabelTypes.isProgram(thirdParent)) {
          return
        }
        const node = path.node
        if (BabelTypes.isBlockStatement(node.body)) {
          const blockStatement = node.body
          // The value of any return statement is wrapped with the early return value,
          // as long as it's not inside an arrow function or regular function statement.
          path.traverse(returnStatementVisitor)

          // Any variables or declarations defined in this block are recorded.
          let identifiersToReturn: Array<string> = []

          function handleNodeAddingIdentifiers(nodeToProcess: BabelTypes.Node): void {
            if (BabelTypes.isVariableDeclaration(nodeToProcess)) {
              for (const declaration of nodeToProcess.declarations) {
                handleNodeAddingIdentifiers(declaration.id)
              }
            } else if (BabelTypes.isIdentifier(nodeToProcess)) {
              identifiersToReturn.push(nodeToProcess.name)
            } else if (BabelTypes.isRestElement(nodeToProcess)) {
              handleNodeAddingIdentifiers(nodeToProcess.argument)
            } else if (BabelTypes.isObjectPattern(nodeToProcess)) {
              for (const property of nodeToProcess.properties) {
                if (BabelTypes.isObjectProperty(property)) {
                  handleNodeAddingIdentifiers(property.value)
                } else if (BabelTypes.isRestElement(property)) {
                  handleNodeAddingIdentifiers(property.argument)
                }
              }
            } else if (BabelTypes.isArrayPattern(nodeToProcess)) {
              for (const element of nodeToProcess.elements) {
                if (element != null) {
                  handleNodeAddingIdentifiers(element)
                }
              }
            } else if (BabelTypes.isFunctionDeclaration(nodeToProcess)) {
              if (nodeToProcess.id != null) {
                handleNodeAddingIdentifiers(nodeToProcess.id)
              }
            } else if (BabelTypes.isClassDeclaration(nodeToProcess)) {
              if (nodeToProcess.id != null) {
                handleNodeAddingIdentifiers(nodeToProcess.id)
              }
            }
          }

          for (const bodyPart of blockStatement.body) {
            handleNodeAddingIdentifiers(bodyPart)
          }

          // An additional return statement is added to the end of the block,
          // which includes all the variables previously recorded.
          const additionalReturnStatement = BabelTypes.returnStatement(
            BabelTypes.callExpression(BabelTypes.identifier(BLOCK_RAN_TO_END_FUNCTION_NAME), [
              BabelTypes.objectExpression(
                identifiersToReturn.map((identifier) => {
                  return BabelTypes.objectProperty(
                    BabelTypes.identifier(identifier),
                    BabelTypes.identifier(identifier),
                  )
                }),
              ),
            ]),
          )
          blockStatement.body.push(additionalReturnStatement)
        }
      },
    },
  })
}

export function transpileJavascript(
  sourceFileName: string,
  sourceFileText: string,
  fileSourceNode: typeof SourceNode,
  elementsWithin: ElementsWithinInPosition,
  applySteganography: SteganographyMode,
): Either<string, TranspileResult> {
  try {
    const { code, map } = fileSourceNode.toStringWithSourceMap({ file: sourceFileName })
    const rawMap = map.toJSON()
    return transpileJavascriptFromCode(
      sourceFileName,
      sourceFileText,
      code,
      rawMap,
      elementsWithin,
      'wrap-in-anon-fn',
      applySteganography,
      [rewriteBlockToCaptureEarlyReturns(code)],
    )
  } catch (e: any) {
    return left(e.message)
  }
}

function insertDataUIDsIntoCodeInner(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  map: RawSourceMap,
  elementsWithin: ElementsWithinInPosition,
  wrap: 'wrap-in-parens' | 'wrap-in-anon-fn' | 'do-not-wrap',
  rootLevel: boolean,
  filename: string,
): Either<string, CodeWithMap> {
  try {
    let codeToUse: string = code
    let mapToUse: RawSourceMap = map

    if (wrap === 'wrap-in-parens') {
      const wrappedInParens = wrapCodeInParensWithMap(
        sourceFileName,
        sourceFileText,
        codeToUse,
        mapToUse,
      )
      codeToUse = wrappedInParens.code
      mapToUse = wrappedInParens.sourceMap
    } else if (wrap === 'wrap-in-anon-fn') {
      const wrappedInAnonFunction = wrapCodeInAnonFunctionWithMap(
        sourceFileName,
        sourceFileText,
        codeToUse,
        mapToUse,
      )
      codeToUse = wrappedInAnonFunction.code
      mapToUse = wrappedInAnonFunction.sourceMap
    }

    const plugins: Array<any> = [
      babelRewriteJSExpressionCode(elementsWithin, false),
      ReactSyntaxPlugin,
    ]
    const transformResult = Babel.transform(codeToUse, {
      presets: [],
      plugins: plugins,
      sourceType: rootLevel ? 'module' : 'script',
      sourceMaps: true,
      retainLines: true,
      inputSourceMap: mapToUse,
      filename: filename,
    })
    return right({
      code: transformResult.code,
      sourceMap: transformResult.map,
    })
  } catch (e: any) {
    return left(e.message)
  }
}

export function insertDataUIDsIntoCode(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  map: RawSourceMap,
  elementsWithin: ElementsWithinInPosition,
  rootLevel: boolean,
  filename: string,
): Either<string, CodeWithMap> {
  const wrappedInParensResult = insertDataUIDsIntoCodeInner(
    sourceFileName,
    sourceFileText,
    code,
    map,
    elementsWithin,
    'wrap-in-parens',
    rootLevel,
    filename,
  )
  if (isRight(wrappedInParensResult)) {
    return wrappedInParensResult
  }
  const wrappedInAnonFunctionResult = insertDataUIDsIntoCodeInner(
    sourceFileName,
    sourceFileText,
    code,
    map,
    elementsWithin,
    'wrap-in-anon-fn',
    rootLevel,
    filename,
  )
  if (isRight(wrappedInAnonFunctionResult)) {
    return wrappedInAnonFunctionResult
  }
  const doNotWrapResult = insertDataUIDsIntoCodeInner(
    sourceFileName,
    sourceFileText,
    code,
    map,
    elementsWithin,
    'do-not-wrap',
    rootLevel,
    filename,
  )
  return doNotWrapResult
}

function getAbsoluteOffsetFromFile(
  lines: string[],
  { line, column }: { line: number; column: number },
): number {
  if (line >= lines.length) {
    return -1
  }

  let offset = 0
  for (let i = 0; i < line - 1; i++) {
    // Add 1 for newline character
    offset += lines[i].length + 1
  }
  offset += column
  return offset
}

function applySteganographyPlugin(
  sourceFileName: string,
  mapToUse: RawSourceMap,
  fileLines: string[],
): () => {
  visitor: BabelTraverse.Visitor
} {
  return () => ({
    visitor: {
      StringLiteral(path) {
        if (path.node.loc == null) {
          return
        }

        // this call somehow messes with the snapshots
        const smc = new SourceMapConsumer(mapToUse)
        const originalStartPosition = smc.originalPositionFor({
          line: path.node.loc.start.line,
          column: path.node.loc.start.column,
        })
        // https://github.com/mozilla/source-map/issues/359 amazingn't
        const absoluteStartPosition =
          getAbsoluteOffsetFromFile(fileLines, {
            line: originalStartPosition.line,
            column: originalStartPosition.column,
          }) - 1

        const original = path.getSource()
        const data: SteganoTextData = {
          filePath: sourceFileName,
          startPosition: absoluteStartPosition,
          endPosition: absoluteStartPosition + original.length,
          originalString: original,
        }

        const cleanedNodeValue = cleanSteganoTextData(path.node.value).cleaned

        path.replaceWith(BabelTypes.stringLiteral(encodeSteganoData(cleanedNodeValue, data)))
      },
    },
  })
}

export function wrapAndTranspileJavascript(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  map: RawSourceMap,
  elementsWithin: ElementsWithinInPosition,
  applySteganography: SteganographyMode,
): Either<string, TranspileResult> {
  const wrappedInParensResult = transpileJavascriptFromCode(
    sourceFileName,
    sourceFileText,
    code,
    map,
    elementsWithin,
    'wrap-in-parens',
    applySteganography,
  )

  if (isRight(wrappedInParensResult)) {
    return wrappedInParensResult
  } else {
    return transpileJavascriptFromCode(
      sourceFileName,
      sourceFileText,
      code,
      map,
      elementsWithin,
      'wrap-in-anon-fn',
      applySteganography,
    )
  }
}

export function transpileJavascriptFromCode(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  map: RawSourceMap,
  elementsWithin: ElementsWithinInPosition,
  wrap: 'wrap-in-parens' | 'wrap-in-anon-fn' | 'do-not-wrap',
  applySteganography: SteganographyMode,
  additionalPlugins: Array<any> = [],
): Either<string, TranspileResult> {
  try {
    let codeToUse: string = code
    let mapToUse: RawSourceMap = map

    if (wrap === 'wrap-in-parens') {
      const wrappedInParens = wrapCodeInParensWithMap(
        sourceFileName,
        sourceFileText,
        codeToUse,
        mapToUse,
      )
      codeToUse = wrappedInParens.code
      mapToUse = wrappedInParens.sourceMap
    } else if (wrap === 'wrap-in-anon-fn') {
      const wrappedInAnonFunction = wrapCodeInAnonFunctionWithMap(
        sourceFileName,
        sourceFileText,
        codeToUse,
        mapToUse,
      )
      codeToUse = wrappedInAnonFunction.code
      mapToUse = wrappedInAnonFunction.sourceMap
    }

    let plugins: Array<any> = []
    if (applySteganography === 'apply-steganography') {
      plugins.push(applySteganographyPlugin(sourceFileName, mapToUse, sourceFileText.split('\n')))
    }
    if (Object.keys(elementsWithin).length > 0) {
      plugins.push(babelRewriteJSExpressionCode(elementsWithin, true))
    }
    plugins.push(...additionalPlugins)
    plugins.push(infiniteLoopPrevention)
    plugins.push('external-helpers')
    plugins.push('transform-typescript')
    plugins.push('transform-react-jsx')
    plugins.push('proposal-class-properties')
    plugins.push(ReactTransformPlugin)
    const transformResult = Babel.transform(codeToUse, {
      presets: ['es2016', 'react'],
      plugins: plugins,
      sourceType: 'script',
      sourceMaps: true,
      inputSourceMap: mapToUse,
      sourceFileName: sourceFileName,
      comments: false,
    })
    const sourceMap: RawSourceMap = {
      ...transformResult.map,
      file: sourceFileName,
    }
    return right({
      code: transformResult.code,
      sourceMap: sourceMap,
    })
  } catch (e: any) {
    return left(e.message)
  }
}
