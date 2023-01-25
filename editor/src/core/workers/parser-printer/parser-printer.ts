import * as React from 'react'
import * as TS from 'typescript'
import {
  addUniquely,
  flatMapArray,
  uniq,
  uniqBy,
  sortBy,
  findLastIndex,
} from '../../shared/array-utils'
import {
  Either,
  eitherToMaybe,
  flatMap2Either,
  flatMapEither,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
  sequenceEither,
  foldEither,
  forEachLeft,
  bimapEither,
  forEachRight,
  traverseEither,
} from '../../shared/either'
import {
  ArbitraryJSBlock,
  BoundParam,
  destructuredArray,
  DestructuredArrayPart,
  destructuredObject,
  destructuredParamPart,
  DestructuredParamPart,
  ElementsWithin,
  functionParam,
  isDestructuredObject,
  isJSXAttributeValue,
  isOmittedParam,
  isRegularParam,
  isUtopiaJSXComponent,
  JSXAttribute,
  JSXElementChild,
  JSXElementName,
  omittedParam,
  Param,
  regularParam,
  TopLevelElement,
  UtopiaJSXComponent,
  utopiaJSXComponent,
  propNamesForParam,
  JSXAttributeOtherJavaScript,
  jsxElementName,
  Comment,
  singleLineComment,
  multiLineComment,
  WithComments,
  VarLetOrConst,
  FunctionDeclarationSyntax,
  getJSXAttributeForced,
  ImportStatement,
  importStatement,
  isImportStatement,
  unparsedCode,
  emptyComments,
  ParsedComments,
  parsedComments,
} from '../../shared/element-template'
import { messageisFatal } from '../../shared/error-messages'
import { memoize } from '../../shared/memoize'
import { defaultIfNull, maybeToArray, optionalMap } from '../../shared/optional-utils'
import {
  HighlightBoundsForUids,
  ImportAlias,
  importAlias,
  Imports,
  ParsedTextFile,
  ParseSuccess,
  PropertyPathPart,
  HighlightBounds,
  ExportsDetail,
  mergeExportsDetail,
  EmptyExportsDetail,
  TextFile,
  reexportWildcard,
  exportVariable,
  reexportVariables,
  exportVariables,
  exportClass,
  exportDefaultFunctionOrClass,
  exportFunction,
  ExportVariable,
  exportDestructuredAssignment,
  exportVariablesWithModifier,
  parseFailure,
  parseSuccess,
  exportIdentifier,
  ExportDetail,
  isImportSideEffects,
} from '../../shared/project-file-types'
import * as PP from '../../shared/property-path'
import { fastForEach, NO_OP } from '../../shared/utils'
import { addImport, emptyImports } from '../common/project-file-utils'
import { UtopiaTsWorkers } from '../common/worker-types'
import { lintCode } from '../linter/linter'
import {
  CanvasMetadataName,
  flattenOutAnnoyingContainers,
  FunctionContents,
  getPropertyNameText,
  liftParsedElementsIntoFunctionContents,
  parseArbitraryNodes,
  parseOutFunctionContents,
  parseOutJSXElements,
  WithParserMetadata,
  parseAttributeOtherJavaScript,
  withParserMetadata,
  isExported,
  expressionTypeForExpression,
  extractPrefixedCode,
  parseAttributeExpression,
  markedAsExported,
  markedAsDefault,
} from './parser-printer-parsing'
import { getBoundsOfNodes, guaranteeUniqueUidsFromTopLevel } from './parser-printer-utils'
import { jsonToExpression } from './json-to-expression'
import { compareOn, comparePrimitive } from '../../../utils/compare'
import { emptySet } from '../../shared/set-utils'
import { addCommentsToNode, getLeadingComments } from './parser-printer-comments'
import { replaceAll } from '../../shared/string-utils'
import { WorkerCodeUpdate, WorkerParsedUpdate } from '../../../components/editor/action-types'
import { fixParseSuccessUIDs } from './uid-fix'
import { applyPrettier } from 'utopia-vscode-common'
import { BakedInStoryboardVariableName } from '../../model/scene-utils'
import { stripExtension } from '../../../components/custom-code/custom-code-utils'
import { absolutePathFromRelativePath } from '../../../utils/path-utils'
import { v4 as UUID } from 'uuid'

function buildPropertyCallingFunction(
  functionName: string,
  parameters: JSXAttribute[],
): TS.Expression {
  return TS.createCall(
    TS.createPropertyAccess(TS.createIdentifier('UtopiaUtils'), TS.createIdentifier(functionName)),
    undefined,
    parameters.map(jsxAttributeToExpression),
  )
}

function getJSXAttributeComments(attribute: JSXAttribute): ParsedComments {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_NESTED_ARRAY':
      return attribute.comments
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return emptyComments
    default:
      const _exhaustiveCheck: never = attribute
      throw new Error(`Unhandled prop type ${JSON.stringify(attribute)}`)
  }
}

function rawCodeToExpressionStatement(
  rawCode: string,
): { statement: TS.ExpressionStatement; sourceFile: TS.SourceFile } | null {
  const sourceFile = TS.createSourceFile(
    'temporary.tsx',
    rawCode,
    TS.ScriptTarget.Latest,
    undefined,
    TS.ScriptKind.TSX,
  )
  const topLevelStatement = sourceFile.statements[0]
  if (topLevelStatement != null && TS.isExpressionStatement(topLevelStatement)) {
    return { statement: topLevelStatement, sourceFile: sourceFile }
  } else {
    return null
  }
}

function jsxAttributeToExpression(attribute: JSXAttribute): TS.Expression {
  function createExpression(): TS.Expression {
    switch (attribute.type) {
      case 'ATTRIBUTE_VALUE':
        if (typeof attribute.value === 'string') {
          return TS.createLiteral(attribute.value)
        } else {
          return jsonToExpression(attribute.value)
        }
      case 'ATTRIBUTE_NESTED_OBJECT':
        const contents = attribute.content
        const objectPropertyExpressions: Array<TS.ObjectLiteralElementLike> = contents.map(
          (prop) => {
            switch (prop.type) {
              case 'PROPERTY_ASSIGNMENT':
                const key = TS.createStringLiteral(prop.key)
                addCommentsToNode(key, prop.keyComments)
                return TS.createPropertyAssignment(key, jsxAttributeToExpression(prop.value))
              case 'SPREAD_ASSIGNMENT':
                return TS.createSpreadAssignment(jsxAttributeToExpression(prop.value))
              default:
                const _exhaustiveCheck: never = prop
                throw new Error(`Unhandled prop type ${prop}`)
            }
          },
        )
        return TS.createObjectLiteral(objectPropertyExpressions)
      case 'ATTRIBUTE_NESTED_ARRAY':
        const arrayExpressions: Array<TS.Expression> = attribute.content.map((elem) => {
          switch (elem.type) {
            case 'ARRAY_SPREAD':
              return TS.createSpread(jsxAttributeToExpression(elem.value))
            case 'ARRAY_VALUE':
              return jsxAttributeToExpression(elem.value)
            default:
              const _exhaustiveCheck: never = elem
              throw new Error(`Unhandled array element type ${elem}`)
          }
        })
        return TS.createArrayLiteral(arrayExpressions)
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        const maybeExpressionStatement = rawCodeToExpressionStatement(attribute.javascript)
        return maybeExpressionStatement == null
          ? TS.createNull()
          : maybeExpressionStatement.statement.expression
      case 'ATTRIBUTE_FUNCTION_CALL':
        return buildPropertyCallingFunction(attribute.functionName, attribute.parameters)
      default:
        const _exhaustiveCheck: never = attribute
        throw new Error(`Unhandled prop type ${JSON.stringify(attribute)}`)
    }
  }
  // Slide the comments onto the expression.
  const expression = createExpression()
  addCommentsToNode(expression, getJSXAttributeComments(attribute))
  return expression
}

function withUID<T>(
  sourceFile: TS.SourceFile | undefined,
  attributes: TS.JsxAttributes,
  defaultResult: T,
  usingUID: (uid: string) => T,
): T {
  function nodeGetText(node: TS.Node): string {
    if (sourceFile == null) {
      return (node as any).text
    } else {
      return node.getText(sourceFile)
    }
  }
  for (const property of attributes?.properties ?? []) {
    if (TS.isJsxAttribute(property)) {
      if (nodeGetText(property.name) === 'data-uid') {
        if (property.initializer != null) {
          const init = property.initializer
          if (TS.isJsxExpression(init)) {
            if (init.expression != null) {
              const uidExpression = init.expression
              if (TS.isStringLiteral(uidExpression)) {
                const uid = nodeGetText(uidExpression)
                return usingUID(uid)
              }
            }
          } else {
            const uid = nodeGetText(init)
            return usingUID(uid)
          }
        }
      }
    }
  }
  return defaultResult
}

function updateJSXElementsWithin(
  expression: TS.Expression,
  elementsWithin: ElementsWithin,
  imports: Imports,
  stripUIDs: boolean,
): TS.Expression {
  function streamlineInElementsWithin(
    attributes: TS.JsxAttributes,
    originalNode: TS.Node,
  ): TS.Node {
    return withUID(undefined, attributes, originalNode, (uid) => {
      if (uid in elementsWithin) {
        return jsxElementToExpression(elementsWithin[uid], imports, stripUIDs)
      } else {
        return originalNode
      }
    })
  }

  function processNode(node: TS.Node): TS.Node {
    if (TS.isJsxElement(node)) {
      const newNode = streamlineInElementsWithin(node.openingElement.attributes, node)
      if (TS.isJsxElement(newNode)) {
        return TS.updateJsxElement(
          node,
          newNode.openingElement,
          newNode.children,
          newNode.closingElement,
        )
      } else {
        return newNode
      }
    } else if (TS.isJsxSelfClosingElement(node)) {
      const newNode = streamlineInElementsWithin(node.attributes, node)
      if (TS.isJsxSelfClosingElement(newNode)) {
        return TS.updateJsxSelfClosingElement(node, newNode.tagName, undefined, newNode.attributes)
      } else {
        return newNode
      }
    } else {
      return node
    }
  }

  const transformer: (context: TS.TransformationContext) => TS.Transformer<TS.Expression> = (
    context,
  ) => {
    const visitor: TS.Visitor = (node) => {
      const nodeWithVisitedChildren = TS.visitEachChild(node, visitor, context)
      return processNode(nodeWithVisitedChildren)
    }
    return (node) => TS.visitNode(node, visitor)
  }
  return TS.transform<TS.Expression>(expression, [transformer]).transformed[0]
}

function jsxElementNameToExpression(name: JSXElementName): TS.JsxTagNameExpression {
  const baseIdentifier = TS.createIdentifier(name.baseVariable)
  return PP.getElements(name.propertyPath).reduce<TS.JsxTagNameExpression>(
    (working: TS.JsxTagNameExpression, element: PropertyPathPart) => {
      // We have to kludge this together because there's no factory function for a `JsxTagNamePropertyAccess`.
      const propertyAccess = TS.createPropertyAccess(
        working,
        TS.createIdentifier(element.toString()),
      )
      const result: TS.JsxTagNamePropertyAccess = {
        ...propertyAccess,
        expression: working,
      }
      return result
    },
    baseIdentifier,
  )
}

function getFragmentElementNameFromImports(imports: Imports): JSXElementName {
  const possibleReactImport = imports['react']
  if (possibleReactImport != null) {
    const fromWithin = possibleReactImport.importedFromWithin.find(
      (within) => within.name === 'Fragment',
    )
    if (fromWithin != null) {
      return jsxElementName(fromWithin.alias, [])
    }
    if (possibleReactImport.importedAs != null) {
      return jsxElementName(possibleReactImport.importedAs, ['Fragment'])
    }
    if (possibleReactImport.importedWithName != null) {
      return jsxElementName(possibleReactImport.importedWithName, ['Fragment'])
    }
  }

  // Default down to this.
  return jsxElementName('React', ['Fragment'])
}

function jsxElementToExpression(
  element: JSXElementChild,
  imports: Imports,
  stripUIDs: boolean,
): TS.JsxElement | TS.JsxSelfClosingElement | TS.JsxText | TS.JsxExpression | TS.JsxFragment {
  switch (element.type) {
    case 'JSX_ELEMENT': {
      let attribsArray: Array<TS.JsxAttributeLike> = []
      fastForEach(element.props, (propEntry) => {
        switch (propEntry.type) {
          case 'JSX_ATTRIBUTES_ENTRY':
            const skip = stripUIDs && propEntry.key === 'data-uid'
            if (!skip) {
              const prop = propEntry.value
              const identifier = TS.createIdentifier(propEntry.key)
              let attributeToAdd: TS.JsxAttribute
              if (isJSXAttributeValue(prop) && typeof prop.value === 'boolean') {
                // Use the shorthand style for true values, and the explicit style for false values
                if (prop.value) {
                  // The `any` allows our `undefined` to punch through the invalid typing
                  // of `createJsxAttribute`.
                  attributeToAdd = TS.createJsxAttribute(identifier, undefined as any)
                } else {
                  attributeToAdd = TS.createJsxAttribute(
                    identifier,
                    TS.createJsxExpression(undefined, TS.createFalse()),
                  )
                }
              } else {
                const attributeExpression = jsxAttributeToExpression(prop)
                const initializer: TS.StringLiteral | TS.JsxExpression = TS.isStringLiteral(
                  attributeExpression,
                )
                  ? attributeExpression
                  : TS.createJsxExpression(undefined, attributeExpression)
                attributeToAdd = TS.createJsxAttribute(identifier, initializer)
              }
              addCommentsToNode(attributeToAdd, propEntry.comments)
              attribsArray.push(attributeToAdd)
            }
            break
          case 'JSX_ATTRIBUTES_SPREAD':
            const attributeExpression = jsxAttributeToExpression(propEntry.spreadValue)
            const attributeToAdd = TS.createJsxSpreadAttribute(attributeExpression)
            addCommentsToNode(attributeToAdd, propEntry.comments)
            attribsArray.push(attributeToAdd)
            break
          default:
            const _exhaustiveCheck: never = propEntry
            throw new Error(`Unhandled attribute type ${JSON.stringify(propEntry)}`)
        }
      })
      const attribs = TS.createJsxAttributes(attribsArray)
      const tagName = jsxElementNameToExpression(element.name)
      if (element.children.length === 0) {
        return TS.createJsxSelfClosingElement(tagName, undefined, attribs)
      } else {
        const opening = TS.createJsxOpeningElement(tagName, undefined, attribs)
        const closing = TS.createJsxClosingElement(tagName)
        return TS.createJsxElement(
          opening,
          element.children.map((child) => jsxElementToExpression(child, imports, stripUIDs)),
          closing,
        )
      }
    }
    case 'JSX_ARBITRARY_BLOCK': {
      const maybeExpressionStatement = rawCodeToExpressionStatement(element.javascript)
      let rawCode: string = element.javascript // Fallback for the case where the code is simply a comment
      if (maybeExpressionStatement != null) {
        const { statement, sourceFile } = maybeExpressionStatement
        const lastToken = statement.getLastToken(sourceFile)
        const finalComments =
          lastToken == null
            ? emptyComments
            : parsedComments([], getLeadingComments(element.javascript, lastToken))

        const updatedStatement = updateJSXElementsWithin(
          statement.expression,
          element.elementsWithin,
          imports,
          stripUIDs,
        )
        addCommentsToNode(updatedStatement, finalComments)
        rawCode = TS.createPrinter({ omitTrailingSemicolon: true }).printNode(
          TS.EmitHint.Unspecified,
          updatedStatement,
          sourceFile,
        )
      }

      // By creating a `JsxText` element containing the raw code surrounded by braces, we can print the code directly
      return TS.createJsxText(`{${rawCode}}`)
    }
    case 'JSX_FRAGMENT': {
      const children = element.children.map((child) => {
        return jsxElementToExpression(child, imports, stripUIDs)
      })
      return TS.createJsxFragment(
        TS.createJsxOpeningFragment(),
        children,
        TS.createJsxJsxClosingFragment(),
      )
    }
    case 'JSX_TEXT_BLOCK': {
      return TS.createJsxText(element.text)
    }
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled element type ${JSON.stringify(element)}`)
  }
}

export interface PrintCodeOptions {
  forPreview: boolean
  pretty: boolean
  includeElementMetadata: boolean
  stripUIDs: boolean
  insertLinesBetweenStatements: boolean
}

export function printCodeOptions(
  forPreview: boolean,
  pretty: boolean,
  includeElementMetadata: boolean,
  stripUIDs: boolean = false,
  insertLinesBetweenStatements: boolean = false,
): PrintCodeOptions {
  return {
    forPreview: forPreview,
    pretty: pretty,
    includeElementMetadata: includeElementMetadata,
    stripUIDs: stripUIDs,
    insertLinesBetweenStatements: insertLinesBetweenStatements,
  }
}

function getModifersForComponent(
  element: UtopiaJSXComponent,
  detailOfExports: ExportsDetail,
): Array<TS.Modifier> {
  let result: Array<TS.Modifier> = []
  let isExportedDirectly: boolean = false
  let isExportedAsDefault: boolean = false
  for (const exportDetail of detailOfExports) {
    switch (exportDetail.type) {
      case 'EXPORT_CLASS':
        break
      case 'EXPORT_FUNCTION':
        if (exportDetail.functionName === element.name) {
          isExportedDirectly = true
        }
        break
      case 'EXPORT_VARIABLES':
        break
      case 'EXPORT_IDENTIFIER':
        break
      case 'EXPORT_DESTRUCTURED_ASSIGNMENT':
        break
      case 'EXPORT_VARIABLES_WITH_MODIFIER':
        for (const exportVar of exportDetail.variables) {
          if (exportVar === element.name) {
            isExportedDirectly = true
          }
        }
        break
      case 'EXPORT_DEFAULT_FUNCTION_OR_CLASS':
        if (exportDetail.name === element.name) {
          isExportedDirectly = true
          isExportedAsDefault = true
        }
        break
      case 'REEXPORT_WILDCARD':
        break
      case 'REEXPORT_VARIABLES':
        break
      default:
        const _exhaustiveCheck: never = exportDetail
        throw new Error(`Unhandled type ${JSON.stringify(exportDetail)}`)
    }
  }

  if (isExportedDirectly) {
    result.push(TS.createToken(TS.SyntaxKind.ExportKeyword))
  }
  if (isExportedAsDefault) {
    result.push(TS.createToken(TS.SyntaxKind.DefaultKeyword))
  }

  return result
}

function printUtopiaJSXComponent(
  printOptions: PrintCodeOptions,
  imports: Imports,
  element: UtopiaJSXComponent,
  detailOfExports: ExportsDetail,
): TS.Node {
  const asJSX = jsxElementToExpression(element.rootElement, imports, printOptions.stripUIDs)
  if (TS.isJsxElement(asJSX) || TS.isJsxSelfClosingElement(asJSX) || TS.isJsxFragment(asJSX)) {
    let elementNode: TS.Node
    const jsxElementExpression = asJSX
    const modifiers = getModifersForComponent(element, detailOfExports)
    const nodeFlags =
      element.declarationSyntax === 'function'
        ? TS.NodeFlags.None
        : nodeFlagsForVarLetOrConst(element.declarationSyntax)
    if (element.isFunction) {
      const functionParams = maybeToArray(element.param).map(printParam)

      function bodyForFunction(): TS.Block {
        let statements: Array<TS.Statement> = []
        if (element.arbitraryJSBlock != null) {
          // I just punched a hole in the space time continuum with this cast to any.
          // Somehow it works, I assume because it really only cares about Nodes internally and
          // not Statements but I will deny all knowledge of ever having done this.
          statements.push(printArbitraryJSBlock(element.arbitraryJSBlock) as any)
        }

        const returnStatement = TS.createReturn(jsxElementExpression)
        addCommentsToNode(returnStatement, element.returnStatementComments)
        statements.push(returnStatement)
        return TS.createBlock(statements, printOptions.insertLinesBetweenStatements)
      }

      function bodyForArrowFunction(): TS.ConciseBody {
        if (element.arbitraryJSBlock != null || element.blockOrExpression === 'block') {
          return bodyForFunction()
        } else if (element.blockOrExpression === 'parenthesized-expression') {
          const bodyExpression = TS.factory.createParenthesizedExpression(jsxElementExpression)
          addCommentsToNode(bodyExpression, element.returnStatementComments)
          return bodyExpression
        } else {
          return jsxElementExpression
        }
      }

      if (element.declarationSyntax === 'function') {
        elementNode = TS.createFunctionDeclaration(
          undefined,
          modifiers,
          undefined,
          element.name ?? undefined,
          undefined,
          functionParams,
          undefined,
          bodyForFunction(),
        )
      } else {
        const arrowFunction = TS.createArrowFunction(
          undefined,
          undefined,
          functionParams,
          undefined,
          undefined,
          bodyForArrowFunction(),
        )
        if (element.name == null) {
          elementNode = TS.createExportAssignment(undefined, modifiers, undefined, arrowFunction)
        } else {
          const varDec = TS.createVariableDeclaration(element.name, undefined, arrowFunction)
          const varDecList = TS.createVariableDeclarationList([varDec], nodeFlags)
          elementNode = TS.createVariableStatement(modifiers, varDecList)
        }
      }
    } else {
      if (element.name == null) {
        elementNode = TS.createExportAssignment(undefined, modifiers, undefined, asJSX)
      } else {
        const varDec = TS.createVariableDeclaration(element.name, undefined, asJSX)
        const varDecList = TS.createVariableDeclarationList([varDec], nodeFlags)
        elementNode = TS.createVariableStatement(modifiers, varDecList)
      }
    }

    return elementNode
  } else {
    throw new Error(
      `Somehow ended up with the wrong type of root element ${JSON.stringify(element.rootElement)}`,
    )
  }
}

function printParam(param: Param): TS.ParameterDeclaration {
  const dotDotDotToken = optionallyCreateDotDotDotToken(param.dotDotDotToken)
  return TS.createParameter(
    undefined,
    undefined,
    dotDotDotToken,
    printBoundParam(param.boundParam),
    undefined,
    undefined,
    undefined,
  )
}

function printBindingExpression(
  defaultExpression: JSXAttributeOtherJavaScript | null,
): TS.Expression | undefined {
  if (defaultExpression == null) {
    return undefined
  } else {
    return jsxAttributeToExpression(defaultExpression)
  }
}

function printBindingElement(propertyName: string | undefined, param: Param): TS.BindingElement {
  const dotDotDotToken = optionallyCreateDotDotDotToken(param.dotDotDotToken)
  return TS.createBindingElement(
    dotDotDotToken,
    optionalMap(TS.createIdentifier, propertyName) ?? undefined,
    printBoundParam(param.boundParam),
    param.boundParam.type === 'REGULAR_PARAM'
      ? printBindingExpression(param.boundParam.defaultExpression)
      : undefined,
  )
}

function printBoundParam(boundParam: BoundParam): TS.BindingName {
  if (isRegularParam(boundParam)) {
    return TS.createIdentifier(boundParam.paramName)
  } else if (isDestructuredObject(boundParam)) {
    const printedParts = boundParam.parts.map((part) =>
      printBindingElement(part.propertyName, part.param),
    )
    return TS.createObjectBindingPattern(printedParts)
  } else {
    const printedParts = boundParam.parts.map((part) => {
      if (isOmittedParam(part)) {
        return TS.createOmittedExpression()
      } else {
        return printBindingElement(undefined, part)
      }
    })
    return TS.createArrayBindingPattern(printedParts)
  }
}

function optionallyCreateDotDotDotToken(createIt: boolean): TS.DotDotDotToken | undefined {
  return createIt ? TS.createToken(TS.SyntaxKind.DotDotDotToken) : undefined
}

function printRawComment(comment: Comment): string {
  return `${comment.rawText}${comment.trailingNewLine ? '\n' : ''}`
}

function printArbitraryJSBlock(block: ArbitraryJSBlock): TS.Node {
  return TS.createUnparsedSourceFile(block.javascript)
}

export const printCode = memoize(printCodeImpl, {
  equals: (a, b) => a === b,
  maxSize: 1,
})

function printStatements(
  statements: Array<TS.Node>,
  shouldPrettify: boolean,
  insertLinesBetweenStatements: boolean,
): string {
  const printer = TS.createPrinter({ newLine: TS.NewLineKind.LineFeed })
  const resultFile = TS.createSourceFile(
    'print.ts',
    '',
    TS.ScriptTarget.Latest,
    false,
    TS.ScriptKind.TS,
  )

  const printedParts: Array<string> = statements.map((statement) =>
    printer.printNode(TS.EmitHint.Unspecified, statement, resultFile),
  )
  const typescriptPrintedResult = printedParts.join(insertLinesBetweenStatements ? '\n' : '')
  let result: string
  if (shouldPrettify) {
    result = applyPrettier(typescriptPrintedResult, false).formatted
  } else {
    result = typescriptPrintedResult
  }
  return result
}

function containsImport(statement: ImportStatement, importToCheck: string): boolean {
  return statement.imports.includes(importToCheck)
}

function printTopLevelElements(
  printOptions: PrintCodeOptions,
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  detailOfExports: ExportsDetail,
): TS.Node[] {
  return topLevelElements.map((e) => {
    switch (e.type) {
      case 'UTOPIA_JSX_COMPONENT':
        return printUtopiaJSXComponent(printOptions, imports, e, detailOfExports)
      case 'ARBITRARY_JS_BLOCK':
        return printArbitraryJSBlock(e)
      case 'IMPORT_STATEMENT':
      case 'UNPARSED_CODE':
        return TS.createUnparsedSourceFile(e.rawCode)
      default:
        const _exhaustiveCheck: never = e
        throw new Error(`Unhandled element type ${JSON.stringify(e)}`)
    }
  })
}

function printCodeImpl(
  filePath: string,
  printOptions: PrintCodeOptions,
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  jsxFactoryFunction: string | null,
  detailOfExports: ExportsDetail,
): string {
  const importOrigins: Array<string> = Object.keys(imports)
  let importDeclarations: Array<TS.Node> = []
  function pushImportDeclaration(importDeclaration: TS.ImportDeclaration) {
    // We have to explicitly insert a newline between declarations
    importDeclarations.push(TS.createUnparsedSourceFile('\n'))
    importDeclarations.push(importDeclaration)
  }

  fastForEach(importOrigins, (importOrigin) => {
    const importForClause = imports[importOrigin]
    const absoluteImportOrigin = stripExtension(
      absolutePathFromRelativePath(filePath, false, importOrigin),
    )
    const matchingTopLevelElements: ImportStatement[] = topLevelElements.filter((e) => {
      if (isImportStatement(e)) {
        const absoluteImportModule = stripExtension(
          absolutePathFromRelativePath(filePath, false, e.module),
        )
        return absoluteImportOrigin === absoluteImportModule
      } else {
        return false
      }
    }) as ImportStatement[]
    const { importedWithName, importedFromWithin, importedAs } = importForClause

    const hasImportWithName = matchingTopLevelElements.some((e) => e.importWithName)
    const missingImportWithName =
      hasImportWithName || importedWithName == null ? null : importedWithName

    const missingImports = importedFromWithin.filter(
      (i) => !matchingTopLevelElements.some((e) => containsImport(e, i.name)),
    )

    if (missingImportWithName != null || missingImports.length > 0) {
      const importClause = TS.createImportClause(
        missingImportWithName == null ? undefined : TS.createIdentifier(missingImportWithName),
        missingImports.length === 0
          ? undefined
          : TS.createNamedImports(
              missingImports.map((i) => {
                return TS.createImportSpecifier(
                  false,
                  TS.createIdentifier(i.name),
                  TS.createIdentifier(i.alias),
                )
              }),
            ),
      )

      const importDeclaration = TS.createImportDeclaration(
        undefined,
        undefined,
        importClause,
        TS.createStringLiteral(importOrigin),
      )
      pushImportDeclaration(importDeclaration)
    }

    const hasImportStarAs = matchingTopLevelElements.some((e) => e.importStarAs)
    const missingImportStarAs = hasImportStarAs || importedAs == null ? null : importedAs

    if (missingImportStarAs != null) {
      const wildcardClause = TS.createImportClause(
        undefined,
        TS.createNamespaceImport(TS.createIdentifier(missingImportStarAs)),
      )

      const importDeclaration = TS.createImportDeclaration(
        undefined,
        undefined,
        wildcardClause,
        TS.createStringLiteral(importOrigin),
      )
      pushImportDeclaration(importDeclaration)
    }

    if (isImportSideEffects(importForClause) && matchingTopLevelElements.length === 0) {
      const importDeclaration = TS.createImportDeclaration(
        undefined,
        undefined,
        undefined,
        TS.createStringLiteral(importOrigin),
      )

      pushImportDeclaration(importDeclaration)
    }
  })

  const lastImportIndex = findLastIndex(isImportStatement, topLevelElements)
  const upToLastImport = lastImportIndex > 0 ? topLevelElements.slice(0, lastImportIndex + 1) : []
  const afterLastImport =
    lastImportIndex > 0 ? topLevelElements.slice(lastImportIndex + 1) : topLevelElements

  const statementsToPrint: Array<TS.Node> = [
    ...printTopLevelElements(printOptions, imports, upToLastImport, detailOfExports),
    ...importDeclarations,
    ...printTopLevelElements(printOptions, imports, afterLastImport, detailOfExports),
  ]

  return printStatements(
    statementsToPrint,
    printOptions.pretty,
    printOptions.insertLinesBetweenStatements,
  )
}

interface PossibleCanvasContentsExpression {
  type: 'POSSIBLE_CANVAS_CONTENTS_EXPRESSION'
  name: string
  initializer: TS.Expression
  declarationSyntax: FunctionDeclarationSyntax
}

function possibleCanvasContentsExpression(
  name: string,
  initializer: TS.Expression,
  declarationSyntax: FunctionDeclarationSyntax,
): PossibleCanvasContentsExpression {
  return {
    type: 'POSSIBLE_CANVAS_CONTENTS_EXPRESSION',
    name: name,
    initializer: initializer,
    declarationSyntax: declarationSyntax,
  }
}

interface PossibleCanvasContentsFunction {
  type: 'POSSIBLE_CANVAS_CONTENTS_FUNCTION'
  name: string | null
  parameters: TS.NodeArray<TS.ParameterDeclaration>
  body: TS.ConciseBody
  declarationSyntax: FunctionDeclarationSyntax
}

function possibleCanvasContentsFunction(
  name: string | null,
  parameters: TS.NodeArray<TS.ParameterDeclaration>,
  body: TS.ConciseBody,
  declarationSyntax: FunctionDeclarationSyntax,
): PossibleCanvasContentsFunction {
  return {
    type: 'POSSIBLE_CANVAS_CONTENTS_FUNCTION',
    name: name,
    parameters: parameters,
    body: body,
    declarationSyntax: declarationSyntax,
  }
}

type PossibleCanvasContents = PossibleCanvasContentsExpression | PossibleCanvasContentsFunction

function isPossibleCanvasContentsFunction(
  canvasContents: PossibleCanvasContents,
): canvasContents is PossibleCanvasContentsFunction {
  return canvasContents.type === 'POSSIBLE_CANVAS_CONTENTS_FUNCTION'
}

function getVarLetOrConst(declarationList: TS.VariableDeclarationList): VarLetOrConst {
  if ((declarationList.flags & TS.NodeFlags.Const) === TS.NodeFlags.Const) {
    return 'const'
  } else if ((declarationList.flags & TS.NodeFlags.Let) === TS.NodeFlags.Let) {
    return 'let'
  } else {
    return 'var'
  }
}

function nodeFlagsForVarLetOrConst(varLetOrConst: VarLetOrConst): TS.NodeFlags {
  switch (varLetOrConst) {
    case 'const':
      return TS.NodeFlags.Const
    case 'let':
      return TS.NodeFlags.Let
    case 'var':
      return TS.NodeFlags.None
    default:
      const _exhaustiveCheck: never = varLetOrConst
      throw new Error(`Unhandled variable declaration type ${JSON.stringify(varLetOrConst)}`)
  }
}

export function looksLikeCanvasElements(
  sourceFile: TS.SourceFile,
  node: TS.Node,
): Either<TS.Node, PossibleCanvasContents> {
  if (TS.isVariableStatement(node)) {
    const variableDeclaration = node.declarationList.declarations[0]
    if (variableDeclaration != null) {
      if (variableDeclaration.initializer != null) {
        const name = variableDeclaration.name.getText(sourceFile)
        const initializer = variableDeclaration.initializer
        const varLetOrConst = getVarLetOrConst(node.declarationList)
        if (TS.isArrowFunction(initializer)) {
          return right(
            possibleCanvasContentsFunction(
              name,
              initializer.parameters,
              initializer.body,
              varLetOrConst,
            ),
          )
        } else if (name === BakedInStoryboardVariableName) {
          // FIXME The below case should result in a parsed top level *element*, but instead it is treated
          // as a *component*. Unfortunately our use of the storyboard incorrectly relies on it being treated
          // as a component. See https://github.com/concrete-utopia/utopia/issues/1718 for more info
          return right(
            possibleCanvasContentsExpression(name, variableDeclaration.initializer, varLetOrConst),
          )
        }
      }
    }
  } else if (TS.isFunctionDeclaration(node)) {
    if (node.body != null) {
      const name = node.name == null ? null : node.name.getText(sourceFile)
      return right(possibleCanvasContentsFunction(name, node.parameters, node.body, 'function'))
    }
  }

  return left(node)
}

function getJsxFactoryFunction(sourceFile: TS.SourceFile): string | null {
  const jsxPragmas = (sourceFile as any).pragmas.get('jsx')
  if (jsxPragmas != null) {
    const chosenpragma = Array.isArray(jsxPragmas) ? jsxPragmas[0] : jsxPragmas
    const factoryName = chosenpragma.arguments.factory
    return factoryName
  } else {
    return null
  }
}

function detailsFromExportAssignment(
  sourceFile: TS.SourceFile,
  declaration: TS.ExportAssignment,
): Either<TS.ExportAssignment, ExportDetail> {
  if (TS.isIdentifier(declaration.expression)) {
    return right(exportIdentifier(declaration.expression.getText(sourceFile)))
  } else if (TS.isArrowFunction(declaration.expression)) {
    return right(exportDefaultFunctionOrClass(null))
  } else {
    return left(declaration)
  }
}

function detailsFromExportDeclaration(
  sourceFile: TS.SourceFile,
  declaration: TS.ExportDeclaration,
): Either<TS.ExportDeclaration, ExportsDetail> {
  const { exportClause, moduleSpecifier } = declaration
  if (exportClause != null && TS.isNamedExports(exportClause)) {
    const moduleName =
      moduleSpecifier != null && TS.isStringLiteral(moduleSpecifier)
        ? moduleSpecifier.text
        : undefined
    const exportVars = [...exportClause.elements.values()].map((specifier) => {
      const specifierName = specifier.name.getText(sourceFile)
      const propertyName = specifier.propertyName?.getText(sourceFile) ?? null
      let variableName: string
      let variableAlias: string | null = null
      if (propertyName == null) {
        variableName = specifierName
      } else {
        variableName = propertyName
        variableAlias = specifierName
      }

      return exportVariable(variableName, variableAlias)
    })
    if (moduleName == null) {
      return right([exportVariables(exportVars)])
    } else {
      return right([reexportVariables(moduleName, exportVars)])
    }
  } else if (
    exportClause != null &&
    TS.isNamespaceExport(exportClause) &&
    moduleSpecifier != null &&
    TS.isStringLiteralLike(moduleSpecifier)
  ) {
    // Caters for `export * as exportClause from 'moduleSpecifier'`.
    return right([reexportWildcard(moduleSpecifier.text, exportClause.name.getText(sourceFile))])
  } else if (
    exportClause == null &&
    moduleSpecifier != null &&
    TS.isStringLiteralLike(moduleSpecifier)
  ) {
    // Caters for `export * from 'moduleSpecifier'`.
    return right([reexportWildcard(moduleSpecifier.text, null)])
  } else {
    return left(declaration)
  }
}

export function getComponentsRenderedWithReactDOM(
  sourceFile: TS.SourceFile,
  node: TS.Node,
): Array<string | null> {
  if (TS.isExpressionStatement(node)) {
    const expressionStatement: TS.ExpressionStatement = node
    if (TS.isCallExpression(expressionStatement.expression)) {
      const callExpression: TS.CallExpression = expressionStatement.expression
      if (TS.isPropertyAccessExpression(callExpression.expression)) {
        const propertyAccessExpression = callExpression.expression
        if (
          TS.isIdentifier(propertyAccessExpression.expression) &&
          TS.isIdentifier(propertyAccessExpression.name)
        ) {
          const propertyExpressionIdentifier: TS.Identifier = propertyAccessExpression.expression
          const propertyNameIdentifier: TS.Identifier = propertyAccessExpression.name
          // Checks if this starts as ReactDOM.render(...).
          if (
            propertyExpressionIdentifier.getText(sourceFile) === 'ReactDOM' &&
            propertyNameIdentifier.getText(sourceFile) === 'render'
          ) {
            const firstArgument = callExpression.arguments[0]
            if (firstArgument != null) {
              if (TS.isJsxSelfClosingElement(firstArgument)) {
                const selfClosingElement: TS.JsxSelfClosingElement = firstArgument
                if (TS.isIdentifier(selfClosingElement.tagName)) {
                  const tagName: TS.Identifier = selfClosingElement.tagName
                  return [tagName.getText(sourceFile)]
                }
              }
              if (TS.isJsxElement(firstArgument)) {
                const jsxElement: TS.JsxElement = firstArgument
                if (TS.isIdentifier(jsxElement.openingElement.tagName)) {
                  const tagName: TS.Identifier = jsxElement.openingElement.tagName
                  return [tagName.getText(sourceFile)]
                }
              }
            }
          }
        }
      }
    }
  }

  return []
}

export function isReactImported(sourceFile: TS.SourceFile): boolean {
  const topLevelNodes = flatMapArray(
    (e) => flattenOutAnnoyingContainers(sourceFile, e),
    sourceFile.getChildren(sourceFile),
  )
  return topLevelNodes.some((topLevelNode) => {
    if (TS.isImportDeclaration(topLevelNode)) {
      if (TS.isStringLiteral(topLevelNode.moduleSpecifier)) {
        const importClause: TS.ImportClause | undefined = topLevelNode.importClause
        const importFrom: string = topLevelNode.moduleSpecifier.text
        if (
          (importClause?.namedBindings != null || importClause?.name != null) &&
          importFrom === 'react'
        ) {
          return true
        }
      }
    }
    return false
  })
}

export function parseCode(
  filename: string,
  sourceText: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  alreadyExistingUIDs_MUTABLE: Set<string>,
): ParsedTextFile {
  const sourceFile = TS.createSourceFile(filename, sourceText, TS.ScriptTarget.ES3)

  const jsxFactoryFunction = getJsxFactoryFunction(sourceFile)

  if (sourceFile == null) {
    return parseFailure([], null, `File ${filename} not found.`, [])
  } else {
    let topLevelElements: Array<Either<string, TopLevelElement>> = []
    let imports: Imports = emptyImports()
    // Find the already existing UIDs so that when we generate one it doesn't duplicate one
    // existing further ahead.
    let highlightBounds: HighlightBoundsForUids = {}

    let allArbitraryNodes: Array<TS.Node> = []

    // Account for exported components.
    let detailOfExports: ExportsDetail = EmptyExportsDetail

    function pushArbitraryNode(node: TS.Node) {
      if (node.kind !== TS.SyntaxKind.EndOfFileToken) {
        const nodeParseResult = parseArbitraryNodes(
          sourceFile,
          sourceText,
          filename,
          [node],
          imports,
          topLevelNames,
          null,
          highlightBounds,
          alreadyExistingUIDs_MUTABLE,
          true,
          '',
          false,
        )
        topLevelElements.push(
          mapEither((parsed) => {
            highlightBounds = parsed.highlightBounds
            return parsed.value
          }, nodeParseResult),
        )
        allArbitraryNodes = [...allArbitraryNodes, node]
      }
    }

    function pushImportStatement(statement: ImportStatement) {
      topLevelElements.push(right(statement))
    }

    function pushUnparsedCode(rawCode: string) {
      topLevelElements.push(right(unparsedCode(rawCode)))
    }

    const topLevelNodes = flatMapArray(
      (e) => flattenOutAnnoyingContainers(sourceFile, e),
      sourceFile.getChildren(sourceFile),
    )

    const topLevelNames = flatMapArray((node) => {
      let names: Array<string> = []
      if (TS.isVariableStatement(node)) {
        for (const variableDeclaration of node.declarationList.declarations) {
          if (variableDeclaration.initializer != null) {
            names.push(variableDeclaration.name.getText(sourceFile))
          }
        }
      } else if (TS.isFunctionLike(node)) {
        if (node.name != null) {
          names.push(node.name.getText(sourceFile))
        }
      } else if (TS.isClassDeclaration(node)) {
        if (node.name != null) {
          names.push(node.name.getText(sourceFile))
        }
      }
      return names
    }, topLevelNodes)

    for (const topLevelElement of topLevelNodes) {
      // Capture anything before this node as unparsed code
      const prefixedCode = extractPrefixedCode(topLevelElement, sourceFile)
      if (prefixedCode.length > 0) {
        pushUnparsedCode(prefixedCode)
      }

      if (TS.isExportAssignment(topLevelElement)) {
        // Handle export assignments: `export default App`
        const fromAssignment = detailsFromExportAssignment(sourceFile, topLevelElement)
        // Parsed it fully, so it can be incorporated.
        forEachRight(fromAssignment, (toMerge) => {
          detailOfExports = detailOfExports.concat(toMerge)

          if (toMerge.type === 'EXPORT_IDENTIFIER') {
            pushUnparsedCode(topLevelElement.getText(sourceFile))
          } else {
            pushArbitraryNode(topLevelElement)
          }
        })
        // Unable to parse it so treat it as an arbitrary node.
        forEachLeft(fromAssignment, (exportDeclaration) => {
          pushArbitraryNode(exportDeclaration)
        })
      } else if (TS.isExportDeclaration(topLevelElement)) {
        // Handle export declarations.
        const fromDeclaration = detailsFromExportDeclaration(sourceFile, topLevelElement)
        // Parsed it fully, so it can be incorporated.
        forEachRight(fromDeclaration, (toMerge) => {
          detailOfExports = mergeExportsDetail(detailOfExports, toMerge)
          pushUnparsedCode(topLevelElement.getText(sourceFile))
        })
        // Unable to parse it so treat it as an arbitrary node.
        forEachLeft(fromDeclaration, (exportDeclaration) => {
          pushArbitraryNode(exportDeclaration)
        })
      } else if (TS.isClassDeclaration(topLevelElement) && markedAsExported(topLevelElement)) {
        // Handle classes.
        // Check if this is the 'default' export, which is to say what the caller will get if not
        // specifying a particular value from the module.
        if (markedAsDefault(topLevelElement)) {
          detailOfExports = mergeExportsDetail(detailOfExports, [
            exportDefaultFunctionOrClass(topLevelElement.name?.getText(sourceFile) ?? null),
          ])
        } else {
          if (topLevelElement.name != null) {
            const name = topLevelElement.name.getText(sourceFile)
            detailOfExports = mergeExportsDetail(detailOfExports, [exportClass(name)])
          }
        }
        // Unable to parse it so treat it as an arbitrary node.
        pushArbitraryNode(topLevelElement)
      } else if (TS.isImportDeclaration(topLevelElement)) {
        // Handle imports.
        if (TS.isStringLiteral(topLevelElement.moduleSpecifier)) {
          const importClause: TS.ImportClause | undefined = topLevelElement.importClause
          const importFrom: string = topLevelElement.moduleSpecifier.text
          let importedFromWithin: Array<ImportAlias> = []
          let importedAs: string | null = null
          // this import looks like `import { Cat, dog } from './src/home'`
          if (
            importClause?.namedBindings != null &&
            TS.isNamedImports(importClause.namedBindings)
          ) {
            const importBindings = importClause.namedBindings
            importBindings.elements.forEach((element) => {
              if (element.propertyName == null) {
                importedFromWithin.push(importAlias(element.name.getText(sourceFile)))
              } else {
                importedFromWithin.push(
                  importAlias(
                    element.propertyName.getText(sourceFile),
                    element.name.getText(sourceFile),
                  ),
                )
              }
            })
          }
          // this import looks like `import * as Merlin from './src/dogs'`
          if (
            importClause?.namedBindings != null &&
            TS.isNamespaceImport(importClause.namedBindings)
          ) {
            const importBindings = importClause.namedBindings
            importedAs = importBindings.name.getText(sourceFile)
          }

          // this import looks like `import Cat from './src/cats'`
          const importedWithName = optionalMap((n) => n.getText(sourceFile), importClause?.name)
          imports = addImport(
            filename,
            importFrom,
            importedWithName,
            importedFromWithin,
            importedAs,
            imports,
          )
          const rawImportStatement = importStatement(
            topLevelElement.getText(sourceFile),
            importedAs != null,
            importedWithName != null,
            importedFromWithin.map((i) => i.name),
            importFrom,
          )
          pushImportStatement(rawImportStatement)
        }
      } else {
        const possibleDeclaration = looksLikeCanvasElements(sourceFile, topLevelElement)
        if (isRight(possibleDeclaration)) {
          const canvasContents = possibleDeclaration.value
          const { name } = canvasContents
          let parsedContents: Either<string, WithParserMetadata<FunctionContents>> = left(
            'No contents',
          )
          let isFunction: boolean = false
          let parsedFunctionParam: Either<string, WithParserMetadata<Param> | null> = right(null)
          let propsUsed: Array<string> = []
          if (isPossibleCanvasContentsFunction(canvasContents)) {
            const { parameters, body } = canvasContents
            isFunction = true
            const parsedFunctionParams = parseParams(
              parameters,
              sourceFile,
              sourceText,
              filename,
              imports,
              topLevelNames,
              highlightBounds,
              alreadyExistingUIDs_MUTABLE,
            )
            parsedFunctionParam = flatMapEither((parsedParams) => {
              const paramsValue = parsedParams.value
              if (paramsValue.length === 0) {
                return right(null)
              } else if (paramsValue.length === 1) {
                // Note: We're explicitly ignoring the `propsUsed` value as
                // that should be handled by the call to `propNamesForParam` below.
                return right(
                  withParserMetadata(paramsValue[0], parsedParams.highlightBounds, [], []),
                )
              } else {
                return left('Invalid number of params')
              }
            }, parsedFunctionParams)
            forEachRight(parsedFunctionParam, (param) => {
              const boundParam = param?.value.boundParam
              const propsObjectName =
                boundParam != null && isRegularParam(boundParam) ? boundParam.paramName : null

              propsUsed = param == null ? [] : propNamesForParam(param.value)

              parsedContents = parseOutFunctionContents(
                sourceFile,
                sourceText,
                filename,
                imports,
                topLevelNames,
                propsObjectName,
                body,
                param?.highlightBounds ?? {},
                alreadyExistingUIDs_MUTABLE,
              )
            })
          } else {
            // In this case it's likely/hopefully a straight JSX expression attached to the var.
            parsedContents = liftParsedElementsIntoFunctionContents(
              expressionTypeForExpression(canvasContents.initializer),
              parseOutJSXElements(
                sourceFile,
                sourceText,
                filename,
                [canvasContents.initializer],
                imports,
                topLevelNames,
                null,
                highlightBounds,
                alreadyExistingUIDs_MUTABLE,
              ),
            )
          }
          if (isLeft(parsedContents) || (isFunction && isLeft(parsedFunctionParam))) {
            pushArbitraryNode(topLevelElement)
            if (isExported(topLevelElement)) {
              if (name == null) {
                detailOfExports = mergeExportsDetail(detailOfExports, [
                  exportDefaultFunctionOrClass(null),
                ])
              } else {
                const defaultExport = markedAsDefault(topLevelElement)
                if (defaultExport) {
                  detailOfExports = mergeExportsDetail(detailOfExports, [
                    exportDefaultFunctionOrClass(name),
                  ])
                } else {
                  detailOfExports = mergeExportsDetail(detailOfExports, [exportFunction(name)])
                }
              }
            }
          } else {
            highlightBounds = {
              ...highlightBounds,
              ...parsedContents.value.highlightBounds,
            }
            const contents = parsedContents.value.value
            // If propsUsed is already populated, it's because the user used destructuring, so we can
            // use that. Otherwise, we have to use the list retrieved during parsing
            propsUsed = propsUsed.length > 0 ? propsUsed : uniq(parsedContents.value.propsUsed)
            if (contents.elements.length === 1) {
              const exported = isExported(topLevelElement)
              // capture var vs let vs const vs function here
              const utopiaComponent = utopiaJSXComponent(
                name,
                isFunction,
                canvasContents.declarationSyntax,
                contents.blockOrExpression,
                foldEither(
                  (_) => null,
                  (param) => param?.value ?? null,
                  parsedFunctionParam,
                ),
                propsUsed,
                contents.elements[0].value,
                contents.arbitraryJSBlock,
                false,
                contents.returnStatementComments,
              )

              const defaultExport = markedAsDefault(topLevelElement)
              if (exported) {
                if (name == null) {
                  detailOfExports = mergeExportsDetail(detailOfExports, [
                    exportDefaultFunctionOrClass(null),
                  ])
                } else {
                  if (defaultExport) {
                    detailOfExports = mergeExportsDetail(detailOfExports, [
                      exportDefaultFunctionOrClass(name),
                    ])
                  } else {
                    detailOfExports = mergeExportsDetail(detailOfExports, [exportFunction(name)])
                  }
                }
              }

              topLevelElements.push(right(utopiaComponent))
            } else {
              pushArbitraryNode(topLevelElement)
            }
          }
        } else {
          // Fallback for things which don't parse but may be similar to those
          // node types which do.
          if (TS.isVariableStatement(topLevelElement)) {
            if (markedAsExported(topLevelElement)) {
              let exportVars: Array<ExportVariable> = []

              function pushToExports(bindingOrName: TS.Identifier | TS.BindingElement): void {
                let nameToUse: string
                let aliasToUse: string | null = null
                if (TS.isIdentifier(bindingOrName)) {
                  nameToUse = bindingOrName.getText(sourceFile)
                } else {
                  if (bindingOrName.propertyName == null) {
                    nameToUse = bindingOrName.name.getText(sourceFile)
                  } else {
                    nameToUse = bindingOrName.name.getText(sourceFile)
                    aliasToUse = bindingOrName.propertyName.getText(sourceFile)
                  }
                }

                exportVars.push(exportVariable(nameToUse, aliasToUse))
              }

              function addDeclaration(name: TS.BindingName): void {
                if (TS.isIdentifier(name)) {
                  pushToExports(name)
                } else if (TS.isObjectBindingPattern(name)) {
                  for (const element of name.elements) {
                    pushToExports(element)
                  }
                }
              }

              for (const declaration of topLevelElement.declarationList.declarations) {
                addDeclaration(declaration.name)

                // This distinguishes between the various cases of variable exports
                // as they're very similar.
                if (TS.isObjectBindingPattern(declaration.name)) {
                  if (declaration.initializer == null) {
                    detailOfExports = mergeExportsDetail(detailOfExports, [
                      exportVariables(exportVars),
                    ])
                  } else {
                    detailOfExports = mergeExportsDetail(detailOfExports, [
                      exportDestructuredAssignment(exportVars),
                    ])
                  }
                } else {
                  detailOfExports = mergeExportsDetail(detailOfExports, [
                    exportVariablesWithModifier(
                      exportVars.map((exportVar) => exportVar.variableName),
                    ),
                  ])
                }
                exportVars = []
              }
            }
            pushArbitraryNode(topLevelElement)
          } else if (
            TS.isFunctionDeclaration(topLevelElement) &&
            topLevelElement.name == null &&
            markedAsDefault(topLevelElement) &&
            markedAsExported(topLevelElement)
          ) {
            detailOfExports = mergeExportsDetail(detailOfExports, [
              exportDefaultFunctionOrClass(null),
            ])
            pushUnparsedCode(topLevelElement.getText(sourceFile))
          } else {
            // If all else fails add it as just an arbitrary node.
            pushArbitraryNode(topLevelElement)
          }
        }
      }
    }

    const sequencedTopLevelElements = sequenceEither(topLevelElements)
    if (isLeft(sequencedTopLevelElements)) {
      return parseFailure(null, null, sequencedTopLevelElements.value, [])
    }
    const realTopLevelElements = sequencedTopLevelElements.value

    let topLevelElementsWithFixedUIDs = guaranteeUniqueUidsFromTopLevel(realTopLevelElements)

    let combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null = null
    if (allArbitraryNodes.length > 0) {
      const nodeParseResult = parseArbitraryNodes(
        sourceFile,
        sourceText,
        filename,
        allArbitraryNodes,
        imports,
        topLevelNames,
        null,
        highlightBounds,
        alreadyExistingUIDs_MUTABLE,
        true,
        '',
        true,
      )
      forEachRight(nodeParseResult, (nodeParseSuccess) => {
        combinedTopLevelArbitraryBlock = nodeParseSuccess.value
      })

      const componentsRenderedByReactDOM = flatMapArray(
        (node) => getComponentsRenderedWithReactDOM(sourceFile, node),
        allArbitraryNodes,
      )
      topLevelElementsWithFixedUIDs = topLevelElementsWithFixedUIDs.map((topLevelElement) => {
        if (
          isUtopiaJSXComponent(topLevelElement) &&
          componentsRenderedByReactDOM.includes(topLevelElement.name)
        ) {
          return utopiaJSXComponent(
            topLevelElement.name,
            topLevelElement.isFunction,
            topLevelElement.declarationSyntax,
            topLevelElement.blockOrExpression,
            topLevelElement.param,
            topLevelElement.propsUsed,
            topLevelElement.rootElement,
            topLevelElement.arbitraryJSBlock,
            true,
            emptyComments,
          )
        } else {
          return topLevelElement
        }
      })
    }

    const unfixedParseSuccess = parseSuccess(
      imports,
      topLevelElementsWithFixedUIDs,
      highlightBounds,
      jsxFactoryFunction,
      combinedTopLevelArbitraryBlock,
      detailOfExports,
    )
    const fixedParseSuccess = fixParseSuccessUIDs(
      oldParseResultForUIDComparison,
      unfixedParseSuccess,
      alreadyExistingUIDs_MUTABLE,
    )
    return fixedParseSuccess
  }
}

function parseParams(
  params: TS.NodeArray<TS.ParameterDeclaration>,
  file: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: Set<string>,
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

function parseParam(
  param: TS.ParameterDeclaration | TS.BindingElement,
  file: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: Set<string>,
): Either<string, WithParserMetadata<Param>> {
  const dotDotDotToken = param.dotDotDotToken != null
  const parsedExpression: Either<
    string,
    WithParserMetadata<JSXAttributeOtherJavaScript | undefined>
  > = param.initializer == null
    ? right(withParserMetadata(undefined, existingHighlightBounds, [], []))
    : parseAttributeOtherJavaScript(
        file,
        sourceText,
        filename,
        imports,
        topLevelNames,
        null,
        param.initializer,
        existingHighlightBounds,
        existingUIDs,
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
  expression: WithParserMetadata<JSXAttributeOtherJavaScript | undefined>,
  file: TS.SourceFile,
  sourceText: string,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: Set<string>,
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

function withJSXElementAttributes(
  sourceFile: TS.SourceFile,
  withAttributes: (node: TS.Node, attrs: TS.JsxAttributes) => void,
): void {
  const transformer = (context: TS.TransformationContext) => {
    const walkTree = (node: TS.Node) => {
      if (TS.isJsxSelfClosingElement(node)) {
        withAttributes(node, node.attributes)
      } else if (TS.isJsxElement(node)) {
        withAttributes(node, node.openingElement.attributes)
      }
      TS.visitEachChild(node, walkTree, context)

      return node
    }
    return (n: TS.Node) => TS.visitNode(n, walkTree)
  }

  TS.transform(sourceFile, [transformer])
}

type HighlightBoundsWithoutUid = Omit<HighlightBounds, 'uid'>
const InvalidBoundsMarker = 'INVALID'
export function boundsAreValid(uid: string): boolean {
  return !uid.startsWith(InvalidBoundsMarker)
}

export function getHighlightBoundsWithUID(
  filename: string,
  sourceText: string,
): Array<HighlightBounds> {
  const sourceFile = TS.createSourceFile(
    filename,
    sourceText,
    TS.ScriptTarget.Latest,
    false,
    TS.ScriptKind.TSX,
  )

  let result: Array<HighlightBounds> = []

  if (sourceFile != null) {
    withJSXElementAttributes(
      sourceFile,
      (boundingElement: TS.Node, attributes: TS.JsxAttributes) => {
        const highlightBoundsForUid = (uid: string) => {
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
        const newBounds = withUID(
          undefined,
          attributes,
          highlightBoundsForUid(`${InvalidBoundsMarker}-${UUID()}`),
          highlightBoundsForUid,
        )
        result.push(newBounds)
      },
    )
  }

  return result
}

export function getHighlightBoundsWithoutUID(
  filename: string,
  sourceText: string,
): Array<HighlightBoundsWithoutUid> {
  const sourceFile = TS.createSourceFile(
    filename,
    sourceText,
    TS.ScriptTarget.Latest,
    false,
    TS.ScriptKind.TSX,
  )

  let result: Array<HighlightBoundsWithoutUid> = []

  if (sourceFile != null) {
    withJSXElementAttributes(
      sourceFile,
      (boundingElement: TS.Node, attributes: TS.JsxAttributes) => {
        const startPosition = TS.getLineAndCharacterOfPosition(
          sourceFile,
          boundingElement.getStart(sourceFile, false),
        )
        const endPosition = TS.getLineAndCharacterOfPosition(sourceFile, boundingElement.getEnd())
        result.push({
          startCol: startPosition.character,
          startLine: startPosition.line,
          endCol: endPosition.character,
          endLine: endPosition.line,
        })
      },
    )
  }

  return result
}

export function lintAndParse(
  filename: string,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  alreadyExistingUIDs_MUTABLE: Set<string>,
): ParsedTextFile {
  const lintResult = lintCode(filename, content)
  // Only fatal or error messages should bounce the parse.
  if (lintResult.filter(messageisFatal).length === 0) {
    const result = parseCode(
      filename,
      content,
      oldParseResultForUIDComparison,
      alreadyExistingUIDs_MUTABLE,
    )
    return result
  } else {
    return parseFailure(null, null, null, lintResult)
  }
}
