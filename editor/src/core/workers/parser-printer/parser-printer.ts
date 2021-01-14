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
import { SafeFunction } from '../../shared/code-exec-utils'
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
  ImportStatement,
  importStatement,
  isImportStatement,
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
  exportsDetail,
  ExportsDetail,
  addNamedExportToDetail,
  setNamedDefaultExportInDetail,
  mergeExportsDetail,
  addModifierExportToDetail,
  isExportDetailNamed,
  EmptyExportsDetail,
  setModifierDefaultExportInDetail,
  isExportDefaultModifier,
  isExportDetailModifier,
  isExportDefaultNamed,
} from '../../shared/project-file-types'
import * as PP from '../../shared/property-path'
import { fastForEach, NO_OP } from '../../shared/utils'
import { addImport, emptyImports, parseFailure, parseSuccess } from '../common/project-file-utils'
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
  isDefaultExport,
  expressionTypeForExpression,
} from './parser-printer-parsing'
import { getBoundsOfNodes, guaranteeUniqueUidsFromTopLevel } from './parser-printer-utils'
import { ParserPrinterResultMessage } from './parser-printer-worker'
import creator from './ts-creator'
import { applyPrettier, PrettierConfig } from './prettier-utils'
import { jsonToExpression } from './json-to-expression'
import { compareOn, comparePrimitive } from '../../../utils/compare'
import { emptySet } from '../../shared/set-utils'
import {
  addCommentsToNode,
  emptyComments,
  getComments,
  mergeParsedComments,
  ParsedComments,
} from './parser-printer-comments'
import { replaceAll } from '../../shared/string-utils'

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
                return TS.createPropertyAssignment(
                  TS.createStringLiteral(prop.key),
                  jsxAttributeToExpression(prop.value),
                )
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
        // SP: This is quite truly the most spectacular fudge I've ever had to create in my entire career.
        // Creates a representation of the AST that is code which executes the TS factory functions...
        // ...Then evals it to produce the AST and drills into the result a little.
        const createExpressionAsString = creator(attribute.javascript)
        const newExpression = SafeFunction(
          false,
          { ts: TS, React: React, addCommentsToNode: addCommentsToNode },
          `return ${createExpressionAsString}.expression`,
          [],
          (e) => {
            throw e
          },
        )()
        return newExpression
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
  for (const property of attributes.properties) {
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
      return streamlineInElementsWithin(node.openingElement.attributes, node)
    } else if (TS.isJsxSelfClosingElement(node)) {
      return streamlineInElementsWithin(node.attributes, node)
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
      fastForEach(Object.keys(element.props), (propsKey) => {
        const skip = stripUIDs && propsKey === 'data-uid'
        if (!skip) {
          const prop = element.props[propsKey]
          const identifier = TS.createIdentifier(propsKey)
          if (isJSXAttributeValue(prop) && typeof prop.value === 'boolean') {
            // Use the shorthand style for true values, and the explicit style for false values
            if (prop.value) {
              // The `any` allows our `undefined` to punch through the invalid typing
              // of `createJsxAttribute`.
              attribsArray.push(TS.createJsxAttribute(identifier, undefined as any))
            } else {
              attribsArray.push(
                TS.createJsxAttribute(
                  identifier,
                  TS.createJsxExpression(undefined, TS.createFalse()),
                ),
              )
            }
          } else {
            const attributeExpression = jsxAttributeToExpression(prop)
            const initializer: TS.StringLiteral | TS.JsxExpression = TS.isStringLiteral(
              attributeExpression,
            )
              ? attributeExpression
              : TS.createJsxExpression(undefined, attributeExpression)
            attribsArray.push(TS.createJsxAttribute(identifier, initializer))
          }
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
      let createExpressionAsString: string

      if (element.javascript == '') {
        // this appears as an empty {} inside the jsx.
        createExpressionAsString = `ts.updateSourceFileNode(
          ts.createSourceFile('temporary.tsx', '', ts.ScriptTarget.Latest),
          [ts.createJsxExpression(
            undefined,
            undefined
          )]
        )`
      } else {
        createExpressionAsString = creator(element.javascript)
      }
      let newExpression = SafeFunction(
        false,
        { ts: TS, React: React, addCommentsToNode: addCommentsToNode },
        `var node = ${createExpressionAsString}; return node.expression || node`,
        [],
        (e) => {
          throw e
        },
      )()
      newExpression = updateJSXElementsWithin(
        newExpression,
        element.elementsWithin,
        imports,
        stripUIDs,
      )
      return TS.createJsxExpression(undefined, newExpression)
    }
    case 'JSX_FRAGMENT': {
      const children = element.children.map((child) => {
        return jsxElementToExpression(child, imports, stripUIDs)
      })
      if (element.longForm) {
        const tagName = jsxElementNameToExpression(getFragmentElementNameFromImports(imports))
        if (element.children.length === 0) {
          return TS.createJsxSelfClosingElement(tagName, undefined, TS.createJsxAttributes([]))
        } else {
          const opening = TS.createJsxOpeningElement(tagName, undefined, TS.createJsxAttributes([]))
          const closing = TS.createJsxClosingElement(tagName)
          return TS.createJsxElement(opening, children, closing)
        }
      } else {
        return TS.createJsxFragment(
          TS.createJsxOpeningFragment(),
          children,
          TS.createJsxJsxClosingFragment(),
        )
      }
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
}

export function printCodeOptions(
  forPreview: boolean,
  pretty: boolean,
  includeElementMetadata: boolean,
  stripUIDs: boolean = false,
): PrintCodeOptions {
  return {
    forPreview: forPreview,
    pretty: pretty,
    includeElementMetadata: includeElementMetadata,
    stripUIDs: stripUIDs,
  }
}

function getModifersForComponent(
  element: UtopiaJSXComponent,
  detailOfExports: ExportsDetail,
): Array<TS.Modifier> {
  let result: Array<TS.Modifier> = []
  let isExportedDirectly: boolean = false
  let isExportedAsDefault: boolean = false
  const defaultExport = detailOfExports.defaultExport
  if (
    defaultExport != null &&
    isExportDefaultModifier(defaultExport) &&
    defaultExport.name === element.name
  ) {
    isExportedAsDefault = true
    isExportedDirectly = true
  } else {
    const componentExport = detailOfExports.namedExports[element.name]
    if (componentExport != null && isExportDetailModifier(componentExport)) {
      isExportedDirectly = true
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
        let bodyBlock: TS.Block
        if (element.arbitraryJSBlock != null) {
          // I just punched a hole in the space time continuum with this cast to any.
          // Somehow it works, I assume because it really only cares about Nodes internally and
          // not Statements but I will deny all knowledge of ever having done this.
          statements.push(printArbitraryJSBlock(element.arbitraryJSBlock) as any)
        }

        const returnStatement = TS.createReturn(jsxElementExpression)
        addCommentsToNode(returnStatement, element.returnStatementComments)
        statements.push(returnStatement)
        return TS.createBlock(statements, true)
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
          element.name,
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
        const varDec = TS.createVariableDeclaration(element.name, undefined, arrowFunction)
        const varDecList = TS.createVariableDeclarationList([varDec], nodeFlags)
        elementNode = TS.createVariableStatement(modifiers, varDecList)
      }
    } else {
      const varDec = TS.createVariableDeclaration(element.name, undefined, asJSX)
      const varDecList = TS.createVariableDeclarationList([varDec], nodeFlags)
      elementNode = TS.createVariableStatement(modifiers, varDecList)
    }

    addCommentsToNode(elementNode, element.comments)
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
  const leadingComments = block.comments.leadingComments.map(printRawComment).join('\n')
  const trailingComments = block.comments.trailingComments.map(printRawComment).join('\n')
  const rawText = `${leadingComments}${block.javascript}${trailingComments}`
  return TS.createUnparsedSourceFile(rawText)
}

export const printCode = memoize(printCodeImpl, {
  equals: (a, b) => a === b,
  maxSize: 1,
})

function isVariableDeclaration(line: string): boolean {
  const withoutExport = line.startsWith('export ') ? line.slice(7) : line
  return (
    withoutExport.startsWith('var') ||
    withoutExport.startsWith('let') ||
    withoutExport.startsWith('const')
  )
}

function isFunctionDeclaration(line: string): boolean {
  const withoutExport = line.startsWith('export ') ? line.slice(7) : line
  const withoutDefault = withoutExport.startsWith('default ')
    ? withoutExport.slice(8)
    : withoutExport
  return withoutDefault.startsWith('function')
}

function createBlanklineTracker(): (statement: TS.Node, printedCode: string) => boolean {
  // When inserting blank lines we apply the following series of rules
  // 1) we shouldn't insert blank lines in amongst import statements or exports declarations (not the modifier keyword)
  // 2) variable declarations shouldn't insert a blank line unless a single declaration is spread over separate lines
  // 3) function declarations should always insert a blank line
  // 4) any other (non-import or export) multiline statement should insert a blank line

  let lastLineWasImport = false
  let lastLineWasExport = false
  let lastLineWasFunction = false
  let lastLineWasVariable = false
  let lastLineWasMultiLineStatement = false

  return (statement: TS.Node, rawPrintedCode: string): boolean => {
    // Awful hack until we implement #851
    const printedCode = replaceAll(rawPrintedCode, '/** @jsx jsx */', '').trim()

    const isUnparsedSource = TS.isUnparsedSource(statement)
    const isMultiLineCode =
      printedCode.includes('\n') ||
      printedCode.includes('\r') ||
      (PrettierConfig.printWidth != null && printedCode.length > PrettierConfig.printWidth)
    const isVariable =
      TS.isVariableStatement(statement) ||
      (isUnparsedSource && !isMultiLineCode && isVariableDeclaration(printedCode))
    const isFunction =
      TS.isFunctionDeclaration(statement) ||
      (isUnparsedSource && isFunctionDeclaration(printedCode))
    const isImport =
      TS.isImportDeclaration(statement) || (isUnparsedSource && printedCode.startsWith('import'))
    const isExport =
      TS.isExportDeclaration(statement) ||
      (isUnparsedSource && !isVariable && !isFunction && printedCode.startsWith('export'))

    const isMultiLineStatement = !(isImport || isExport) && isMultiLineCode

    const shouldPrefixWithBlankLine =
      (lastLineWasImport && !isImport) ||
      (!lastLineWasExport && isExport) ||
      (!lastLineWasVariable && isVariable) ||
      (!lastLineWasFunction && isFunction) ||
      lastLineWasMultiLineStatement ||
      isMultiLineStatement

    lastLineWasImport = isImport
    lastLineWasExport = isExport
    lastLineWasFunction = isFunction
    lastLineWasVariable = isVariable
    lastLineWasMultiLineStatement = isMultiLineStatement

    return shouldPrefixWithBlankLine
  }
}

function printStatements(statements: Array<TS.Node>, shouldPrettify: boolean): string {
  const printer = TS.createPrinter({ newLine: TS.NewLineKind.LineFeed })
  const resultFile = TS.createSourceFile(
    'print.ts',
    '',
    TS.ScriptTarget.Latest,
    false,
    TS.ScriptKind.TS,
  )

  const checkShouldInsertBlankLineBeforeStatement = createBlanklineTracker()
  let printedParts: Array<string> = []

  fastForEach(statements, (statement) => {
    const nextLine = printer.printNode(TS.EmitHint.Unspecified, statement, resultFile).trim()
    const shouldPrefixWithBlankLine = checkShouldInsertBlankLineBeforeStatement(statement, nextLine)

    if (printedParts.length > 0 && shouldPrefixWithBlankLine) {
      printedParts.push('\n')
    }

    printedParts.push(nextLine)
  })

  const typescriptPrintedResult = printedParts.join('\n')
  let result: string
  if (shouldPrettify) {
    result = applyPrettier(typescriptPrintedResult, false).formatted
  } else {
    result = typescriptPrintedResult
  }
  return result
}

function produceExportStatements(
  detailOfExports: ExportsDetail,
): Array<TS.ExportAssignment | TS.ExportDeclaration> {
  let workingExportSpecifiers: Array<TS.ExportSpecifier> = []
  let lastModuleName: string | undefined = undefined
  let exportStatements: Array<TS.ExportAssignment | TS.ExportDeclaration> = []
  const { defaultExport, namedExports } = detailOfExports
  if (defaultExport != null) {
    if (isExportDefaultNamed(defaultExport)) {
      exportStatements.push(
        TS.createExportAssignment(
          undefined,
          undefined,
          undefined,
          TS.createIdentifier(defaultExport.name),
        ),
      )
    }
  }

  function createExportStatementFromWorkingSpecifiers() {
    const moduleSpecifier =
      lastModuleName == null ? undefined : TS.createStringLiteral(lastModuleName)
    exportStatements.push(
      TS.createExportDeclaration(
        undefined,
        undefined,
        TS.createNamedExports(workingExportSpecifiers),
        moduleSpecifier,
      ),
    )
    workingExportSpecifiers = []
  }

  for (const exportName of Object.keys(namedExports)) {
    const namedExport = namedExports[exportName]
    if (isExportDetailNamed(namedExport)) {
      const { name, moduleName } = namedExport
      if (moduleName != lastModuleName && workingExportSpecifiers.length > 0) {
        // Create this export statement and move onto the next
        createExportStatementFromWorkingSpecifiers()
      }

      lastModuleName = moduleName

      if (exportName === namedExport.name) {
        workingExportSpecifiers.push(
          TS.createExportSpecifier(undefined, TS.createIdentifier(exportName)),
        )
      } else {
        workingExportSpecifiers.push(
          TS.createExportSpecifier(
            TS.createIdentifier(namedExport.name),
            TS.createIdentifier(exportName),
          ),
        )
      }
    }
  }

  if (workingExportSpecifiers.length > 0) {
    createExportStatementFromWorkingSpecifiers()
  }

  return exportStatements
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
        return TS.createUnparsedSourceFile(e.rawText)
      default:
        const _exhaustiveCheck: never = e
        throw new Error(`Unhandled element type ${JSON.stringify(e)}`)
    }
  })
}

function printCodeImpl(
  printOptions: PrintCodeOptions,
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  jsxFactoryFunction: string | null,
  detailOfExports: ExportsDetail,
): string {
  const importOrigins: Array<string> = Object.keys(imports)
  let importDeclarations: Array<TS.ImportDeclaration> = []
  fastForEach(importOrigins, (importOrigin) => {
    const importForClause = imports[importOrigin]
    const matchingTopLevelElements: ImportStatement[] = topLevelElements.filter(
      (e) => isImportStatement(e) && e.module === importOrigin,
    ) as ImportStatement[]
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
      addCommentsToNode(importDeclaration, importForClause.comments)
      importDeclarations.push(importDeclaration)
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
      addCommentsToNode(importDeclaration, importForClause.comments)
      importDeclarations.push(importDeclaration)
    }
  })

  const lastImportIndex = findLastIndex(isImportStatement, topLevelElements)
  const upToLastImport = lastImportIndex > 0 ? topLevelElements.slice(0, lastImportIndex + 1) : []
  const afterLastImport =
    lastImportIndex > 0 ? topLevelElements.slice(lastImportIndex + 1) : topLevelElements

  const exportStatements = produceExportStatements(detailOfExports)

  const statementsToPrint: Array<TS.Node> = [
    ...printTopLevelElements(printOptions, imports, upToLastImport, detailOfExports),
    ...importDeclarations,
    ...printTopLevelElements(printOptions, imports, afterLastImport, detailOfExports),
    ...exportStatements,
  ]

  return printStatements(statementsToPrint, printOptions.pretty)
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
  name: string
  parameters: TS.NodeArray<TS.ParameterDeclaration>
  body: TS.ConciseBody
  declarationSyntax: FunctionDeclarationSyntax
}

function possibleCanvasContentsFunction(
  name: string,
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
        } else {
          return right(
            possibleCanvasContentsExpression(name, variableDeclaration.initializer, varLetOrConst),
          )
        }
      }
    }
  } else if (TS.isFunctionDeclaration(node)) {
    if (node.name != null && node.body != null) {
      const name = node.name.getText(sourceFile)
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
): Either<TS.ExportAssignment, ExportsDetail> {
  if (TS.isIdentifier(declaration.expression)) {
    const componentIdentifier = declaration.expression
    return right(
      setNamedDefaultExportInDetail(
        exportsDetail(null, {}),
        componentIdentifier.getText(sourceFile),
      ),
    )
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
    const result = exportClause.elements.reduce((workingResult, specifier) => {
      const specifierName = specifier.name.getText(sourceFile)
      if (specifier.propertyName == null) {
        return addNamedExportToDetail(workingResult, specifierName, specifierName, moduleName)
      } else {
        const specifierPropertyName = specifier.propertyName.getText(sourceFile)
        return addNamedExportToDetail(
          workingResult,
          specifierName,
          specifierPropertyName,
          moduleName,
        )
      }
    }, exportsDetail(null, {}))
    return right(result)
  } else {
    return left(declaration)
  }
}

export function getComponentsRenderedWithReactDOM(
  sourceFile: TS.SourceFile,
  node: TS.Node,
): Array<string> {
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

export function parseCode(filename: string, sourceText: string): ParsedTextFile {
  const sourceFile = TS.createSourceFile(filename, sourceText, TS.ScriptTarget.ES3)

  const jsxFactoryFunction = getJsxFactoryFunction(sourceFile)

  if (sourceFile == null) {
    return parseFailure([], null, `File ${filename} not found.`, [])
  } else {
    let topLevelElements: Array<Either<string, TopLevelElement>> = []
    let imports: Imports = emptyImports()
    // Find the already existing UIDs so that when we generate one it doesn't duplicate one
    // existing further ahead.
    const alreadyExistingUIDs: Set<string> = emptySet() // collatedUIDs(sourceFile)
    let highlightBounds: HighlightBoundsForUids = {}

    // As we hit chunks of arbitrary code, shove them here so we can
    // handle them as a block of code.
    let arbitraryNodes: Array<TS.Node> = []
    let allArbitraryNodes: Array<TS.Node> = []
    let arbitraryNodeComments: ParsedComments = emptyComments

    // Account for exported components.
    let detailOfExports: ExportsDetail = EmptyExportsDetail

    function applyAndResetArbitraryNodes(): void {
      const filteredArbitraryNodes = arbitraryNodes.filter(
        (n) => n.kind !== TS.SyntaxKind.EndOfFileToken,
      )
      if (filteredArbitraryNodes.length > 0) {
        const nodeParseResult = parseArbitraryNodes(
          sourceFile,
          sourceText,
          filename,
          filteredArbitraryNodes,
          imports,
          topLevelNames,
          null,
          highlightBounds,
          alreadyExistingUIDs,
          true,
          arbitraryNodeComments,
        )
        topLevelElements.push(
          mapEither((parsed) => {
            highlightBounds = parsed.highlightBounds
            return parsed.value
          }, nodeParseResult),
        )
        allArbitraryNodes = [...allArbitraryNodes, ...filteredArbitraryNodes]
      }
      arbitraryNodes = []
      arbitraryNodeComments = emptyComments
    }

    function pushImportStatement(statement: ImportStatement) {
      topLevelElements.push(right(statement))
      arbitraryNodeComments = emptyComments
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
      } else if (TS.isFunctionDeclaration(node)) {
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
      // Capture the comments so we can attach them to the node
      const comments = getComments(sourceText, topLevelElement)
      function pushArbitraryNode(node: TS.Node) {
        arbitraryNodes.push(node)
        arbitraryNodeComments = mergeParsedComments(arbitraryNodeComments, comments)
      }

      // Handle export assignments: `export default App`
      if (TS.isExportAssignment(topLevelElement)) {
        const fromAssignment = detailsFromExportAssignment(sourceFile, topLevelElement)
        // Parsed it fully, so it can be incorporated.
        forEachRight(fromAssignment, (toMerge) => {
          detailOfExports = mergeExportsDetail(detailOfExports, toMerge)
        })
        // Unable to parse it so treat it as an arbitrary node.
        forEachLeft(fromAssignment, (exportDeclaration) => {
          pushArbitraryNode(exportDeclaration)
        })
        // Handle export declarations.
      } else if (TS.isExportDeclaration(topLevelElement)) {
        const fromDeclaration = detailsFromExportDeclaration(sourceFile, topLevelElement)
        // Parsed it fully, so it can be incorporated.
        forEachRight(fromDeclaration, (toMerge) => {
          detailOfExports = mergeExportsDetail(detailOfExports, toMerge)
        })
        // Unable to parse it so treat it as an arbitrary node.
        forEachLeft(fromDeclaration, (exportDeclaration) => {
          pushArbitraryNode(exportDeclaration)
        })
        // Handle imports.
      } else if (TS.isImportDeclaration(topLevelElement)) {
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
            importFrom,
            importedWithName,
            importedFromWithin,
            importedAs,
            comments,
            imports,
          )
          const rawImportStatement = importStatement(
            topLevelElement.getFullText(sourceFile),
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
              alreadyExistingUIDs,
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
                alreadyExistingUIDs,
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
                alreadyExistingUIDs,
              ),
            )
          }
          if (isLeft(parsedContents) || (isFunction && isLeft(parsedFunctionParam))) {
            pushArbitraryNode(topLevelElement)
            applyAndResetArbitraryNodes() // TODO Should we be doing this every time we push an arbitrary node?
          } else {
            highlightBounds = parsedContents.value.highlightBounds
            const contents = parsedContents.value.value
            // If propsUsed is already populated, it's because the user used destructuring, so we can
            // use that. Otherwise, we have to use the list retrieved during parsing
            propsUsed = propsUsed.length > 0 ? propsUsed : uniq(parsedContents.value.propsUsed)
            if (contents.elements.length === 1) {
              applyAndResetArbitraryNodes()
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
                comments,
                contents.returnStatementComments,
              )

              const defaultExport = isDefaultExport(topLevelElement)
              if (exported) {
                if (defaultExport) {
                  detailOfExports = setModifierDefaultExportInDetail(detailOfExports, name)
                } else {
                  detailOfExports = addModifierExportToDetail(detailOfExports, name)
                }
              }

              topLevelElements.push(right(utopiaComponent))
            } else {
              pushArbitraryNode(topLevelElement)
            }
          }
        } else {
          pushArbitraryNode(topLevelElement)
        }
      }
    }

    applyAndResetArbitraryNodes()

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
        alreadyExistingUIDs,
        true,
        emptyComments,
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
            emptyComments,
          )
        } else {
          return topLevelElement
        }
      })
    }

    return parseSuccess(
      imports,
      topLevelElementsWithFixedUIDs,
      highlightBounds,
      jsxFactoryFunction,
      combinedTopLevelArbitraryBlock,
      detailOfExports,
    )
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
  > =
    param.initializer == null
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

export function getParseResult(
  workers: UtopiaTsWorkers,
  filename: string,
  fileContents: string,
): Promise<ParsedTextFile> {
  return new Promise((resolve, reject) => {
    const handleMessage = (e: MessageEvent) => {
      const data = e.data as ParserPrinterResultMessage
      switch (data.type) {
        case 'parsefileresult': {
          resolve(data.parseResult)
          workers.removeParserPrinterEventListener(handleMessage)
          break
        }
        case 'parseprintfailed': {
          reject()
          workers.removeParserPrinterEventListener(handleMessage)
          break
        }
      }
    }

    workers.addParserPrinterEventListener(handleMessage)
    workers.sendParseFileMessage(filename, fileContents)
  })
}

export function printCodeAsync(
  workers: UtopiaTsWorkers,
  success: ParseSuccess,
): Promise<{ code: string; highlightBounds: HighlightBoundsForUids }> {
  return new Promise((resolve, reject) => {
    const handleMessage = (e: MessageEvent) => {
      const data = e.data as ParserPrinterResultMessage
      switch (data.type) {
        case 'printcoderesult': {
          resolve({ code: data.printResult, highlightBounds: data.highlightBounds })
          workers.removeParserPrinterEventListener(handleMessage)
          break
        }
        case 'parseprintfailed': {
          reject()
          workers.removeParserPrinterEventListener(handleMessage)
          break
        }
      }
    }

    workers.addParserPrinterEventListener(handleMessage)
    workers.sendPrintCodeMessage(success)
  })
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
        withUID(undefined, attributes, undefined, (uid) => {
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
            uid: uid,
          })
        })
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

export function lintAndParse(filename: string, content: string): ParsedTextFile {
  const lintResult = lintCode(filename, content)
  // Only fatal or error messages should bounce the parse.
  if (lintResult.filter(messageisFatal).length === 0) {
    return parseCode(filename, content)
  } else {
    return parseFailure(null, null, null, lintResult)
  }
}
