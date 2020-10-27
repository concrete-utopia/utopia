import * as React from 'react'
import * as TS from 'typescript'
import {
  BakedInStoryboardVariableName,
  convertTopLevelElementsBackToScenesAndTopLevelElements_FOR_PP_ONLY,
  EmptyUtopiaCanvasComponent,
  convertUtopiaCanvasComponentToScenes,
} from '../../model/scene-utils'
import { addUniquely, flatMapArray, uniq, uniqBy, sortBy } from '../../shared/array-utils'
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
} from '../../shared/element-template'
import { messageisFatal } from '../../shared/error-messages'
import { memoize } from '../../shared/memoize'
import { defaultIfNull, maybeToArray, optionalMap } from '../../shared/optional-utils'
import {
  HighlightBoundsForUids,
  ImportAlias,
  importAlias,
  Imports,
  ParseResult,
  ParseSuccess,
  PropertyPathPart,
  isParsedJSONSuccess,
  HighlightBounds,
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
  isExported,
  liftParsedElementsIntoFunctionContents,
  parseArbitraryNodes,
  parseOutFunctionContents,
  parseOutJSXElements,
  WithParserMetadata,
  parseAttributeOtherJavaScript,
  withParserMetadata,
} from './parser-printer-parsing'
import {
  getBoundsOfNodes,
  guaranteeUniqueUidsFromTopLevel,
  TopLevelElementAndCodeContext,
} from './parser-printer-utils'
import { ParserPrinterResultMessage } from './parser-printer-worker'
import creator from './ts-creator'
import { applyPrettier } from './prettier-utils'
import { jsonToExpression } from './json-to-expression'

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

function jsxAttributeToExpression(attribute: JSXAttribute): TS.Expression {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      if (typeof attribute.value === 'string') {
        return TS.createLiteral(attribute.value)
      } else {
        return jsonToExpression(attribute.value)
      }
    case 'ATTRIBUTE_NESTED_OBJECT':
      const contents = attribute.content
      const objectPropertyExpressions: Array<TS.ObjectLiteralElementLike> = contents.map((prop) => {
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
      })
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
        { ts: TS, React: React },
        `return ${createExpressionAsString}.statements[0].expression`,
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
            if (prop.value) {
              // The `any` allows our `undefined` to punch through the invalid typing
              // of `createJsxAttribute`.
              attribsArray.push(TS.createJsxAttribute(identifier, undefined as any))
            }
            // No else case here as a boolean false value means it gets omitted.
          } else {
            const attributeExpression = jsxAttributeToExpression(prop)
            const initializer: TS.StringLiteral | TS.JsxExpression = TS.createJsxExpression(
              undefined,
              attributeExpression,
            )
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
        { ts: TS, React: React },
        `var node = ${createExpressionAsString}.statements[0]; return node.expression || node`,
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

function printUtopiaJSXComponent(
  printOptions: PrintCodeOptions,
  imports: Imports,
  element: UtopiaJSXComponent,
): TS.Node {
  const asJSX = jsxElementToExpression(element.rootElement, imports, printOptions.stripUIDs)
  if (TS.isJsxElement(asJSX) || TS.isJsxSelfClosingElement(asJSX)) {
    if (element.isFunction) {
      const arrowParams = maybeToArray(element.param).map(printParam)
      let statements: Array<TS.Statement> = []
      if (element.arbitraryJSBlock != null) {
        // I just punched a hole in the space time continuum with this cast to any.
        // Somehow it works, I assume because it really only cares about Nodes internally and
        // not Statements but I will deny all knowledge of ever having done this.
        statements.push(printArbitraryJSBlock(element.arbitraryJSBlock) as any)
      }
      statements.push(TS.createReturn(asJSX))
      const bodyBlock = TS.createBlock(statements, true)
      const arrowFunction = TS.createArrowFunction(
        undefined,
        undefined,
        arrowParams,
        undefined,
        undefined,
        bodyBlock,
      )
      const varDec = TS.createVariableDeclaration(element.name, undefined, arrowFunction)
      return TS.createVariableStatement([TS.createToken(TS.SyntaxKind.ExportKeyword)], [varDec])
    } else {
      const varDec = TS.createVariableDeclaration(element.name, undefined, asJSX)
      return TS.createVariableStatement([TS.createToken(TS.SyntaxKind.ExportKeyword)], [varDec])
    }
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

function printArbitraryJSBlock(block: ArbitraryJSBlock): TS.Node {
  return TS.createUnparsedSourceFile(block.javascript)
}

export const printCode = memoize(printCodeImpl, {
  equals: (a, b) => a === b,
  maxSize: 1,
})

function printStatements(statements: Array<TS.Node>, shouldPrettify: boolean): string {
  const printer = TS.createPrinter({ newLine: TS.NewLineKind.LineFeed })
  const resultFile = TS.createSourceFile(
    'print.ts',
    '',
    TS.ScriptTarget.Latest,
    false,
    TS.ScriptKind.TS,
  )
  const printFlags =
    TS.ListFormat.Indented |
    TS.ListFormat.MultiLine |
    TS.ListFormat.SpaceBetweenBraces |
    TS.ListFormat.MultiLineBlockStatements |
    TS.ListFormat.PreferNewLine |
    TS.ListFormat.MultiLineFunctionBodyStatements |
    TS.ListFormat.MultiLineTypeLiteralMembers
  const typescriptPrintedResult = printer.printList(
    printFlags,
    TS.createNodeArray(statements),
    resultFile,
  )
  let result: string
  if (shouldPrettify) {
    result = applyPrettier(typescriptPrintedResult, false).formatted
  } else {
    result = typescriptPrintedResult
  }
  return result
}

function createJsxPragma(jsxFactoryFunction: string | null): string {
  if (jsxFactoryFunction == null) {
    return ''
  } else {
    return `/** @jsx ${jsxFactoryFunction} */\n`
  }
}

function printCodeImpl(
  printOptions: PrintCodeOptions,
  imports: Imports,
  topLevelElementsIncludingScenes: Array<TopLevelElement>,
  jsxFactoryFunction: string | null,
): string {
  // if the project used the old SceneMetadata notation, strip out the Storyboard component here
  const topLevelElements = topLevelElementsIncludingScenes

  const importOrigins: Array<string> = Object.keys(imports)
  importOrigins.sort()
  let importDeclarations: Array<TS.ImportDeclaration> = []
  fastForEach(importOrigins, (importOrigin) => {
    const importForClause = imports[importOrigin]

    if (importForClause.importedWithName != null || importForClause.importedFromWithin.length > 0) {
      let importedWithName: TS.Identifier | undefined = undefined
      if (importForClause.importedWithName != null) {
        importedWithName = TS.createIdentifier(importForClause.importedWithName)
      }
      // Importing parts or the whole module by name.
      const importClause = TS.createImportClause(
        importedWithName,
        importForClause.importedFromWithin.length === 0
          ? undefined
          : TS.createNamedImports(
              importForClause.importedFromWithin.map((i) => {
                return TS.createImportSpecifier(
                  TS.createIdentifier(i.name),
                  TS.createIdentifier(i.alias),
                )
              }),
            ),
      )

      importDeclarations.push(
        TS.createImportDeclaration(
          undefined,
          undefined,
          importClause,
          TS.createStringLiteral(importOrigin),
        ),
      )
    }

    if (importForClause.importedAs != null) {
      // Wildcard import of the package.
      const wildcardClause = TS.createImportClause(
        undefined,
        TS.createNamespaceImport(TS.createIdentifier(importForClause.importedAs)),
      )
      importDeclarations.push(
        TS.createImportDeclaration(
          undefined,
          undefined,
          wildcardClause,
          TS.createStringLiteral(importOrigin),
        ),
      )
    }

    if (
      importForClause.importedAs == null &&
      importForClause.importedWithName == null &&
      importForClause.importedFromWithin.length === 0
    ) {
      // side-effect only import ( `import './style.css'` )
      importDeclarations.push(
        TS.createImportDeclaration(
          undefined,
          undefined,
          undefined,
          TS.createStringLiteral(importOrigin),
        ),
      )
    }
  })

  const topLevelStatements = topLevelElements.map((e) => {
    switch (e.type) {
      case 'UTOPIA_JSX_COMPONENT':
        return printUtopiaJSXComponent(printOptions, imports, e)
      case 'ARBITRARY_JS_BLOCK':
        return printArbitraryJSBlock(e)
      default:
        const _exhaustiveCheck: never = e
        throw new Error(`Unhandled element type ${JSON.stringify(e)}`)
    }
  })

  let statementsToPrint: Array<TS.Node> = []
  statementsToPrint.push(...importDeclarations)

  statementsToPrint.push(...topLevelStatements)
  const printedCode = printStatements(statementsToPrint, printOptions.pretty)
  const jsxPragma = createJsxPragma(jsxFactoryFunction)
  // we just dumbly append the parsed jsx pragma to the top of the file, no matter where it was originally
  const result = jsxPragma + printedCode
  return result
}

interface PossibleCanvasContentsExpression {
  name: string
  initializer: TS.Expression
}

interface PossibleCanvasContentsFunction {
  name: string
  parameters: TS.NodeArray<TS.ParameterDeclaration>
  body: TS.ConciseBody
}

type PossibleCanvasContents = PossibleCanvasContentsExpression | PossibleCanvasContentsFunction

function isPossibleCanvasContentsFunction(
  canvasContents: PossibleCanvasContents,
): canvasContents is PossibleCanvasContentsFunction {
  return (canvasContents as PossibleCanvasContentsFunction).body != null
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
        if (TS.isArrowFunction(initializer)) {
          return right({ name: name, parameters: initializer.parameters, body: initializer.body })
        } else {
          return right({ name: name, initializer: variableDeclaration.initializer })
        }
      }
    }
  } else if (TS.isFunctionDeclaration(node)) {
    if (node.name != null && node.body != null) {
      const name = node.name.getText(sourceFile)
      return right({ name: name, parameters: node.parameters, body: node.body })
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

export function parseCode(filename: string, sourceText: string): ParseResult {
  const sourceFile = TS.createSourceFile(filename, sourceText, TS.ScriptTarget.ES3)

  const jsxFactoryFunction = getJsxFactoryFunction(sourceFile)

  if (sourceFile == null) {
    return left(parseFailure([], null, `File ${filename} not found.`, [], ''))
  } else {
    const code = sourceFile.text
    let topLevelElements: Array<Either<string, TopLevelElementAndCodeContext>> = []
    let imports: Imports = emptyImports()
    // Find the already existing UIDs so that when we generate one it doesn't duplicate one
    // existing further ahead.
    const alreadyExistingUIDs: ReadonlyArray<string> = collatedUIDs(sourceFile)
    let highlightBounds: HighlightBoundsForUids = {}

    // As we hit chunks of arbitrary code, shove them here so we can
    // handle them as a block of code.
    let arbitraryNodes: Array<TS.Node> = []
    let allArbitraryNodes: Array<TS.Node> = []

    function applyAndResetArbitraryNodes(): void {
      const filteredArbitraryNodes = arbitraryNodes.filter(
        (n) => n.kind !== TS.SyntaxKind.EndOfFileToken,
      )
      if (filteredArbitraryNodes.length > 0) {
        const nodeParseResult = parseArbitraryNodes(
          sourceFile,
          filename,
          filteredArbitraryNodes,
          imports,
          topLevelNames,
          null,
          highlightBounds,
          alreadyExistingUIDs,
          true,
        )
        topLevelElements.push(
          mapEither((parsed) => {
            highlightBounds = parsed.highlightBounds
            const bounds = getBoundsOfNodes(sourceFile, filteredArbitraryNodes)
            return {
              element: parsed.value,
              bounds: bounds,
            }
          }, nodeParseResult),
        )
        allArbitraryNodes = [...allArbitraryNodes, ...filteredArbitraryNodes]
      }
      arbitraryNodes = []
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
      // Handle imports.
      if (TS.isImportDeclaration(topLevelElement)) {
        if (TS.isStringLiteral(topLevelElement.moduleSpecifier)) {
          const importClause: TS.ImportClause | undefined = topLevelElement.importClause
          const importFrom: string = topLevelElement.moduleSpecifier.text
          let importedFromWithin: Array<ImportAlias> = []
          let importedAs: string | null = null
          // this import looks like `import Cat from './src/cats'`
          const importedWithName = optionalMap((n) => n.getText(sourceFile), importClause?.name)
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
          importedFromWithin.sort((i1, i2) => {
            if (i1.name < i2.name) {
              return -1
            }
            if (i1.name > i2.name) {
              return 1
            }
            return 0
          })
          imports = addImport(importFrom, importedWithName, importedFromWithin, importedAs, imports)
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
              parseOutJSXElements(
                sourceFile,
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
            arbitraryNodes.push(topLevelElement)
          } else {
            highlightBounds = parsedContents.value.highlightBounds
            const contents = parsedContents.value.value
            // If propsUsed is already populated, it's because the user used destructuring, so we can
            // use that. Otherwise, we have to use the list retrieved during parsing
            propsUsed = propsUsed.length > 0 ? propsUsed : uniq(parsedContents.value.propsUsed)
            if (contents.elements.length === 1) {
              applyAndResetArbitraryNodes()
              const utopiaComponent = utopiaJSXComponent(
                name,
                isFunction,
                foldEither(
                  (_) => null,
                  (param) => param?.value ?? null,
                  parsedFunctionParam,
                ),
                propsUsed,
                contents.elements[0].value,
                contents.arbitraryJSBlock,
              )
              const bounds = getBoundsOfNodes(sourceFile, topLevelElement)
              topLevelElements.push(
                right({
                  element: utopiaComponent,
                  bounds: bounds,
                }),
              )
            } else {
              arbitraryNodes.push(topLevelElement)
            }
          }
        } else {
          arbitraryNodes.push(topLevelElement)
        }
      }
    }

    applyAndResetArbitraryNodes()

    const sequencedTopLevelElements = sequenceEither(topLevelElements)
    if (isLeft(sequencedTopLevelElements)) {
      return left(parseFailure(null, null, sequencedTopLevelElements.value, [], code))
    }
    const realTopLevelElements = sequencedTopLevelElements.value

    const topLevelElementsWithFixedUIDs = guaranteeUniqueUidsFromTopLevel(realTopLevelElements)

    const topLevelElementsIncludingScenes = topLevelElementsWithFixedUIDs.map((e) => e.element)

    let combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null = null
    if (allArbitraryNodes.length > 0) {
      const nodeParseResult = parseArbitraryNodes(
        sourceFile,
        filename,
        allArbitraryNodes,
        imports,
        topLevelNames,
        null,
        highlightBounds,
        alreadyExistingUIDs,
        true,
      )
      forEachRight(nodeParseResult, (nodeParseSuccess) => {
        combinedTopLevelArbitraryBlock = nodeParseSuccess.value
      })
    }

    return right(
      parseSuccess(
        imports,
        topLevelElementsIncludingScenes,
        code,
        highlightBounds,
        jsxFactoryFunction,
        combinedTopLevelArbitraryBlock,
      ),
    )
  }
}

function parseParams(
  params: TS.NodeArray<TS.ParameterDeclaration>,
  file: TS.SourceFile,
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: ReadonlyArray<string>,
): Either<string, WithParserMetadata<Array<Param>>> {
  let parsedParams: Array<Param> = []
  let highlightBounds: HighlightBoundsForUids = { ...existingHighlightBounds }
  let propsUsed: Array<string> = []
  for (const param of params) {
    const parseResult = parseParam(
      param,
      file,
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
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: ReadonlyArray<string>,
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
  filename: string,
  imports: Imports,
  topLevelNames: Array<string>,
  existingHighlightBounds: Readonly<HighlightBoundsForUids>,
  existingUIDs: ReadonlyArray<string>,
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
): Promise<ParseResult> {
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

function collatedUIDs(sourceFile: TS.SourceFile): Array<string> {
  let result: Array<string> = []
  function addUID(boundingElement: TS.Node, attributes: TS.JsxAttributes): void {
    withUID(undefined, attributes, undefined, (uid) => {
      result = addUniquely(result, uid)
    })
  }
  withJSXElementAttributes(sourceFile, addUID)
  return result
}

export function lintAndParse(filename: string, content: string): ParseResult {
  const lintResult = lintCode(filename, content)
  // Only fatal or error messages should bounce the parse.
  if (lintResult.filter(messageisFatal).length === 0) {
    return parseCode(filename, content)
  } else {
    return left(parseFailure(null, null, null, lintResult, content))
  }
}
