import * as TS from 'typescript'
import {
  isParsedJSONFailure,
  ParsedJSONFailure,
  ParsedJSONResult,
} from '../../shared/project-file-types'
import { parsedJSONFailure, parsedJSONSuccess } from '../common/project-file-utils'
import { createCodeSnippet } from '../ts/ts-utils'

export function jsonToExpression(json: any): TS.Expression {
  switch (typeof json) {
    case 'object':
      if (json == null) {
        return TS.createNull()
      }
      if (Array.isArray(json)) {
        const arrayExpressions: Array<TS.Expression> = json.map(jsonToExpression)
        return TS.createArrayLiteral(arrayExpressions)
      } else {
        const keys = Object.keys(json)
        const objectPropertyExpressions: Array<TS.ObjectLiteralElementLike> = keys.map((key) => {
          return TS.createPropertyAssignment(
            TS.createStringLiteral(key),
            jsonToExpression(json[key]),
          )
        })
        return TS.createObjectLiteral(objectPropertyExpressions)
      }
    case 'string':
      return TS.createStringLiteral(json)
    case 'boolean':
      return json ? TS.createTrue() : TS.createFalse()
    case 'number':
      if (json < 0) {
        return TS.createPrefix(TS.SyntaxKind.MinusToken, TS.createLiteral(json * -1))
      } else {
        return TS.createLiteral(json)
      }
    case 'undefined':
      return TS.createIdentifier('undefined')
    default:
      throw new Error(`Unsure how to handle ${JSON.stringify(json)}`)
  }
}

function parseJSONObject(
  sourceFile: TS.SourceFile,
  node: TS.ObjectLiteralExpression,
): ParsedJSONResult {
  let result: { [key: string]: any } = {}
  for (const property of node.properties) {
    if (TS.isPropertyAssignment(property)) {
      const propertyResult = parseJSON(sourceFile, property.initializer)
      if (isParsedJSONFailure(propertyResult)) {
        return propertyResult
      } else {
        const propertyName = TS.isStringLiteral(property.name)
          ? property.name.text
          : property.name.getText(sourceFile)
        result[propertyName] = propertyResult.value
      }
    } else {
      return createJsonError(sourceFile, node, 'expected property assignment')
    }
  }
  return parsedJSONSuccess(result)
}

function parseJSONArray(
  sourceFile: TS.SourceFile,
  node: TS.ArrayLiteralExpression,
): ParsedJSONResult {
  let result: Array<any> = []
  for (const child of node.elements) {
    const childResult = parseJSON(sourceFile, child)
    if (isParsedJSONFailure(childResult)) {
      return childResult
    } else {
      result.push(childResult.value)
    }
  }
  return parsedJSONSuccess(result)
}

function parsePrefixUnaryExpression(
  sourceFile: TS.SourceFile,
  node: TS.PrefixUnaryExpression,
): ParsedJSONResult {
  // Cater for negative numbers, because of course they're done in a weird way.
  const operator = node.operator
  if (operator === TS.SyntaxKind.MinusToken) {
    const operand = node.operand
    if (TS.isNumericLiteral(operand)) {
      return parsedJSONSuccess(Number.parseFloat(operand.getText(sourceFile)) * -1)
    }
  }
  return createJsonError(sourceFile, node, `expected '-' prefix for negative number`)
}

function parseJSON(sourceFile: TS.SourceFile, node: TS.Node): ParsedJSONResult {
  if (TS.isObjectLiteralExpression(node)) {
    return parseJSONObject(sourceFile, node)
  } else if (TS.isArrayLiteralExpression(node)) {
    return parseJSONArray(sourceFile, node)
  } else if (TS.isStringLiteral(node)) {
    return parsedJSONSuccess(node.text)
  } else if (TS.isNumericLiteral(node)) {
    return parsedJSONSuccess(Number.parseFloat(node.getText(sourceFile)))
  } else if (TS.isPrefixUnaryExpression(node)) {
    return parsePrefixUnaryExpression(sourceFile, node)
  } else {
    switch (node.kind) {
      case TS.SyntaxKind.TrueKeyword:
        return parsedJSONSuccess(true)
      case TS.SyntaxKind.FalseKeyword:
        return parsedJSONSuccess(false)
      case TS.SyntaxKind.NullKeyword:
        return parsedJSONSuccess(null)
      default:
        return createJsonError(sourceFile, node, 'invalid content for JSON')
    }
  }
}

function createJsonError(
  sourceFile: TS.SourceFile,
  node: TS.Node,
  reason: string,
): ParsedJSONFailure {
  const nodeStart = node.getStart(sourceFile)
  const nodeEnd = node.getEnd()
  const codeSnippet = createCodeSnippet(sourceFile, nodeStart, nodeEnd)
  let { line: startLine, character: startCol } = sourceFile.getLineAndCharacterOfPosition(nodeStart)
  let { line: endLine, character: endCol } = sourceFile.getLineAndCharacterOfPosition(nodeEnd)

  return parsedJSONFailure(
    node,
    codeSnippet,
    reason,
    startLine + 1, // typescript lines and cols are zero-based, correcting it to be one-based
    startCol + 1,
    endLine + 1,
    endCol + 1,
  )
}
