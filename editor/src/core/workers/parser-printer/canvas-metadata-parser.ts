import * as TS from 'typescript'
import {
  AllFlexAlignments,
  AllFlexDirections,
  AllFlexJustifyContents,
  AllFlexWraps,
  FlexAlignment,
  FlexDirection,
  FlexJustifyContent,
  FlexWrap,
  LayoutSystem,
  NormalisedFrame,
} from 'utopia-api'
import {
  descriptionParseError,
  objectKeyParser,
  optionalObjectKeyParser,
  parseAny,
  parseArray,
  parseEnum,
  parseNullable,
  parseNumber,
  parseObject,
  ParseResult,
  parseString,
} from '../../../utils/value-parser-utils'
import {
  convertScenesToUtopiaCanvasComponent,
  createSceneUidFromIndex,
} from '../../model/scene-utils'
import {
  applicative2Either,
  applicative4Either,
  applicative5Either,
  applicative8Either,
  Either,
  foldEither,
  left,
  right,
} from '../../shared/either'
import { UtopiaJSXComponent } from '../../shared/element-template'
import {
  CanvasElementMetadataMap,
  CanvasMetadata,
  CanvasMetadataRightBeforePrinting,
  ElementCanvasMetadata,
  isParsedJSONFailure,
  ParsedJSONResult,
  PrintedCanvasMetadata,
  SceneContainer,
  SceneFlexContainer,
  SceneMetadata,
} from '../../shared/project-file-types'
import { parsedJSONFailure, parsedJSONSuccess } from '../common/project-file-utils'
import { createCodeSnippet } from '../ts/ts-utils'

function parseFlexDirection(value: unknown): ParseResult<FlexDirection> {
  return parseEnum(AllFlexDirections)(value)
}

function parseFlexAlignment(value: unknown): ParseResult<FlexAlignment> {
  return parseEnum(AllFlexAlignments)(value)
}

function parseFlexJustifyContent(value: unknown): ParseResult<FlexJustifyContent> {
  return parseEnum(AllFlexJustifyContents)(value)
}

function parseFlexWrap(value: unknown): ParseResult<FlexWrap> {
  return parseEnum(AllFlexWraps)(value)
}

function parseNormalisedFrame(value: unknown): ParseResult<NormalisedFrame> {
  return applicative4Either(
    (frameLeft, frameTop, frameWidth, frameHeight) => {
      return {
        left: frameLeft,
        top: frameTop,
        width: frameWidth,
        height: frameHeight,
      }
    },
    objectKeyParser(parseNumber, 'left')(value),
    objectKeyParser(parseNumber, 'top')(value),
    objectKeyParser(parseNumber, 'width')(value),
    objectKeyParser(parseNumber, 'height')(value),
  )
}

function parseSceneFlexContainer(value: unknown): ParseResult<SceneFlexContainer> {
  return applicative2Either(
    (otherPropsApplicative, paddingApplicative) =>
      ({
        ...otherPropsApplicative,
        ...paddingApplicative,
      } as SceneFlexContainer),
    applicative4Either(
      (paddingTop, paddingRight, paddingBottom, paddingLeft) => {
        let sceneFlexContainer: Partial<SceneFlexContainer> = {}
        if (paddingTop != null) {
          sceneFlexContainer.paddingTop = paddingTop
        }
        if (paddingRight != null) {
          sceneFlexContainer.paddingRight = paddingRight
        }
        if (paddingBottom != null) {
          sceneFlexContainer.paddingBottom = paddingBottom
        }
        if (paddingLeft != null) {
          sceneFlexContainer.paddingLeft = paddingLeft
        }
        return sceneFlexContainer
      },
      objectKeyParser(parseNullable(parseNumber), 'paddingTop')(value),
      objectKeyParser(parseNullable(parseNumber), 'paddingRight')(value),
      objectKeyParser(parseNullable(parseNumber), 'paddingBottom')(value),
      objectKeyParser(parseNullable(parseNumber), 'paddingLeft')(value),
    ),
    applicative8Either(
      (
        layoutSystem,
        flexDirection,
        alignContent,
        alignItems,
        justifyContent,
        flexWrap,
        gapCross,
        gapMain,
      ) => {
        let sceneFlexContainer: SceneFlexContainer = {
          layoutSystem,
        }
        if (flexDirection != null) {
          sceneFlexContainer.flexDirection = flexDirection
        }
        if (alignContent != null) {
          sceneFlexContainer.alignContent = alignContent
        }
        if (alignItems != null) {
          sceneFlexContainer.alignItems = alignItems
        }
        if (justifyContent != null) {
          sceneFlexContainer.justifyContent = justifyContent
        }
        if (flexWrap != null) {
          sceneFlexContainer.flexWrap = flexWrap
        }
        if (gapCross != null) {
          sceneFlexContainer.gapCross = gapCross
        }
        if (gapMain != null) {
          sceneFlexContainer.gapMain = gapMain
        }
        return sceneFlexContainer
      },
      objectKeyParser(parseEnum([LayoutSystem.Flex]), 'layoutSystem')(value),
      objectKeyParser(parseNullable(parseFlexDirection), 'flexDirection')(value),
      objectKeyParser(parseNullable(parseFlexAlignment), 'alignContent')(value),
      objectKeyParser(parseNullable(parseFlexAlignment), 'alignItems')(value),
      objectKeyParser(parseNullable(parseFlexJustifyContent), 'justifyContent')(value),
      objectKeyParser(parseNullable(parseFlexWrap), 'wrap')(value),
      objectKeyParser(parseNullable(parseNumber), 'gapCross')(value),
      objectKeyParser(parseNullable(parseNumber), 'gapMain')(value),
    ),
  )
}

function parseSceneContainer(value: unknown): ParseResult<SceneContainer> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    const valueAsObject: any = value
    switch (valueAsObject.layoutSystem) {
      case LayoutSystem.PinSystem:
        return right({
          layoutSystem: LayoutSystem.PinSystem,
        })
      case LayoutSystem.Flex:
        return parseSceneFlexContainer(value)
      default:
        return left(descriptionParseError('Scene container has an invalid layout type.'))
    }
  } else {
    return left(descriptionParseError('Scene container is not an object.'))
  }
}

function parseSceneMetadata(value: unknown, index: number): ParseResult<SceneMetadata> {
  return applicative5Either(
    (component, props, frame, container, label) => {
      var sceneMetadata: SceneMetadata = {
        uid: createSceneUidFromIndex(index),
        component: component,
        props: props,
        frame: frame,
        container: container,
      }
      if (label != null) {
        sceneMetadata.label = label
      }
      return sceneMetadata
    },
    objectKeyParser(parseNullable(parseString), 'component')(value),
    objectKeyParser(parseObject(parseAny), 'props')(value),
    objectKeyParser(parseNormalisedFrame, 'frame')(value),
    objectKeyParser(parseSceneContainer, 'container')(value),
    optionalObjectKeyParser(parseString, 'label')(value),
  )
}

function parseElementCanvasMetadata(value: unknown): ParseResult<ElementCanvasMetadata> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    return right({})
  } else {
    return left(descriptionParseError('Canvas metadata is not an object.'))
  }
}

function parseCanvasElementMetadataMap(value: unknown): ParseResult<CanvasElementMetadataMap> {
  return parseObject(parseElementCanvasMetadata)(value)
}

export function parsePrintedCanvasMetadata(value: unknown): ParseResult<PrintedCanvasMetadata> {
  return applicative2Either(
    (scenes, elementMetadata) => {
      return {
        scenes: scenes,
        elementMetadata: elementMetadata,
      }
    },
    objectKeyParser(parseNullable(parseArray(parseSceneMetadata)), 'scenes')(value),
    objectKeyParser(parseCanvasElementMetadataMap, 'elementMetadata')(value),
  )
}

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

export function canvasMetadataToExpression(
  canvasMetadata: CanvasMetadataRightBeforePrinting,
): TS.Expression {
  return jsonToExpression(canvasMetadata)
}

export function parseCanvasMetadata(sourceFile: TS.SourceFile, node: TS.Node): ParsedJSONResult {
  const unprocessedCanvasMetadata = parseJSON(sourceFile, node)
  if (unprocessedCanvasMetadata.type === 'FAILURE') {
    return unprocessedCanvasMetadata
  } else {
    const canvasMetadata: CanvasMetadata = unprocessedCanvasMetadata.value
    return parsedJSONSuccess(canvasMetadata)
  }
}

export function convertPrintedMetadataToCanvasMetadata(
  metadata: Either<unknown, PrintedCanvasMetadata>,
): {
  sanitizedCanvasMetadata: Either<unknown, CanvasMetadata>
  utopiaComponentFromSceneMetadata: UtopiaJSXComponent | null
} {
  return foldEither(
    (l) => ({ sanitizedCanvasMetadata: left(l), utopiaComponentFromSceneMetadata: null }),
    (printedMetadata: PrintedCanvasMetadata) => ({
      sanitizedCanvasMetadata: right({}),
      utopiaComponentFromSceneMetadata: convertScenesToUtopiaCanvasComponent(
        printedMetadata.scenes,
      ),
    }),
    metadata,
  )
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

function createJsonError(sourceFile: TS.SourceFile, node: TS.Node, reason: string) {
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
