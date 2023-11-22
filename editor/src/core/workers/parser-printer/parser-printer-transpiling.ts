import * as Babel from '@babel/standalone'
import type * as BabelTraverse from '@babel/traverse'
import * as BabelTypes from '@babel/types'
import ReactSyntaxPlugin from 'babel-plugin-syntax-jsx'
import ReactTransformPlugin from 'babel-plugin-transform-react-jsx'
import type { SourceNode } from 'source-map'
import type { Either } from '../../shared/either'
import { left, right } from '../../shared/either'
import type { JSXElement } from '../../shared/element-template'
import {
  getDefinedElsewhereFromElement,
  getJSXElementNameAsString,
} from '../../shared/element-template'
import { getUtopiaIDFromJSXElement } from '../../shared/uid-utils'
import { fastForEach } from '../../shared/utils'
import type { RawSourceMap } from '../ts/ts-typings/RawSourceMap'
import infiniteLoopPrevention from './transform-prevent-infinite-loops'
import type { ElementsWithinInPosition, CodeWithMap } from './parser-printer-utils'
import { wrapCodeInParens, wrapCodeInParensWithMap } from './parser-printer-utils'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../shared/dom-utils'
import type { SteganoTextData } from '../../shared/stegano-text'
import { cleanSteganoTextData, encodeSteganoTextData } from '../../shared/stegano-text'
import { SourceMapConsumer } from 'source-map'
import type { SteganographyMode } from './parser-printer'

interface TranspileResult {
  code: string
  sourceMap: RawSourceMap
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
    elementWithin: JSXElement,
    uid: string,
  ): void {
    if (insertCanvasRenderCall) {
      const functionIdentifier = BabelTypes.identifier(JSX_CANVAS_LOOKUP_FUNCTION_NAME)
      const definedElsewhere = getDefinedElsewhereFromElement(elementWithin)
      const passthroughProps = definedElsewhere.map((elsewhere) => {
        return BabelTypes.objectProperty(
          BabelTypes.identifier(elsewhere),
          BabelTypes.identifier(elsewhere),
        )
      })
      const definedElsewherePassthrough = BabelTypes.objectExpression(
        passthroughProps.concat(
          BabelTypes.objectProperty(
            BabelTypes.identifier('callerThis'),
            BabelTypes.thisExpression(),
          ),
        ),
      )
      const callArguments = [BabelTypes.stringLiteral(uid), definedElsewherePassthrough]
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
    if (typeof value === 'object' && BabelTypes.isStringLiteral(value)) {
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
        return tagName === getJSXElementNameAsString(e.element.name)
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

export function transpileJavascript(
  sourceFileName: string,
  sourceFileText: string,
  fileSourceNode: typeof SourceNode,
  elementsWithin: ElementsWithinInPosition,
  wrapInParens: boolean,
  applySteganography: SteganographyMode,
): Either<string, TranspileResult> {
  try {
    const { code, map } = fileSourceNode.toStringWithSourceMap({ file: sourceFileName })
    const rawMap = JSON.parse(map.toString())
    return transpileJavascriptFromCode(
      sourceFileName,
      sourceFileText,
      code,
      rawMap,
      elementsWithin,
      wrapInParens,
      applySteganography,
    )
  } catch (e: any) {
    return left(e.message)
  }
}

export function insertDataUIDsIntoCode(
  code: string,
  elementsWithin: ElementsWithinInPosition,
  wrapInParens: boolean,
  rootLevel: boolean,
  filename: string,
): Either<string, CodeWithMap> {
  try {
    let codeToUse: string = code
    if (wrapInParens) {
      codeToUse = wrapCodeInParens(codeToUse)
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

        path.replaceWith(BabelTypes.stringLiteral(encodeSteganoTextData(cleanedNodeValue, data)))
      },
    },
  })
}

export function transpileJavascriptFromCode(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  map: RawSourceMap,
  elementsWithin: ElementsWithinInPosition,
  wrapInParens: boolean,
  applySteganography: SteganographyMode,
): Either<string, TranspileResult> {
  try {
    let codeToUse: string = code
    let mapToUse: RawSourceMap = map

    if (wrapInParens) {
      const wrappedInParens = wrapCodeInParensWithMap(
        sourceFileName,
        sourceFileText,
        codeToUse,
        mapToUse,
      )
      codeToUse = wrappedInParens.code
      mapToUse = wrappedInParens.sourceMap
    }

    let plugins: Array<any> =
      applySteganography === 'apply-steganography'
        ? [applySteganographyPlugin(sourceFileName, mapToUse, sourceFileText.split('\n'))]
        : []
    if (Object.keys(elementsWithin).length > 0) {
      plugins.push(babelRewriteJSExpressionCode(elementsWithin, true))
    }
    plugins.push(infiniteLoopPrevention)
    plugins.push('external-helpers')
    plugins.push('transform-typescript')
    plugins.push('transform-react-jsx')
    plugins.push('proposal-class-properties')
    plugins.push(ReactTransformPlugin)
    const transformResult = Babel.transform(codeToUse, {
      presets: ['es2015', 'react'],
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
