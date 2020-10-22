import * as TS from 'typescript'
import { TopLevelElement, JSXElement } from '../../shared/element-template'
import { fixUtopiaElement } from '../../shared/uid-utils'
import { fastForEach } from '../../shared/utils'
import { RawSourceMap } from '../ts/ts-typings/RawSourceMap'
import { SourceMapConsumer, SourceNode } from 'source-map'

export interface TopLevelElementAndCodeContext {
  element: TopLevelElement
  bounds: NodesBounds
}

// Checks if the first value is greater than the second one.
export function lineAndCharacterGreaterThan(
  first: TS.LineAndCharacter,
  second: TS.LineAndCharacter,
): boolean {
  if (first.line > second.line) {
    return true
  } else if (first.line === second.line) {
    return first.character > second.character
  } else {
    return false
  }
}

export interface NodesBounds {
  start: TS.LineAndCharacter
  end: TS.LineAndCharacter
}

export function getBoundsOfNodes(
  sourceFile: TS.SourceFile,
  node: TS.Node | Array<TS.Node>,
): NodesBounds {
  let workingStart: TS.LineAndCharacter | null = null as TS.LineAndCharacter | null
  let workingEnd: TS.LineAndCharacter | null = null as TS.LineAndCharacter | null

  let nodes: Array<TS.Node> = []
  if (Array.isArray(node)) {
    if (node.length === 0) {
      throw new Error('Cannot get bounds of empty node array.')
    }
    nodes = node
  } else {
    nodes = [node]
  }
  fastForEach(nodes, (n) => {
    const start = TS.getLineAndCharacterOfPosition(sourceFile, n.getStart(sourceFile))
    if (workingStart == null || lineAndCharacterGreaterThan(workingStart, start)) {
      workingStart = start
    }
    const end = TS.getLineAndCharacterOfPosition(sourceFile, n.end)
    if (workingEnd == null || lineAndCharacterGreaterThan(end, workingEnd)) {
      workingEnd = end
    }
  })
  if (workingStart == null || workingEnd == null) {
    throw new Error('Invalid state, bounds should exist.')
  } else {
    return {
      start: workingStart,
      end: workingEnd,
    }
  }
}

export function guaranteeUniqueUidsFromTopLevel(
  topLevelElements: Array<TopLevelElementAndCodeContext>,
): Array<TopLevelElementAndCodeContext> {
  return topLevelElements.map((tle) => {
    if (tle.element.type === 'UTOPIA_JSX_COMPONENT') {
      return {
        ...tle,
        element: {
          ...tle.element,
          rootElement: fixUtopiaElement(tle.element.rootElement, []),
        },
      }
    } else {
      return tle
    }
  })
}

export interface CodeWithMap {
  code: string
  sourceMap: RawSourceMap
}

function removeTrailingSemicolon(code: string): string {
  const hasTrailingSemicolon = code[code.length - 1] === ';'
  if (hasTrailingSemicolon) {
    return code.slice(0, -1)
  } else {
    return code
  }
}
export function wrapCodeInParens(code: string): string {
  return `(${removeTrailingSemicolon(code)})`
}

export function wrapCodeInParensWithMap(
  sourceFileName: string,
  sourceFileText: string,
  code: string,
  sourceMap: RawSourceMap,
): CodeWithMap {
  const wrappedCode = wrapCodeInParens(code)

  const consumer = new SourceMapConsumer(sourceMap)
  const node = SourceNode.fromStringWithSourceMap(wrappedCode, consumer)
  node.setSourceContent(sourceFileName, sourceFileText)
  const result = node.toStringWithSourceMap({ file: sourceFileName })
  return { code: result.code, sourceMap: result.map }
}

export function prependToSourceString(
  sourceFileName: string,
  sourceFileText: string,
  sourceCode: string,
  sourceMap: RawSourceMap,
  toPrepend: string,
  toAppend: string,
): CodeWithMap {
  const consumer = new SourceMapConsumer(sourceMap)
  const node = SourceNode.fromStringWithSourceMap(sourceCode + toAppend, consumer)
  node.setSourceContent(sourceFileName, sourceFileText)
  node.prepend(toPrepend)
  const { code, map } = node.toStringWithSourceMap({ file: sourceFileName })
  return {
    code: code,
    sourceMap: JSON.parse(map.toString()),
  }
}

export const JSX_CANVAS_LOOKUP_FUNCTION_NAME = 'utopiaCanvasJSXLookup'

interface ElementWithinInPosition {
  uid: string
  element: JSXElement
  startLine: number
  startColumn: number
}

export type ElementsWithinInPosition = Array<ElementWithinInPosition>
