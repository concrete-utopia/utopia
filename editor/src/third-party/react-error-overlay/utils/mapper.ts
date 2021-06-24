/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import StackFrame from './stack-frame'
import { getSourceMap, SourceMap } from './getSourceMap'
import { getLinesAround } from './getLinesAround'
import { settle } from 'settle-promise'
import { RETURN_TO_PREPEND } from '../../../core/shared/utils'

/**
 * Enhances a set of <code>StackFrame</code>s with their original positions and code (when available).
 * @param {StackFrame[]} frames A set of <code>StackFrame</code>s which contain (generated) code positions.
 * @param {number} [contextLines=3] The number of lines to provide before and after the line specified in the <code>StackFrame</code>.
 */
async function map(frames: StackFrame[], contextLines: number = 3): Promise<StackFrame[]> {
  const cache: any = {}
  const files: string[] = []
  frames.forEach((frame) => {
    const { fileName } = frame
    if (fileName == null) {
      return
    }
    if (files.indexOf(fileName) !== -1) {
      return
    }
    files.push(fileName)
  })
  await settle(
    files.map(async (fileName) => {
      const fetchUrl =
        fileName.indexOf('webpack-internal:') === 0
          ? `/__get-internal-source?fileName=${encodeURIComponent(fileName)}`
          : fileName

      const fileSource = await fetch(fetchUrl).then((r) => r.text())
      const map = await getSourceMap(fileName, fileSource)
      cache[fileName] = { fileSource, map }
    }),
  )
  return frames.map((frame) => {
    const { functionName, fileName, lineNumber, columnNumber } = frame
    let { map, fileSource } = cache[fileName!] || ({} as any)
    if (map == null || lineNumber == null) {
      return frame
    }
    const { source, line, column } = map.getOriginalPosition(lineNumber, columnNumber)
    const originalSource = source == null ? [] : map.getSource(source)
    return new StackFrame(
      functionName,
      fileName,
      lineNumber,
      columnNumber,
      getLinesAround(lineNumber, contextLines, fileSource),
      functionName,
      source,
      line,
      column,
      getLinesAround(line, contextLines, originalSource),
    )
  })
}

export function unmapUtopiaSafeFunction(frame: StackFrame): StackFrame {
  const LinesToDropFromSafeFunction = 2

  if (frame._originalScriptCode == null || frame._originalLineNumber == null) {
    return frame
  }
  const code = frame._originalScriptCode

  return new StackFrame(
    frame.functionName,
    frame.fileName,
    frame.lineNumber,
    frame.columnNumber,
    frame._scriptCode,
    frame._originalFunctionName,
    frame._originalFileName,
    frame._originalLineNumber - LinesToDropFromSafeFunction,
    frame._originalColumnNumber,
    code,
  )
}

function getOriginalSourcePosition(
  map: SourceMap,
  lineNumber: number,
  columnNumber: number,
): { sourceFilename: string; line: number; column: number } | null {
  const originalPosition = map.getOriginalPosition(lineNumber, columnNumber)
  if (
    originalPosition.source != null &&
    originalPosition.line != null &&
    originalPosition.column != null
  ) {
    return {
      sourceFilename: originalPosition.source,
      line: originalPosition.line,
      column: originalPosition.column,
    }
  }

  // if our original code is `a.a` then our transpiled code is `return a.a`
  // it can happen that the error's line number and column are pointing to the 'r' in return
  // which is not part of the original code, so the map.getSource might have failed because of that
  // just in case we hit this edge case, let's retry getOriginalPosition with a column number offset
  // by the length of the prepended 'return '
  const correctedOriginalPosition = map.getOriginalPosition(
    lineNumber,
    columnNumber + RETURN_TO_PREPEND.length,
  )
  if (
    correctedOriginalPosition.source != null &&
    correctedOriginalPosition.line != null &&
    correctedOriginalPosition.column != null
  ) {
    return {
      sourceFilename: correctedOriginalPosition.source,
      line: correctedOriginalPosition.line,
      column: correctedOriginalPosition.column,
    }
  }
  return null
}

export function unmapBabelTranspiledCode(frame: StackFrame, map: SourceMap): StackFrame {
  if (frame._originalColumnNumber == null || frame._originalLineNumber == null) {
    return frame
  }

  const originalSourcePosition = getOriginalSourcePosition(
    map,
    frame._originalLineNumber,
    frame._originalColumnNumber,
  )

  if (originalSourcePosition != null) {
    const originalSource = map.getSource(originalSourcePosition.sourceFilename) || []
    return new StackFrame(
      frame.functionName,
      frame.fileName,
      frame.lineNumber,
      frame.columnNumber,
      frame._scriptCode,
      frame.functionName,
      originalSourcePosition.sourceFilename,
      originalSourcePosition.line,
      originalSourcePosition.column,
      getLinesAround(originalSourcePosition.line, 3, originalSource),
    )
  } else {
    return frame
  }
}

export { map }
export default map
