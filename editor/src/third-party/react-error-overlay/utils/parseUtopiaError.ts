import StackFrame, { ScriptLine } from './stack-frame'
import parseError from './parser'
import { SOURCE_MAP_PREFIX } from '../../../core/shared/code-exec-utils'

/**
 * Turns an <code>Error</code>, or similar object, into a set of <code>StackFrame</code>s.
 */
export function parseUtopiaError(
  parsedStackFrames: Array<StackFrame>,
  scriptFile: string[],
): StackFrame[] {
  // TODO make it work for multi-file stack traces
  const scriptLines = scriptFile.map(
    (sourceLine, index) => new ScriptLine(index + 1, sourceLine, false),
  )
  return parsedStackFrames.map((frame) => {
    const fixedFilename = frame.fileName?.split(SOURCE_MAP_PREFIX)[0]
    return new StackFrame(
      frame.functionName,
      fixedFilename,
      frame.lineNumber,
      frame.columnNumber,
      scriptLines,
      frame.functionName,
      fixedFilename,
      frame.lineNumber,
      frame.columnNumber,
      scriptLines,
    )
  })
}
