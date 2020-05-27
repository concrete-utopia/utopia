import StackFrame, { ScriptLine } from './stack-frame'
import parseError from './parser'

/**
 * Turns an <code>Error</code>, or similar object, into a set of <code>StackFrame</code>s.
 */
export function parseUtopiaError(
  error: Error | string | string[] | { stack: string },
  scriptFile: string[],
): StackFrame[] {
  const parsedStackFrames = parseError(error)

  // TODO make it work for multi-file stack traces
  const scriptLines = scriptFile.map(
    (sourceLine, index) => new ScriptLine(index + 1, sourceLine, false),
  )
  return parsedStackFrames.map((frame) => {
    return new StackFrame(
      frame.functionName,
      frame.fileName,
      frame.lineNumber,
      frame.columnNumber,
      scriptLines,
      frame.functionName,
      frame.fileName,
      frame.lineNumber,
      frame.columnNumber,
      scriptLines,
    )
  })
}
