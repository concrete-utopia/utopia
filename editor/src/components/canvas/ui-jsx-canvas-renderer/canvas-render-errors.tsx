import type { FancyError } from '../../../core/shared/code-exec-utils'
import type { JSXElement } from '../../../core/shared/element-template'
import type { HighlightBoundsForUids } from '../../../core/shared/project-file-types'
import { getUtopiaIDFromJSXElement } from '../../../core/shared/uid-utils'
import StackFrame, { ScriptLine } from '../../../third-party/react-error-overlay/utils/stack-frame'

export function canvasMissingJSXElementError(
  factoryFunctionName: string | null,
  sourceCode: string,
  jsxElement: JSXElement,
  filePath: string,
  highlightBounds: HighlightBoundsForUids | null,
): FancyError {
  const error = new Error(
    `(${jsxElement.name.baseVariable}) Element type is invalid: expected a string (for built-in components) or a class/function (for composite components) but got: undefined.`,
  )

  const uid = getUtopiaIDFromJSXElement(jsxElement)
  const highlightBoundsForElement = highlightBounds != null ? highlightBounds[uid] : null
  const lineNumber = (highlightBoundsForElement?.startLine ?? 0) + 1
  ;(error as FancyError).stackFrames = createStackFramesFromPosition(
    factoryFunctionName,
    sourceCode,
    filePath,
    lineNumber,
    highlightBoundsForElement?.startCol ?? 1,
  )
  return error
}

export function createStackFramesFromPosition(
  factoryFunctionName: string | null,
  sourceCode: string,
  filename: string,
  lineNumber: number,
  columnNumber: number,
): StackFrame[] {
  // TODO make it work for multi-file stack traces
  const fixedSourceCode = sourceCode.split('\n')
  const scriptLines = fixedSourceCode.map(
    (sourceLine, index) => new ScriptLine(index + 1, sourceLine, false),
  )
  return [
    new StackFrame(
      factoryFunctionName,
      filename,
      lineNumber,
      columnNumber,
      scriptLines,
      factoryFunctionName,
      filename,
      lineNumber,
      columnNumber,
      scriptLines,
    ),
  ]
}
