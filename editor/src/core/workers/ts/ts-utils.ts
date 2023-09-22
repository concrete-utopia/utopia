import * as TS from 'typescript-for-the-editor'
import type { ErrorMessage } from '../../shared/error-messages'
import { fastForEach } from '../../shared/utils'

export function diagnosticToErrorMessage(diagnostic: TS.Diagnostic): ErrorMessage {
  let message = TS.flattenDiagnosticMessageText(diagnostic.messageText, '\n')
  const severity = getSeverity(diagnostic.category)
  if (diagnostic.file != null) {
    const start = diagnostic.start == null ? 0 : diagnostic.start
    const length = diagnostic.length == null ? 0 : diagnostic.length
    let { line: startLine, character: startColumn } =
      diagnostic.file.getLineAndCharacterOfPosition(start)
    let { line: endLine, character: endColumn } = diagnostic.file.getLineAndCharacterOfPosition(
      start + length,
    )
    const codeSnippet = createCodeSnippet(diagnostic.file, start, start + length)
    return {
      fileName: diagnostic.file.fileName,
      startLine: startLine + 1,
      startColumn: startColumn + 1,
      endLine: endLine + 1,
      endColumn: endColumn + 1,
      codeSnippet: codeSnippet,
      severity: severity,
      type: TS.DiagnosticCategory[diagnostic.category],
      message: message,
      errorCode: `TS${diagnostic.code}`,
      source: 'build',
      passTime: null,
    }
  } else {
    return {
      fileName: '',
      startLine: null,
      startColumn: null,
      endLine: null,
      endColumn: null,
      severity: severity,
      type: TS.DiagnosticCategory[diagnostic.category],
      message: message,
      codeSnippet: '',
      errorCode: `TS${diagnostic.code}`,
      source: 'build',
      passTime: null,
    }
  }
}

const LineNumberLength = 4
const LineNumberDecoration = ' | '

export function createCodeSnippetFromCode(
  code: string,
  startLine: number,
  startColumn: number,
  endLine: number,
  endColumn: number,
): string {
  const codeLines = code.split('\n')

  const nicePrintedCodeLines = codeLines
    .filter((_, index) => {
      return index >= startLine && index <= endLine
    })
    .map((codeLine, index) => {
      const lineNumber = index + 1 + startLine
      let startColumnForThisLine: number
      let endColumnForThisLine: number
      if (lineNumber === startLine) {
        startColumnForThisLine = startColumn
      } else {
        startColumnForThisLine = 0
      }
      if (lineNumber === endLine) {
        endColumnForThisLine = endColumn
      } else {
        endColumnForThisLine = codeLine.length
      }
      const linePrefix = lineNumber.toString().padStart(LineNumberLength, ' ')
      return (
        `${linePrefix}${LineNumberDecoration}${codeLine}` +
        '\n' +
        printWigglyLine(startColumnForThisLine, endColumnForThisLine) +
        '\n'
      )
    })

  // TODO enhance me to contain lines before/after
  return nicePrintedCodeLines.join('\n')
}

export function createCodeSnippet(sourceFile: TS.SourceFile, start: number, end: number): string {
  let { line: startLine, character: errorStartCol } =
    sourceFile.getLineAndCharacterOfPosition(start)
  let { line: endLine, character: errorEndCol } = sourceFile.getLineAndCharacterOfPosition(end)
  const codeString = sourceFile.text
  return createCodeSnippetFromCode(codeString, startLine, errorStartCol, endLine, errorEndCol)
}

function printWigglyLine(fromChar: number, toChar: number): string {
  return (
    ' '.repeat(Math.max(fromChar + LineNumberLength + LineNumberDecoration.length, 0)) +
    '~'.repeat(Math.max(toChar - fromChar, 0))
  )
}

function getSeverity(category: TS.DiagnosticCategory): ErrorMessage['severity'] {
  switch (category) {
    case TS.DiagnosticCategory.Error:
      return 'fatal'
    default:
      return 'warning'
  }
}
