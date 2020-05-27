export type ErrorMessageSeverity = 'fatal' | 'error' | 'warning'

export type ErrorMessageSource = 'eslint' | 'build' | 'utopia-parser' | 'runtime'

export interface ErrorMessage {
  fileName: string
  startLine: number | null
  startColumn: number | null
  endLine: number | null
  endColumn: number | null
  codeSnippet: string
  severity: ErrorMessageSeverity
  type: string
  message: string
  errorCode: string
  source: ErrorMessageSource
  passTime: number | null
}

export function messageisFatal(e: ErrorMessage): boolean {
  return e.severity === 'fatal'
}

export function messageIsFatalOrError(e: ErrorMessage): boolean {
  return e.severity === 'fatal' || e.severity === 'error'
}

export function messageIsWarning(e: ErrorMessage): boolean {
  return e.severity === 'warning'
}
