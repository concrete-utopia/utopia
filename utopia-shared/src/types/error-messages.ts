export type ErrorMessageSeverity = 'fatal' | 'error' | 'warning'

export type ErrorMessageSource =
  | 'eslint'
  | 'build'
  | 'utopia-parser'
  | 'runtime'
  | 'component-descriptor'

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
