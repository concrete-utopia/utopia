// Above everything else as it needs to be set before the ESLint
// import is fired.
import * as BrowserFS from 'browserfs'
import stripAnsi from 'strip-ansi'

// eslint react plugin uses this
BrowserFS.configure({ fs: 'InMemory', options: {} }, (e) => {
  if (e != null) {
    throw e
  }
})
;(global as any).BrowserFS = BrowserFS

import type { Linter as ESLintLinter } from 'eslint'
import Linter from 'eslint4b'
import { ESLINT_CONFIG, EslintPluginRules } from './eslint-config'
import type { ErrorMessage } from '../../shared/error-messages'
import BabelEslint from 'babel-eslint'
import { getFileExtension } from '../../shared/file-utils'

class CustomUtopiaLinter extends Linter {
  constructor() {
    super()
    // we replace the default `espree` parser by `babel-eslint` which supports experimental js feature that are in common use
    this.defineParser('babel-eslint', BabelEslint)
    this.addRules(EslintPluginRules)
  }

  addRules(pluginRules: { [key: string]: any }) {
    Object.keys(pluginRules).forEach((name) => {
      this.defineRule(name, pluginRules[name])
    })
  }
}

const linter = new CustomUtopiaLinter()

const FileExtensionsToLint = ['.js', '.jsx', '.ts', '.tsx']

export function lintCode(
  filename: string,
  code: string,
  config: ESLintLinter.Config = ESLINT_CONFIG,
): ErrorMessage[] {
  const passTime = Date.now()
  try {
    const fileExtension = getFileExtension(filename)
    if (FileExtensionsToLint.includes(fileExtension.toLowerCase())) {
      const lintResult = linter.verify(code, config, { filename: filename })
      const codeLines = code.split('\n')

      return lintResult.map((r: any): ErrorMessage => {
        let severity: ErrorMessage['severity']
        if (r.fatal as boolean) {
          severity = 'fatal'
        } else if (r.severity === 2) {
          severity = 'error'
        } else {
          severity = 'warning'
        }

        const ansiStrippedResultMessage = stripAnsi(r.message)
        const strippedAndSplitMessage = ansiStrippedResultMessage.split('\n\n')

        const message = strippedAndSplitMessage[0]
        const messageWithRule = r.ruleId == null ? message : `${message} (${r.ruleId})`
        const codeSnippetFromESLint = strippedAndSplitMessage[1]
        const codeSnippet =
          codeSnippetFromESLint ??
          codeLines.slice(Math.max(r.line - 3, 0), r.endLine + 3).join('\n')

        return {
          message: messageWithRule,
          fileName: filename,
          startLine: r.line,
          startColumn: r.column,
          endLine: r.endLine,
          endColumn: r.endColumn,
          codeSnippet: codeSnippet,
          severity: severity,
          type: severity,
          errorCode: r.ruleId,
          source: 'eslint',
          passTime: passTime,
        }
      })
    } else {
      return []
    }
  } catch (e) {
    return [
      {
        message: `ESLint runtime error:\n${e}`,
        fileName: filename,
        startLine: null,
        startColumn: null,
        endLine: null,
        endColumn: null,
        codeSnippet: '',
        severity: 'fatal',
        type: 'fatal',
        errorCode: '',
        source: 'eslint',
        passTime: passTime,
      },
    ]
  }
}
