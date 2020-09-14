// Above everything else as it needs to be set before the ESLint
// import is fired.
import * as BrowserFS from 'browserfs'
import * as stripAnsi from 'strip-ansi'

// eslint react plugin uses this
BrowserFS.configure({ fs: 'InMemory', options: {} }, (e) => {
  if (e) {
    throw e
  }
})
;(global as any).BrowserFS = BrowserFS

import * as ESLint from 'eslint'
import * as Linter from 'eslint4b'
import { ESLINT_CONFIG, EslintPluginRules } from './eslint-config'
import { ErrorMessage } from '../../shared/error-messages'
import * as BabelEslint from 'babel-eslint'

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

export function lintCode(
  filename: string,
  code: string,
  config: ESLint.Linter.Config = ESLINT_CONFIG,
): ErrorMessage[] {
  const passTime = Date.now()
  const lintResult = linter.verify(code, config, { filename: filename })
  return lintResult.map(
    (r: any): ErrorMessage => {
      let severity: ErrorMessage['severity']
      if (r.fatal) {
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
      const codeSnippet = strippedAndSplitMessage[1]

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
    },
  )
}
