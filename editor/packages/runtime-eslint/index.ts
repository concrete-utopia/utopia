import type { Linter as ESLintLinter } from 'eslint'
import { rules as reactPluginRules } from 'eslint-plugin-react'
import { rules as reactHooksPluginRules } from 'eslint-plugin-react-hooks'
import { rules as jsxA11yPluginRules } from 'eslint-plugin-jsx-a11y'
import eslintImportRuleFirst from 'eslint-plugin-import/lib/rules/first'
import eslintImportRuleNoAmd from 'eslint-plugin-import/lib/rules/no-amd'
import eslintImportRuleNoWebpackSyntax from 'eslint-plugin-import/lib/rules/no-webpack-loader-syntax'
import Linter from 'eslint4b'
import BabelEslint from 'babel-eslint'

export type { ESLintLinter }
export {
  reactPluginRules,
  reactHooksPluginRules,
  jsxA11yPluginRules,
  eslintImportRuleFirst,
  eslintImportRuleNoAmd,
  eslintImportRuleNoWebpackSyntax,
  Linter,
  BabelEslint,
}
