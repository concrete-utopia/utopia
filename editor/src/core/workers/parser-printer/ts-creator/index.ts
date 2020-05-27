// I have pulled out the bare minimum that we need from https://github.com/HearTao/ts-creator, because
// that project was also bundling a whole load of stuff we already have and stuff we don't need,
// causing our bundles to double in size. This is literally only for turning a JS block into an expression.

import { transformSourceFile } from './transformer'
import { createPrinter, createSourceFile, ScriptTarget, ScriptKind } from 'typescript'
import { Options } from 'prettier'
import * as prettier from 'prettier/standalone'
import * as parserTypescript from 'prettier/parser-typescript'

const prettierOptions: Options = {
  parser: 'typescript',
  plugins: [parserTypescript],
  semi: false,
  singleQuote: true,
  jsxSingleQuote: false,
  bracketSpacing: true,
  tabWidth: 2,
  useTabs: false,
  trailingComma: 'none',
  proseWrap: 'preserve',
}

export default function create(code: string): string {
  const printer = createPrinter()

  const file = createSourceFile(
    'temporary.tsx',
    code,
    ScriptTarget.Latest,
    undefined,
    ScriptKind.TSX,
  )

  const factoryFile = transformSourceFile(file)
  const factoryCode = printer.printFile(factoryFile)

  let result = prettier.format(factoryCode, prettierOptions)

  if (result.startsWith(';')) {
    result = result.substring(1)
  }
  return result
}
