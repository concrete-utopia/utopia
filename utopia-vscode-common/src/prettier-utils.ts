import * as parserTypescript from 'prettier/parser-typescript'
import * as prettier from 'prettier/standalone'
import type { CursorOptions, Options } from 'prettier'

export const PrettierConfig: Options = {
  parser: 'typescript',
  plugins: [parserTypescript],
  printWidth: 60,
  trailingComma: 'all',
  tabWidth: 2,
  semi: false,
  singleQuote: true,
  quoteProps: 'as-needed',
  bracketSpacing: true,
  jsxSingleQuote: true,
  jsxBracketSameLine: false,
  arrowParens: 'always',
}

export function applyPrettier(
  code: string,
  calculateCursorOffset: boolean,
  cursorOffset?: number,
): {
  formatted: string
  cursorOffset: number
} {
  const offset = cursorOffset || 0
  try {
    if (calculateCursorOffset) {
      return prettier.formatWithCursor(code, {
        ...PrettierConfig,
        cursorOffset: offset,
      } as CursorOptions)
    } else {
      const formattedCode = prettier.format(code, PrettierConfig)
      return {
        formatted: formattedCode,
        cursorOffset: 0,
      }
    }
  } catch (e) {
    // Prettier will attempt to parse the code and will throw on syntax errors
    return { formatted: code, cursorOffset: offset }
  }
}
