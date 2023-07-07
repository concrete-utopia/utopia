import type {
  CSSBorder,
  CSSColor,
  CSSLineStyle,
  CSSLineWidth,
} from '../../components/inspector/common/css-utils'
import {
  emptyCSSBorder,
  isCSSColor,
  isCSSLineWidth,
} from '../../components/inspector/common/css-utils'
import type { Either } from '../../core/shared/either'
import { isRight, left, right } from '../../core/shared/either'
import { parseLineWidth } from './css-parser-border-size'
import { parseLineStyle } from './css-parser-border-style'
import { getLexerPropertyMatches, parseDoubleBar, parseLexedColor } from './css-parser-utils'

export function parseBorder(value: unknown): Either<string, CSSBorder> {
  const lexer = getLexerPropertyMatches('border', value, '', ['line-style', 'line-width', 'color'])
  if (isRight(lexer)) {
    const parsed = parseDoubleBar<CSSColor | CSSLineWidth | CSSLineStyle>(3, [
      parseLexedColor,
      parseLineWidth,
      parseLineStyle,
    ])(lexer.value)
    if (isRight(parsed)) {
      const border = parsed.value.value.reduce(
        (working, target) => {
          if (isCSSColor(target)) {
            return { ...working, color: target }
          } else if (isCSSLineWidth(target)) {
            return { ...working, width: target }
          } else {
            return { ...working, style: target }
          }
        },
        { ...emptyCSSBorder },
      )
      return right(border)
    } else {
      return left(
        `Value ${JSON.stringify(
          value,
        )} is not a valid <'border-width'>, <'border-size'>, or <'border-color'>`,
      )
    }
  }
  return left('Value was not lexer match array')
}
