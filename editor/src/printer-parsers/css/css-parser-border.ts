import {
  CSSBorder,
  CSSColor,
  CSSLineStyle,
  CSSLineWidth,
  isCSSColor,
  isCSSLineWidth,
} from '../../components/inspector/common/css-utils'
import { isRight, left, right, Either } from '../../core/shared/either'
import { parseLineWidth } from './css-parser-border-size'
import { parseLineStyle } from './css-parser-border-style'
import { getLexerMatches, parseDoubleBar, parseLexedColor } from './css-parser-utils'

export function parseBorder(value: unknown): Either<string, CSSBorder> {
  const lexer = getLexerMatches('border', value, ['line-style', 'line-width', 'color'])
  if (isRight(lexer)) {
    const parsed = parseDoubleBar<CSSColor | CSSLineWidth | CSSLineStyle>(3, [
      parseLexedColor,
      parseLineWidth,
      parseLineStyle,
    ])(lexer.value)
    if (isRight(parsed)) {
      const border = parsed.value.value.reduce((working, target) => {
        if (isCSSColor(target)) {
          return { ...working, color: target }
        } else if (isCSSLineWidth(target)) {
          return { ...working, width: target }
        } else {
          return { ...working, style: target }
        }
      }, {} as CSSBorder)
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
