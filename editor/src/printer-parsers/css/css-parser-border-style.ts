import type { CSSLineStyle } from '../../components/inspector/common/css-utils'
import {
  cssLineStyle,
  cssLineStyleKeywordValues,
} from '../../components/inspector/common/css-utils'
import type { Either } from '../../core/shared/either'
import { isRight, left, mapEither } from '../../core/shared/either'
import type { Parser } from '../../utils/value-parser-utils'
import { descriptionParseError } from '../../utils/value-parser-utils'
import {
  isLexerMatch,
  isNamedSyntaxType,
  parseCSSValidKeyword,
  parseWholeValue,
} from './css-parser-utils'

export const parseLineStyle: Parser<CSSLineStyle> = (value: unknown) => {
  if (isLexerMatch(value) && isNamedSyntaxType(value.syntax, ['line-style'])) {
    const parsed = parseWholeValue(parseCSSValidKeyword(cssLineStyleKeywordValues))(value.match)
    return mapEither(cssLineStyle, parsed)
  } else {
    return left(descriptionParseError('Value is not a <line-style>'))
  }
}

export function parseBorderStyle(value: unknown): Either<string, CSSLineStyle> {
  const parsed = parseLineStyle(value)
  if (isRight(parsed)) {
    return parsed
  } else {
    return left(parsed.value.type)
  }
}
