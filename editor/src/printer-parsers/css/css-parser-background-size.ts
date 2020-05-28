import {
  CSSBackgroundSize,
  CSSBGSize,
  cssBGSize,
  CSSBGSizeCurlyBraceValueValue,
  cssDefault,
} from '../../components/inspector/common/css-utils'
import { Either, isRight, left, mapEither, traverseEither } from '../../core/shared/either'
import { descriptionParseError, parseAlternative, Parser } from '../../utils/value-parser-utils'
import {
  isLexerMatch,
  parseCSSValidKeyword,
  parseCurlyBraces,
  parseLengthPercentage,
  parseWholeValue,
  getLexerMatches,
} from './css-parser-utils'

const parseBGSizeCurlyBrace: Parser<CSSBGSize> = (value: unknown) => {
  const curlyBraceParsers: Array<Parser<CSSBGSizeCurlyBraceValueValue>> = [
    parseLengthPercentage,
    parseCSSValidKeyword(['auto']),
  ]
  const parsed = parseCurlyBraces(1, 2, curlyBraceParsers)(value)
  return mapEither((v) => cssBGSize(cssDefault(v)), parsed)
}

const parseBGSizeKeyword: Parser<CSSBGSize> = (value: unknown) => {
  const parsed = parseWholeValue(parseCSSValidKeyword(['contain', 'cover']))(value)
  return mapEither((v) => cssBGSize(cssDefault(v)), parsed)
}

export const parseBGSize: Parser<CSSBGSize> = (value: unknown) => {
  if (isLexerMatch(value)) {
    return parseAlternative(
      [parseBGSizeCurlyBrace, parseBGSizeKeyword],
      `Value ${JSON.stringify(value)} is not valid bg-size`,
    )(value.match)
  } else {
    return left(descriptionParseError(`${JSON.stringify(value)} is not lexer match`))
  }
}

export function parseBackgroundSize(value: unknown): Either<string, CSSBackgroundSize> {
  const lexer = getLexerMatches('background-size', value, 'bg-size')
  if (isRight(lexer)) {
    const parsed = traverseEither(parseBGSize, lexer.value)
    if (isRight(parsed)) {
      return parsed
    } else {
      return left('Lexer values could not be parsed by parseBGSize')
    }
  }
  return left('Value was not lexer match array')
}
