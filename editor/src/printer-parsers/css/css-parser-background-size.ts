import {
  CSSBackgroundSize,
  CSSBGSize,
  cssBGSize,
  CSSBGSizeCurlyBraceValueValue,
  cssDefault,
} from '../../components/inspector/common/css-utils'
import {
  bimapEither,
  Either,
  isRight,
  left,
  mapEither,
  sequenceEither,
} from '../../core/shared/either'
import { descriptionParseError, parseAlternative, Parser } from '../../utils/value-parser-utils'
import {
  getLexerTypeMatches,
  isLexerMatch,
  parseCSSValidKeyword,
  parseCurlyBraces,
  parseLengthPercentage,
  parseWholeValue,
  PreparsedLayer,
  traverseForPreparsedLayers,
} from './css-parser-utils'

const parseBGSizeCurlyBrace: Parser<CSSBGSize> = (value: unknown) => {
  const curlyBraceParsers: Array<Parser<CSSBGSizeCurlyBraceValueValue>> = [
    parseLengthPercentage,
    parseCSSValidKeyword(['auto']),
  ]
  const parsed = parseCurlyBraces(1, 2, curlyBraceParsers)(value)
  /* n.b. this always returns enabled: true, and is overridden by
   *  parseBackgroundSize if need be.
   */
  return mapEither((v) => cssBGSize(cssDefault(v), true), parsed)
}

const parseBGSizeKeyword: Parser<CSSBGSize> = (value: unknown) => {
  const parsed = parseWholeValue(parseCSSValidKeyword(['contain', 'cover']))(value)
  /* n.b. this always returns enabled: true, and is overridden by
   *  parseBackgroundSize if need be.
   */
  return mapEither((v) => cssBGSize(cssDefault(v), true), parsed)
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
  if (typeof value === 'string') {
    const preparsedLayers: Array<PreparsedLayer> = traverseForPreparsedLayers(value)
    return sequenceEither(
      preparsedLayers.map((layer) => {
        const lexerMatch = getLexerTypeMatches('bg-size', layer.value)
        if (isRight(lexerMatch)) {
          const parsed = parseBGSize(lexerMatch.value)
          return bimapEither(
            (l) => l.type,
            (r) => {
              r.enabled = layer.enabled
              return r
            },
            parsed,
          )
        } else {
          return lexerMatch
        }
      }),
    )
  }
  return left('Value is not string')
}
