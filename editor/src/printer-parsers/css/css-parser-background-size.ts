import * as csstree from 'css-tree'
import * as R from 'ramda'
import {
  CSSBackgroundSize,
  CSSBGSize,
  cssBGSize,
  CSSBGSizeCurlyBraceValueValue,
  cssDefault,
} from '../../components/inspector/common/css-utils'
import {
  Either,
  isRight,
  left,
  mapEither,
  traverseEither,
  Right,
  sequenceEither,
  bimapEither,
} from '../../core/shared/either'
import {
  descriptionParseError,
  parseAlternative,
  Parser,
  ParseError,
} from '../../utils/value-parser-utils'
import {
  getLexerPropertyMatches,
  isLexerMatch,
  parseCSSValidKeyword,
  parseCurlyBraces,
  parseLengthPercentage,
  parseWholeValue,
  getLexerTypeMatches,
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

interface PreparsedLayer {
  type: 'PREPARSED_LAYER'
  value: string
  enabled: boolean
}

function preparsedLayer(value: string, enabled: boolean): PreparsedLayer {
  return {
    type: 'PREPARSED_LAYER',
    value,
    enabled,
  }
}

const commentedValueAndSurrounding = /(.*)\/\*\s*(.+?)\s*\*\/(.*)/

function preparseLayers(value: string): Array<PreparsedLayer> {
  return R.flatten(
    value.split(',').map((v) => {
      const result = v.trim().match(commentedValueAndSurrounding)
      if (result != null) {
        const [, before, comment, after] = [...result]
        if (before !== '' && after === '') {
          return [preparsedLayer(before, true), preparsedLayer(comment, false)]
        } else if (after !== '' && before === '') {
          return [preparsedLayer(comment, false), preparsedLayer(after, true)]
        }
      }
      return [preparsedLayer(v, true)]
    }),
  )
}

export function parseBackgroundSize(value: unknown): Either<string, CSSBackgroundSize> {
  if (typeof value === 'string') {
    const preparsedLayers: Array<PreparsedLayer> = preparseLayers(value)
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
