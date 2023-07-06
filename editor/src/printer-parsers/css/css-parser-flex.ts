import type { CSSFlex, CSSNumber } from '../../components/inspector/common/css-utils'
import {
  cssNumber,
  printCSSNumber,
  printCSSNumberWithDefaultUnit,
} from '../../components/inspector/common/css-utils'
import type { Either } from '../../core/shared/either'
import { isRight, left, mapEither, right } from '../../core/shared/either'
import type { JSExpressionValue } from '../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import type { Parser } from '../../utils/value-parser-utils'
import { descriptionParseError } from '../../utils/value-parser-utils'
import {
  getLexerPropertyMatches,
  parseLengthPercentage,
  parseCSSArray,
  parseNumber,
  isLexerMatch,
  isNamedSyntaxProperty,
} from './css-parser-utils'

export const AssumedFlexDefaults: CSSFlex = {
  flexGrow: 1,
  flexShrink: 1,
  flexBasis: {
    value: 0,
    unit: null,
  },
}
export const parseFlex = (value: unknown): Either<string, CSSFlex> => {
  const lexer = getLexerPropertyMatches('flex', value, '')
  if (isRight(lexer)) {
    const parseResult = parseCSSArray<CSSFlexGrow | CSSFlexShrink | CSSFlexBasis>([
      parseFlexGrow,
      parseFlexShrink,
      parseFlexBasis,
    ])(lexer.value)
    if (isRight(parseResult)) {
      const flexResult = parseResult.value.reduce((working, target) => {
        if (isCSSFlexBasis(target)) {
          return { ...working, flexBasis: target.value } as CSSFlex
        } else if (isCSSFlexGrow(target)) {
          return { ...working, flexGrow: printCSSNumber(target.value, null) } as CSSFlex
        } else if (isCSSFlexShrink(target)) {
          return { ...working, flexShrink: printCSSNumber(target.value, null) } as CSSFlex
        } else {
          return working
        }
      }, AssumedFlexDefaults)
      return right(flexResult)
    } else {
      return left(
        `Value ${JSON.stringify(value)} is not a valid flex, ${JSON.stringify(parseResult)}`,
      )
    }
  }
  return left('Value was not lexer match array')
}

export const parseFlexGrow: Parser<CSSFlexGrow> = (value: unknown) => {
  if (isLexerMatch(value) && isNamedSyntaxProperty(value.syntax, ['flex-grow'])) {
    const parsed = parseNumber(value.match[0])
    return mapEither(cssFlexGrow, parsed)
  } else {
    return left(descriptionParseError('Value is not a flex-grow>'))
  }
}
export const parseFlexShrink: Parser<CSSFlexShrink> = (value: unknown) => {
  if (isLexerMatch(value) && isNamedSyntaxProperty(value.syntax, ['flex-shrink'])) {
    const parsed = parseNumber(value.match[0])
    return mapEither(cssFlexShrink, parsed)
  } else {
    return left(descriptionParseError('Value is not a flex-shrink>'))
  }
}
export const parseFlexBasis: Parser<CSSFlexBasis> = (value: unknown) => {
  if (
    isLexerMatch(value) &&
    isNamedSyntaxProperty(value.syntax, ['flex-basis']) &&
    isLexerMatch(value.match[0]) &&
    isNamedSyntaxProperty(value.match[0].syntax, ['width', 'height'])
  ) {
    const parsed = parseLengthPercentage(value.match[0].match[0])
    return mapEither(cssFlexBasis, parsed)
  } else {
    return left(descriptionParseError('Value is not a flex-basis>'))
  }
}

interface CSSFlexGrow {
  type: 'flex-grow'
  value: CSSNumber
}
interface CSSFlexShrink {
  type: 'flex-shrink'
  value: CSSNumber
}
interface CSSFlexBasis {
  type: 'flex-basis'
  value: CSSNumber
}

function cssFlexGrow(value: CSSNumber): CSSFlexGrow {
  return {
    type: 'flex-grow',
    value,
  }
}
function cssFlexShrink(value: CSSNumber): CSSFlexShrink {
  return {
    type: 'flex-shrink',
    value,
  }
}
function cssFlexBasis(value: CSSNumber): CSSFlexBasis {
  return {
    type: 'flex-basis',
    value,
  }
}

function isCSSFlexGrow(value: CSSFlexGrow | CSSFlexShrink | CSSFlexBasis): value is CSSFlexGrow {
  return value.type === 'flex-grow' && value.value.unit == null
}
function isCSSFlexShrink(
  value: CSSFlexGrow | CSSFlexShrink | CSSFlexBasis,
): value is CSSFlexShrink {
  return value.type === 'flex-shrink' && value.value.unit == null
}
function isCSSFlexBasis(value: CSSFlexGrow | CSSFlexShrink | CSSFlexBasis): value is CSSFlexBasis {
  return value.type === 'flex-basis'
}

export const printFlexAsAttributeValue = (value: CSSFlex): JSExpressionValue<number | string> => {
  const flexGrow = value.flexGrow
  const flexShrink = value.flexShrink
  const flexBasis = printCSSNumber(value.flexBasis, null)

  if (flexBasis === printCSSNumber(AssumedFlexDefaults.flexBasis, null)) {
    if (flexShrink === AssumedFlexDefaults.flexShrink) {
      return jsExpressionValue(`${flexGrow}`, emptyComments)
    } else {
      return jsExpressionValue(`${flexGrow} ${flexShrink}`, emptyComments)
    }
  } else {
    return jsExpressionValue(`${flexGrow} ${flexShrink} ${flexBasis}`, emptyComments)
  }
}
