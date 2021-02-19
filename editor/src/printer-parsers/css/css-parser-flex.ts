import {
  CSSFlex,
  CSSNumber,
  printCSSNumber,
  printCSSNumberWithDefaultUnit,
} from '../../components/inspector/common/css-utils'
import { Either, isRight, left, mapEither, right } from '../../core/shared/either'
import { JSXAttributeValue, jsxAttributeValue } from '../../core/shared/element-template'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { descriptionParseError, Parser } from '../../utils/value-parser-utils'
import {
  canUseOneValueSyntax,
  canUseThreeValueSyntax,
  canUseTwoValueSyntax,
} from './css-parser-padding'
import {
  getLexerPropertyMatches,
  parseLengthPercentage,
  parseCSSArray,
  parseNumber,
  isLexerMatch,
  isNamedSyntaxType,
  parseDoubleBar,
} from './css-parser-utils'

export const parseFlex = (value: unknown): Either<string, CSSFlex> => {
  const lexer = getLexerPropertyMatches('flex', value, '')
  if (isRight(lexer)) {
    const parseResult = parseDoubleBar<CSSFlexGrow | CSSFlexShrink | CSSFlexBasis>(3, [
      parseFlexGrow,
      parseFlexShrink,
      parseFlexBasis,
    ])(lexer.value)
    if (isRight(parseResult)) {
      const flexResult = parseResult.value.value.reduce(
        (working, target) => {
          if (isCSSFlexBasis(target)) {
            return { ...working, flexBasis: target.value } as CSSFlex
          } else if (isCSSFlexGrow(target)) {
            return { ...working, flexGrow: printCSSNumber(target.value) } as CSSFlex
          } else if (isCSSFlexShrink(target)) {
            return { ...working, flexShrink: printCSSNumber(target.value) } as CSSFlex
          } else {
            return working
          }
        },
        {
          flexGrow: undefined,
          flexShrink: undefined,
          flexBasis: undefined,
        } as CSSFlex,
      )
      return right(flexResult)
    } else {
      return left(`Value ${JSON.stringify(value)} is not a valid flex, ${parseResult.value}`)
    }
  }
  return left('Value was not lexer match array')
}

export const parseFlexGrow: Parser<CSSFlexGrow> = (value: unknown) => {
  if (isLexerMatch(value) && isNamedSyntaxType(value.syntax, ['flex-grow'])) {
    const parsed = parseNumber(value.match[0])
    return mapEither(cssFlexGrow, parsed)
  } else {
    return left(descriptionParseError('Value is not a flex-grow>'))
  }
}
export const parseFlexShrink: Parser<CSSFlexShrink> = (value: unknown) => {
  if (isLexerMatch(value) && isNamedSyntaxType(value.syntax, ['flex-shrink'])) {
    const parsed = parseNumber(value.match[0])
    return mapEither(cssFlexShrink, parsed)
  } else {
    return left(descriptionParseError('Value is not a flex-shrink>'))
  }
}
export const parseFlexBasis: Parser<CSSFlexBasis> = (value: unknown) => {
  if (isLexerMatch(value) && isNamedSyntaxType(value.syntax, ['flex-basis'])) {
    const parsed = parseLengthPercentage(value.match[0])
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

export const printFlexAsAttributeValue = (value: CSSFlex): JSXAttributeValue<number | string> => {
  const flexGrow = value.flexGrow
  const flexShrink = value.flexShrink
  const flexBasis =
    value.flexBasis == null ? value.flexBasis : printCSSNumberWithDefaultUnit(value.flexBasis, 'px')

  if (flexBasis == null) {
    if (flexShrink == null) {
      return jsxAttributeValue(`${flexGrow}`, emptyComments)
    } else {
      return jsxAttributeValue(`${flexGrow} ${flexShrink}`, emptyComments)
    }
  } else if (flexGrow == null && flexShrink == null) {
    return jsxAttributeValue(`${flexBasis}`, emptyComments)
  } else if (flexGrow === flexShrink) {
    return jsxAttributeValue(`${flexGrow} ${flexBasis}`, emptyComments)
  } else {
    return jsxAttributeValue(`${flexGrow} ${flexShrink} ${flexBasis}`, emptyComments)
  }
}
