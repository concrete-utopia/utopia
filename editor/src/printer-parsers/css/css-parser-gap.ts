import {
  CSSGap,
  CSSNumber,
  printCSSNumber,
  printCSSNumberWithDefaultUnit,
} from '../../components/inspector/common/css-utils'
import { Either, isRight, left, right } from '../../core/shared/either'
import { JSXAttributeValue, jsxAttributeValue } from '../../core/shared/element-template'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { getLexerPropertyMatches, parseLengthPercentage, parseCSSArray } from './css-parser-utils'

export const parseGap = (value: unknown): Either<string, CSSGap> => {
  const lexer = getLexerPropertyMatches('gap', value, 'px')
  if (isRight(lexer)) {
    const parseResult = parseCSSArray([parseLengthPercentage])(lexer.value)
    if (isRight(parseResult)) {
      const resultArray = parseResult.value
      let rowGap: CSSNumber, columnGap: CSSNumber
      if (resultArray.length === 0 || resultArray.length > 2) {
        return left(`Value ${JSON.stringify(value)} is not a valid margin`)
      } else if (resultArray.length === 1) {
        rowGap = resultArray[0]
        columnGap = resultArray[0]
      } else {
        rowGap = resultArray[0]
        columnGap = resultArray[1]
      }
      return right({
        rowGap: rowGap,
        columnGap: columnGap,
      })
    } else {
      return left(`Value ${JSON.stringify(value)} is not a valid margin`)
    }
  }
  return left('Value was not lexer match array')
}

export const printGapAsAttributeValue = (value: CSSGap): JSXAttributeValue<number | string> => {
  const rowGap = printCSSNumberWithDefaultUnit(value.rowGap, 'px')
  const columnGap = printCSSNumberWithDefaultUnit(value.columnGap, 'px')

  if (rowGap === columnGap) {
    const gapValue = printCSSNumber(value.rowGap)
    return jsxAttributeValue(gapValue, emptyComments)
  } else {
    const gapValue = `${rowGap} ${columnGap}`
    return jsxAttributeValue(gapValue, emptyComments)
  }
}
