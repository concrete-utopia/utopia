import { CSSNumber, CSSPadding, printCSSNumber } from '../../components/inspector/common/css-utils'
import { Either, isRight, left, right } from '../../core/shared/either'
import { JSXAttributeValue, jsxAttributeValue } from '../../core/shared/element-template'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { getLexerPropertyMatches, parseLengthPercentage, parseCSSArray } from './css-parser-utils'

export const parsePadding = (value: unknown): Either<string, CSSPadding> => {
  const lexer = getLexerPropertyMatches('padding', value)
  if (isRight(lexer)) {
    const parseResult = parseCSSArray([parseLengthPercentage])(lexer.value)
    if (isRight(parseResult)) {
      const resultArray = parseResult.value
      let paddingTop, paddingRight, paddingBottom, paddingLeft
      if (resultArray.length === 0 || resultArray.length > 4) {
        return left(`Value ${JSON.stringify(value)} is not a valid padding`)
      } else if (resultArray.length === 1) {
        paddingTop = resultArray[0]
        paddingRight = resultArray[0]
        paddingBottom = resultArray[0]
        paddingLeft = resultArray[0]
      } else if (resultArray.length === 2) {
        paddingTop = resultArray[0]
        paddingRight = resultArray[1]
        paddingBottom = resultArray[0]
        paddingLeft = resultArray[1]
      } else if (resultArray.length === 3) {
        paddingTop = resultArray[0]
        paddingRight = resultArray[1]
        paddingBottom = resultArray[2]
        paddingLeft = resultArray[1]
      } else {
        paddingTop = resultArray[0]
        paddingRight = resultArray[1]
        paddingBottom = resultArray[2]
        paddingLeft = resultArray[3]
      }
      return right({
        paddingTop: paddingTop,
        paddingRight: paddingRight,
        paddingBottom: paddingBottom,
        paddingLeft: paddingLeft,
      })
    } else {
      return left(`Value ${JSON.stringify(value)} is not a valid padding`)
    }
  }
  return left('Value was not lexer match array')
}

export const printPaddingAsAttributeValue = (
  value: CSSPadding,
): JSXAttributeValue<number | string> => {
  // TODO all paddings!
  return jsxAttributeValue(printCSSNumber(value.paddingTop), emptyComments)
}
