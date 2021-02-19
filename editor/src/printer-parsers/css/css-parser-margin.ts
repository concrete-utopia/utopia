import {
  CSSMargin,
  CSSNumber,
  printCSSNumber,
  printCSSNumberWithDefaultUnit,
} from '../../components/inspector/common/css-utils'
import { Either, isRight, left, right } from '../../core/shared/either'
import { JSXAttributeValue, jsxAttributeValue } from '../../core/shared/element-template'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import {
  canUseOneValueSyntax,
  canUseThreeValueSyntax,
  canUseTwoValueSyntax,
} from './css-parser-padding'
import { getLexerPropertyMatches, parseLengthPercentage, parseCSSArray } from './css-parser-utils'

export const parseMargin = (value: unknown): Either<string, CSSMargin> => {
  const lexer = getLexerPropertyMatches('margin', value, 'px')
  if (isRight(lexer)) {
    const parseResult = parseCSSArray([parseLengthPercentage])(lexer.value)
    if (isRight(parseResult)) {
      const resultArray = parseResult.value
      let marginTop: CSSNumber,
        marginRight: CSSNumber,
        marginBottom: CSSNumber,
        marginLeft: CSSNumber
      if (resultArray.length === 0 || resultArray.length > 4) {
        return left(`Value ${JSON.stringify(value)} is not a valid margin`)
      } else if (resultArray.length === 1) {
        marginTop = resultArray[0]
        marginRight = resultArray[0]
        marginBottom = resultArray[0]
        marginLeft = resultArray[0]
      } else if (resultArray.length === 2) {
        marginTop = resultArray[0]
        marginRight = resultArray[1]
        marginBottom = resultArray[0]
        marginLeft = resultArray[1]
      } else if (resultArray.length === 3) {
        marginTop = resultArray[0]
        marginRight = resultArray[1]
        marginBottom = resultArray[2]
        marginLeft = resultArray[1]
      } else {
        marginTop = resultArray[0]
        marginRight = resultArray[1]
        marginBottom = resultArray[2]
        marginLeft = resultArray[3]
      }
      return right({
        marginTop: marginTop,
        marginRight: marginRight,
        marginBottom: marginBottom,
        marginLeft: marginLeft,
      })
    } else {
      return left(`Value ${JSON.stringify(value)} is not a valid margin`)
    }
  }
  return left('Value was not lexer match array')
}

export const printMarginAsAttributeValue = (
  value: CSSMargin,
): JSXAttributeValue<number | string> => {
  const marginTop = printCSSNumberWithDefaultUnit(value.marginTop, 'px')
  const marginRight = printCSSNumberWithDefaultUnit(value.marginRight, 'px')
  const marginBottom = printCSSNumberWithDefaultUnit(value.marginBottom, 'px')
  const marginLeft = printCSSNumberWithDefaultUnit(value.marginLeft, 'px')

  if (canUseOneValueSyntax(marginTop, marginRight, marginBottom, marginLeft)) {
    const marginValue = printCSSNumber(value.marginTop)
    return jsxAttributeValue(marginValue, emptyComments)
  } else if (canUseTwoValueSyntax(marginTop, marginRight, marginBottom, marginLeft)) {
    const marginValue = `${marginTop} ${marginLeft}`
    return jsxAttributeValue(marginValue, emptyComments)
  } else if (canUseThreeValueSyntax(marginTop, marginRight, marginBottom, marginLeft)) {
    const marginValue = `${marginTop} ${marginLeft} ${marginBottom}`
    return jsxAttributeValue(marginValue, emptyComments)
  } else {
    const marginValue = `${marginTop} ${marginRight} ${marginBottom} ${marginLeft}`
    return jsxAttributeValue(marginValue, emptyComments)
  }
}
