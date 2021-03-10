import {
  CSSNumber,
  cssNumberToString,
  CSSPadding,
  printCSSNumber,
  printCSSNumberWithDefaultUnit,
} from '../../components/inspector/common/css-utils'
import { Either, isRight, left, right } from '../../core/shared/either'
import { JSXAttributeValue, jsxAttributeValue } from '../../core/shared/element-template'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { getLexerPropertyMatches, parseLengthPercentage, parseCSSArray } from './css-parser-utils'

export const parsePadding = (value: unknown): Either<string, CSSPadding> => {
  const lexer = getLexerPropertyMatches('padding', value, 'px')
  if (isRight(lexer)) {
    const parseResult = parseCSSArray([parseLengthPercentage])(lexer.value)
    if (isRight(parseResult)) {
      const resultArray = parseResult.value
      let paddingTop: CSSNumber,
        paddingRight: CSSNumber,
        paddingBottom: CSSNumber,
        paddingLeft: CSSNumber
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
  const paddingTop = printCSSNumberWithDefaultUnit(value.paddingTop, 'px')
  const paddingRight = printCSSNumberWithDefaultUnit(value.paddingRight, 'px')
  const paddingBottom = printCSSNumberWithDefaultUnit(value.paddingBottom, 'px')
  const paddingLeft = printCSSNumberWithDefaultUnit(value.paddingLeft, 'px')

  if (canUseOneValueSyntax(paddingTop, paddingRight, paddingBottom, paddingLeft)) {
    const paddingValue = printCSSNumber(value.paddingTop, 'px')
    return jsxAttributeValue(paddingValue, emptyComments)
  } else if (canUseTwoValueSyntax(paddingTop, paddingRight, paddingBottom, paddingLeft)) {
    const paddingValue = `${paddingTop} ${paddingLeft}`
    return jsxAttributeValue(paddingValue, emptyComments)
  } else if (canUseThreeValueSyntax(paddingTop, paddingRight, paddingBottom, paddingLeft)) {
    const paddingValue = `${paddingTop} ${paddingLeft} ${paddingBottom}`
    return jsxAttributeValue(paddingValue, emptyComments)
  } else {
    const paddingValue = `${paddingTop} ${paddingRight} ${paddingBottom} ${paddingLeft}`
    return jsxAttributeValue(paddingValue, emptyComments)
  }
}

export function canUseOneValueSyntax(
  topValue: string,
  rightValue: string,
  bottomValue: string,
  leftValue: string,
): boolean {
  return topValue === rightValue && topValue === bottomValue && topValue === leftValue
}

export function canUseTwoValueSyntax(
  topValue: string,
  rightValue: string,
  bottomValue: string,
  leftValue: string,
): boolean {
  return topValue !== leftValue && topValue === bottomValue && leftValue === rightValue
}

export function canUseThreeValueSyntax(
  topValue: string,
  rightValue: string,
  bottomValue: string,
  leftValue: string,
): boolean {
  // not using the real 3-value shorthand because it is very uncommon
  // return topValue !== bottomValue && leftValue === rightValue
  return false
}
