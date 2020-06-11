import {
  cssLineStyle,
  cssLineWidth,
  cssKeyword,
  cssNumber,
  cssColor,
  CSSBorder,
} from '../../components/inspector/common/css-utils'
import { parseBorder } from './css-parser-border'
import { isRight } from '../../core/shared/either'

describe('parseBorder', () => {
  it('parses a CSSBorder', () => {
    const validValue: Array<string> = [
      'solid 1px',
      'solid 1px #000',
      '1px #000 solid',
      '#000 solid 1px',
    ]

    const expectedValidValue: Array<CSSBorder> = [
      {
        style: cssLineStyle(cssKeyword('solid')),
        width: cssLineWidth(cssNumber(1, 'px')),
      },
      {
        style: cssLineStyle(cssKeyword('solid')),
        width: cssLineWidth(cssNumber(1, 'px')),
        color: cssColor('#000'),
      },
      {
        style: cssLineStyle(cssKeyword('solid')),
        width: cssLineWidth(cssNumber(1, 'px')),
        color: cssColor('#000'),
      },
      {
        style: cssLineStyle(cssKeyword('solid')),
        width: cssLineWidth(cssNumber(1, 'px')),
        color: cssColor('#000'),
      },
    ]

    validValue.forEach((valid, i) => {
      const parsed = parseBorder(valid)
      if (isRight(parsed)) {
        expect(parsed.value).toEqual(expectedValidValue[i])
      } else {
        fail()
      }
    })
  })
})
