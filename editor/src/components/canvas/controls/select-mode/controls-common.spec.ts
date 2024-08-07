import { cssNumberWithRenderedValue, fallbackEmptyValue } from './controls-common'
import { cssNumber, emptyCssNumber } from '../../../../components/inspector/common/css-utils'
describe('empty css value', () => {
  const renderedValuePx = 16
  const tests = [
    {
      title: 'should fallback to rendered px in case of an empty value',
      cssNumber: emptyCssNumber(),
      expected: { value: 16, unit: 'px' },
    },
    {
      title: 'shouldnt fallback to rendered px in case of a non-empty value',
      cssNumber: cssNumber(1, 'em'),
      expected: { value: 1, unit: 'em' },
    },
    {
      title: 'shouldnt fallback to rendered px in case of a zero value',
      cssNumber: cssNumber(0),
      expected: { value: 0, unit: null },
    },
  ]

  tests.forEach((test) => {
    it(`${test.title}`, () => {
      expect(
        fallbackEmptyValue(cssNumberWithRenderedValue(test.cssNumber, renderedValuePx)),
      ).toEqual(test.expected)
    })
  })
})
