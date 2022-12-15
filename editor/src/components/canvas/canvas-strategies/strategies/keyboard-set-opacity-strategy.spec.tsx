import { parseOpacityFromKeyboard } from './keyboard-set-opacity-strategy'
import * as fc from 'fast-check'

const digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'].map(fc.constant)

describe('set opacity from keyboard', () => {
  describe('parseOpacity', () => {
    it('edge cases', () => {
      expect(parseOpacityFromKeyboard('')).toEqual(null)
      expect(parseOpacityFromKeyboard('1a')).toEqual(null)
      expect(parseOpacityFromKeyboard('a1')).toEqual(null)
      expect(parseOpacityFromKeyboard('a0')).toEqual(null)
      expect(parseOpacityFromKeyboard('a33')).toEqual('33%')
      expect(parseOpacityFromKeyboard('0')).toEqual('100%')
      expect(parseOpacityFromKeyboard('100')).toEqual('100%')
    })

    it('single-digit input should be the digit with a `0%` appended', () => {
      fc.assert(
        fc.property(fc.oneof(...digits), (d) => {
          fc.pre(d !== '0')
          expect(parseOpacityFromKeyboard(d)).toEqual(d + '0%')
        }),
      )
    })

    it('two-digit input should be the digits with a % appended', () => {
      fc.assert(
        fc.property(fc.oneof(...digits), fc.oneof(...digits), (a, b) => {
          expect(parseOpacityFromKeyboard(a + b)).toEqual(a + b + '%')
        }),
      )
    })

    it('when there are more than two digits, input should be the last two digits with a % appended', () => {
      fc.assert(
        fc.property(fc.oneof(...digits), fc.oneof(...digits), fc.oneof(...digits), (a, b, c) => {
          fc.pre(a + b + c !== '100')
          expect(parseOpacityFromKeyboard(a + b + c)).toEqual(b + c + '%')
        }),
      )
    })
  })
})
