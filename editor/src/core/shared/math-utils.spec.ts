import { wrapValue } from './math-utils'

describe('math utils', () => {
  describe('wrapValue', () => {
    it('wraps values inside the given boundaries', async () => {
      expect(wrapValue(0, 0, 5)).toEqual(0)
      expect(wrapValue(3, 0, 5)).toEqual(3)
      expect(wrapValue(5, 0, 5)).toEqual(5)
      expect(wrapValue(6, 0, 5)).toEqual(0)
      expect(wrapValue(8, 0, 5)).toEqual(2)
      expect(wrapValue(14, 0, 5)).toEqual(2)
      expect(wrapValue(-1, 0, 5)).toEqual(5)
      expect(wrapValue(-3, 0, 5)).toEqual(3)
      expect(wrapValue(-14, 0, 5)).toEqual(4)
    })
  })
})
