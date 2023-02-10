import { intersection, wrapValue } from './math-utils'
import * as fc from 'fast-check'

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
      expect(wrapValue(14, 10, 15)).toEqual(14)
      expect(wrapValue(18, 10, 15)).toEqual(12)
      expect(wrapValue(9, 10, 15)).toEqual(15)
      expect(wrapValue(7, 10, 15)).toEqual(13)
      expect(wrapValue(-4, -7, -3)).toEqual(-4)
      expect(wrapValue(-11, -7, -3)).toEqual(-6)
      expect(wrapValue(-2, -7, -3)).toEqual(-7)
    })
  })

  describe('intersection', () => {
    it('intersection of overlapping sets', () => {
      const result = intersection([new Set([1, 2, 3, 4]), new Set([3, 4, 5, 6])])
      expect([...result]).toEqual([3, 4])
    })

    it('intersection of disjunct sets', () => {
      const result = intersection([new Set([1, 2, 3]), new Set([4, 5, 6])])
      expect([...result]).toEqual([])
    })

    it('intersection is commutative', () => {
      fc.assert(
        fc.property(fc.set(fc.nat()), fc.array(fc.nat()), (left, right) => {
          const resultA = intersection([new Set(left), new Set(right)])
          const resultB = intersection([new Set(right), new Set(left)])
          expect([...resultA]).toEqual([...resultB])
        }),
      )
    })

    it('intersection is of a set with itself is the original set', () => {
      fc.assert(
        fc.property(fc.set(fc.nat()), (xs) => {
          const resultA = intersection([new Set(xs), new Set(xs)])
          expect([...resultA]).toEqual([...xs])
        }),
      )
    })
  })
})
