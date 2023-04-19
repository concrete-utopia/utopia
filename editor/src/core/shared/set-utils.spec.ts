import * as fc from 'fast-check'
import { intersection } from './set-utils'

describe('intersection', () => {
  it('intersection of overlapping sets', () => {
    const result = intersection([new Set([1, 2, 3, 4]), new Set([3, 4, 5, 6])])
    expect(result).toEqual(new Set([3, 4]))
  })

  it('intersection of disjunct sets', () => {
    const result = intersection([new Set([1, 2, 3]), new Set([4, 5, 6])])
    expect(result).toEqual(new Set([]))
  })

  it('intersection is commutative', () => {
    fc.assert(
      fc.property(fc.set(fc.nat()), fc.set(fc.nat()), (left, right) => {
        const resultA = intersection([new Set(left), new Set(right)])
        const resultB = intersection([new Set(right), new Set(left)])
        expect(resultA).toEqual(resultB)
      }),
    )
  })

  it('intersection is of a set with itself is the original set', () => {
    fc.assert(
      fc.property(fc.set(fc.nat()), (xs) => {
        const resultA = intersection([new Set(xs), new Set(xs)])
        expect(resultA).toEqual(new Set(xs))
      }),
    )
  })
})
