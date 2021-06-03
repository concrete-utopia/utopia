import { intersection } from './array-utils'

describe('intersection', () => {
  it('two empty arrays should return an empty array', () => {
    const actualResult = intersection([], [])
    expect(actualResult).toEqual([])
  })
  it('first empty array should return an empty array', () => {
    const actualResult = intersection([], [1, 2, 3])
    expect(actualResult).toEqual([])
  })
  it('second empty array should return an empty array', () => {
    const actualResult = intersection([1, 2, 3], [])
    expect(actualResult).toEqual([])
  })
  it('identical arrays should return the identical array', () => {
    const actualResult = intersection([1, 2, 3], [1, 2, 3])
    expect(actualResult).toEqual([1, 2, 3])
  })
  it('lots of duplicated values just includes one', () => {
    const actualResult = intersection([1, 1, 1], [1, 1, 1])
    expect(actualResult).toEqual([1])
  })
})
