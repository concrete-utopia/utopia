import { aperture, intersection } from './array-utils'

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

describe('aperture', () => {
  it('should return an empty array when given an empty array', () => {
    const actualResult = aperture(3, [])
    expect(actualResult).toEqual([[]])
  })
  it('should return the moving window result expected from a reasonable array', () => {
    const actualResult = aperture(3, [1, 2, 3, 4, 5, 6])
    expect(actualResult).toEqual([
      [1, 2, 3],
      [2, 3, 4],
      [3, 4, 5],
      [4, 5, 6],
    ])
  })
  it('should return an empty array for a negative window size', () => {
    const actualResult = aperture(-3, [1, 2, 3, 4, 5, 6])
    expect(actualResult).toEqual([])
  })
  it('should return a single element array when the window is larger than the input', () => {
    const actualResult = aperture(20, [1, 2, 3, 4, 5, 6])
    expect(actualResult).toEqual([[1, 2, 3, 4, 5, 6]])
  })
})
