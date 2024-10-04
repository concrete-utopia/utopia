import {
  aperture,
  intersection,
  mapAndFilter,
  possiblyUniqueInArray,
  sortArrayByAndReturnPermutation,
  revertArrayOrder,
  strictEvery,
} from './array-utils'

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

describe('sortArrayByAndReturnPermutation', () => {
  it('should sort the array', () => {
    const { sortedArray } = sortArrayByAndReturnPermutation([3, 1, 2], (a) => a)
    expect(sortedArray).toEqual([1, 2, 3])
  })
  it('should respect the sort order', () => {
    const { sortedArray } = sortArrayByAndReturnPermutation([3, 1, 2], (a) => a, false)
    expect(sortedArray).toEqual([3, 2, 1])
  })
  it('should return the permutation that reverses the sort', () => {
    const originalArray = [10, 5, 6, 32, 102, 7, 91]
    const { sortedArray, permutation } = sortArrayByAndReturnPermutation(originalArray, (a) => a)
    expect(sortedArray).toEqual([5, 6, 7, 10, 32, 91, 102])
    const reversedToOriginal = revertArrayOrder(sortedArray, permutation)
    expect(reversedToOriginal).toEqual(originalArray)
  })
})

describe('mapAndFilter', () => {
  const input = [1, 2, 3, 4, 5]
  const mapFn = (n: number) => n + 10
  const filter = (n: number) => n % 2 === 0

  it('should correctly map an array', () => {
    const actualResult = mapAndFilter(mapFn, () => true, input)
    expect(actualResult).toEqual([11, 12, 13, 14, 15])
  })
  it('should correctly filter an array', () => {
    const actualResult = mapAndFilter((n) => n, filter, input)
    expect(actualResult).toEqual([2, 4])
  })
  it('should correctly map and filter an array', () => {
    const actualResult = mapAndFilter(mapFn, filter, input)
    expect(actualResult).toEqual([12, 14])
  })
})

describe('strictEvery', () => {
  it('returns false for empty arrays', () => {
    // Because `[].every()` always returns true, and that has caused so much pain
    expect([].every(() => false)).toBeTruthy()
    expect(strictEvery([], () => false)).toBeFalsy()
  })

  it('returns true for non-empty arrays where the predicate is satisfied', () => {
    expect([1].every(() => true)).toBeTruthy()
    expect(strictEvery([1], () => true)).toBeTruthy()
  })

  it('returns false for non-empty arrays where the predicate is not satisfied', () => {
    expect([1].every(() => false)).toBeFalsy()
    expect(strictEvery([1], () => false)).toBeFalsy()
  })
})

it('possiblyUniqueInArray', () => {
  const arr = [0, 1, 2, 3, 4]
  expect(possiblyUniqueInArray(arr, [1, 2, 4], 3)).toEqual(3)
  expect(possiblyUniqueInArray(arr, [1, 2, 4], 4)).toEqual(0)
  expect(possiblyUniqueInArray(arr, [1, 2, 4], 0)).toEqual(0)
  expect(possiblyUniqueInArray(arr, [1, 2, 4], 1)).toEqual(3)
  expect(possiblyUniqueInArray(arr, [1, 2, 4], 2)).toEqual(3)
  expect(possiblyUniqueInArray(arr, [1, 2, 4], 5)).toEqual(5)
  expect(possiblyUniqueInArray(arr, arr, 2)).toEqual(2)
  expect(possiblyUniqueInArray([], [], 2)).toEqual(2)
})
