import * as FastCheck from 'fast-check'
import { Arbitrary } from 'fast-check'
import {
  arrayDeepEquality,
  createCallFromEqualsFunction,
  keepDeepEqualityResult,
} from './deep-equality'

describe('createCallFromEqualsFunction', () => {
  it('matches the result of the originating function', () => {
    const arbitraryPair: Arbitrary<{ first: number; second: number }> = FastCheck.tuple(
      FastCheck.integer(0, 5),
      FastCheck.integer(0, 5),
    ).map(([first, second]) => {
      return {
        first: first,
        second: second,
      }
    })
    const equalityCall = createCallFromEqualsFunction((first, second) => {
      return first === second
    })
    function checkCall(value: { first: number; second: number }): boolean {
      const { first, second } = value
      const normalEqualityResult = first === second
      const callEqualityResult = equalityCall(first, second)
      const equalityIdentical = normalEqualityResult === callEqualityResult.areEqual
      const expectedValueResult = normalEqualityResult ? first : second
      const valueIdentical = expectedValueResult === callEqualityResult.value
      return (
        normalEqualityResult === callEqualityResult.areEqual && equalityIdentical && valueIdentical
      )
    }
    const prop = FastCheck.property(arbitraryPair, checkCall)
    FastCheck.assert(prop, { verbose: true })
  })
})

const numberArrayEquality = arrayDeepEquality(
  createCallFromEqualsFunction((first: number, second: number) => first === second),
)

describe('arrayDeepEquality', () => {
  it('empty arrays are equal', () => {
    const actualResult = numberArrayEquality([], [])
    const expectedResult = keepDeepEqualityResult([], true)
    expect(actualResult).toEqual(expectedResult)
  })
  it('same reference arrays are equal', () => {
    const array = [1, 2, 3]
    const actualResult = numberArrayEquality(array, array)
    const expectedResult = keepDeepEqualityResult(array, true)
    expect(actualResult).toEqual(expectedResult)
  })
  it('identical arrays are equal', () => {
    const actualResult = numberArrayEquality([1, 2, 3], [1, 2, 3])
    const expectedResult = keepDeepEqualityResult([1, 2, 3], true)
    expect(actualResult).toEqual(expectedResult)
  })
})
