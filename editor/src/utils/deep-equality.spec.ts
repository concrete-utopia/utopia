import * as FastCheck from 'fast-check'
import type { Arbitrary } from 'fast-check'
import {
  arrayDeepEquality,
  createCallFromEqualsFunction,
  createCallWithShallowEquals,
  createCallWithTripleEquals,
  keepDeepEqualityResult,
  nullableDeepEquality,
  objectDeepEquality,
  undefinableDeepEquality,
} from './deep-equality'

describe('createCallWithTripleEquals', () => {
  const oldValue: Array<number> = [1, 2, 3]
  const newSameValue: Array<number> = [1, 2, 3]
  const newDifferentValue: Array<number> = [4, 5, 6]

  const equalityCall = createCallWithTripleEquals<Array<number>>()

  it('same reference returns the same reference', () => {
    const result = equalityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value but referentially different returns different reference', () => {
    const result = equalityCall(oldValue, newSameValue)
    expect(result.value).toBe(newSameValue)
    expect(result.areEqual).toEqual(false)
  })
  it('different value returns different reference', () => {
    const result = equalityCall(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('createCallWithShallowEquals', () => {
  const oldA = {}
  const oldB = {}
  const oldC = {}
  const newC = {}

  const oldValue = {
    a: oldA,
    b: oldB,
    c: oldC,
  }
  const newSameValue = {
    a: oldA,
    b: oldB,
    c: oldC,
  }
  const newDifferentValue = {
    a: oldA,
    b: oldB,
    c: newC,
  }

  const equalityCall = createCallWithShallowEquals<any>()

  it('same reference returns the same reference', () => {
    const result = equalityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = equalityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = equalityCall(oldValue, newDifferentValue)
    expect(result.value.a).toBe(oldValue.a)
    expect(result.value.b).toBe(oldValue.b)
    expect(result.value.c).toBe(newDifferentValue.c)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

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

describe('arrayDeepEquality', () => {
  const oldValue: Array<Array<number>> = [[1], [2], [3]]
  const newSameValue: Array<Array<number>> = [[1], [2], [3]]
  const newDifferentValue: Array<Array<number>> = [[1], [2], [4]]

  const numberArrayArrayEquality = arrayDeepEquality(
    arrayDeepEquality(
      createCallFromEqualsFunction((first: number, second: number) => first === second),
    ),
  )

  it('empty arrays are equal', () => {
    const actualResult = numberArrayArrayEquality([], [])
    const expectedResult = keepDeepEqualityResult([], true)
    expect(actualResult).toEqual(expectedResult)
  })
  it('same reference returns the same reference', () => {
    const result = numberArrayArrayEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = numberArrayArrayEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = numberArrayArrayEquality(oldValue, newDifferentValue)
    expect(result.value[0]).toEqual(oldValue[0])
    expect(result.value[1]).toEqual(oldValue[1])
    expect(result.value[2]).toEqual(newDifferentValue[2])
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('objectDeepEquality', () => {
  const oldA = {
    thing: 1,
  }
  const oldB = {
    thing: 2,
  }
  const oldC = {
    thing: 3,
  }
  const newC = {
    thing: 4,
  }

  const oldValue = {
    a: oldA,
    b: oldB,
    c: oldC,
  }
  const newSameValue = {
    a: oldA,
    b: oldB,
    c: oldC,
  }
  const newDifferentValue = {
    a: oldA,
    b: oldB,
    c: newC,
  }

  const equalityCall = objectDeepEquality(createCallWithShallowEquals<any>())

  it('same reference returns the same reference', () => {
    const result = equalityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = equalityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = equalityCall(oldValue, newDifferentValue)
    expect(result.value.a).toBe(oldValue.a)
    expect(result.value.b).toBe(oldValue.b)
    expect(result.value.c).toBe(newDifferentValue.c)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('nullableDeepEquality', () => {
  const oldValue: Array<number> = [1, 2, 3]
  const newSameValue: Array<number> = [1, 2, 3]
  const newDifferentValue: Array<number> = [4, 5, 6]

  const equalityCall = nullableDeepEquality(createCallWithTripleEquals<Array<number>>())

  it('same reference returns the same reference', () => {
    const result = equalityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value but referentially different returns different reference', () => {
    const result = equalityCall(oldValue, newSameValue)
    expect(result.value).toBe(newSameValue)
    expect(result.areEqual).toEqual(false)
  })
  it('different value returns different reference', () => {
    const result = equalityCall(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
  it('null for both returns true', () => {
    const result = equalityCall(null, null)
    expect(result.value).toBeNull()
    expect(result.areEqual).toEqual(true)
  })
  it('null for old returns new', () => {
    const result = equalityCall(null, newSameValue)
    expect(result.value).toBe(newSameValue)
    expect(result.areEqual).toEqual(false)
  })
  it('null for new returns new', () => {
    const result = equalityCall(oldValue, null)
    expect(result.value).toBeNull()
    expect(result.areEqual).toEqual(false)
  })
})

describe('undefinableDeepEquality', () => {
  const oldValue: Array<number> = [1, 2, 3]
  const newSameValue: Array<number> = [1, 2, 3]
  const newDifferentValue: Array<number> = [4, 5, 6]

  const equalityCall = undefinableDeepEquality(createCallWithTripleEquals<Array<number>>())

  it('same reference returns the same reference', () => {
    const result = equalityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value but referentially different returns different reference', () => {
    const result = equalityCall(oldValue, newSameValue)
    expect(result.value).toBe(newSameValue)
    expect(result.areEqual).toEqual(false)
  })
  it('different value returns different reference', () => {
    const result = equalityCall(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
  it('undefined for both returns true', () => {
    const result = equalityCall(undefined, undefined)
    expect(result.value).toBeUndefined()
    expect(result.areEqual).toEqual(true)
  })
  it('undefined for old returns new', () => {
    const result = equalityCall(undefined, newSameValue)
    expect(result.value).toBe(newSameValue)
    expect(result.areEqual).toEqual(false)
  })
  it('undefined for new returns new', () => {
    const result = equalityCall(oldValue, undefined)
    expect(result.value).toBeUndefined()
    expect(result.areEqual).toEqual(false)
  })
})
