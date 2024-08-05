/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert"] }] */
import * as FastCheck from 'fast-check'
import fastDeepEquals from 'fast-deep-equal'
import { left, right } from '../either'
import { filtered, fromField, not, traverseArray } from './optic-creators'
import {
  allBy,
  anyBy,
  exists,
  foldLOf,
  forEachOf,
  modify,
  set,
  toArrayOf,
  toFirst,
  toLast,
  unsafeGet,
} from './optic-utilities'

interface FromFieldTest {
  testField: string
  otherField: number
}

const fromFieldTestArbitrary: FastCheck.Arbitrary<FromFieldTest> = FastCheck.tuple(
  FastCheck.string(),
  FastCheck.integer(),
).map(([testFieldValue, otherFieldValue]) => {
  return {
    testField: testFieldValue,
    otherField: otherFieldValue,
  }
})

describe('toArrayOf', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(toArrayOf(not, bool), [!bool])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        toArrayOf(fromField<FromFieldTest, 'testField'>('testField'), fromFieldTest),
        [fromFieldTest.testField],
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      return fastDeepEquals(
        toArrayOf(
          filtered((toCheck: number) => toCheck > 1000),
          number,
        ),
        number > 1000 ? [number] : [],
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (integerArray) => {
      return fastDeepEquals(toArrayOf(traverseArray(), integerArray), integerArray)
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('toFirst', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(toFirst(not, bool), right(!bool))
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        toFirst(fromField<FromFieldTest, 'testField'>('testField'), fromFieldTest),
        right(fromFieldTest.testField),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      return fastDeepEquals(
        toFirst(
          filtered((toCheck: number) => toCheck > 1000),
          number,
        ),
        number > 1000 ? right(number) : left('No values present.'),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (integerArray) => {
      return fastDeepEquals(
        toFirst(traverseArray(), integerArray),
        integerArray.length === 0 ? left('No values present.') : right(integerArray[0]),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('toLast', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(toLast(not, bool), right(!bool))
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        toLast(fromField<FromFieldTest, 'testField'>('testField'), fromFieldTest),
        right(fromFieldTest.testField),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      return fastDeepEquals(
        toLast(
          filtered((toCheck: number) => toCheck > 1000),
          number,
        ),
        number > 1000 ? right(number) : left('No values present.'),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (integerArray) => {
      return fastDeepEquals(
        toLast(traverseArray(), integerArray),
        integerArray.length === 0
          ? left('No values present.')
          : right(integerArray[integerArray.length - 1]),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('foldLOf', () => {
  const foldNumbers = (r: number, a: number) => r + a
  const foldBooleans = (r: boolean, a: boolean) => r || a
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(foldLOf(not, foldBooleans, false, bool), !bool)
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        foldLOf(
          fromField<FromFieldTest, 'otherField'>('otherField'),
          foldNumbers,
          100,
          fromFieldTest,
        ),
        fromFieldTest.otherField + 100,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      return fastDeepEquals(
        foldLOf(
          filtered((toCheck: number) => toCheck > 1000),
          foldNumbers,
          100,
          number,
        ),
        number > 1000 ? number + 100 : 100,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (numbers) => {
      return fastDeepEquals(
        foldLOf(traverseArray<number>(), foldNumbers, 100, numbers),
        numbers.reduce(foldNumbers, 100),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('anyBy', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(
        anyBy(not, (b) => b, bool),
        !bool,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        anyBy(fromField<FromFieldTest, 'otherField'>('otherField'), (n) => n > 1000, fromFieldTest),
        fromFieldTest.otherField > 1000,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      return fastDeepEquals(
        anyBy(
          filtered((toCheck: number) => toCheck > 1000),
          (n) => n < 100000,
          number,
        ),
        number > 1000 && number < 100000,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (numbers) => {
      return fastDeepEquals(
        anyBy(traverseArray<number>(), (n) => n > 1000, numbers),
        numbers.some((n) => n > 1000),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('allBy', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(
        allBy(not, (b) => b, bool),
        !bool,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        allBy(fromField<FromFieldTest, 'otherField'>('otherField'), (n) => n > 1000, fromFieldTest),
        fromFieldTest.otherField > 1000,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      const actualResult = allBy(
        filtered((toCheck: number) => toCheck > 1000),
        (n) => n < 100000,
        number,
      )
      const expectedResult = (number > 1000 && number < 100000) || number <= 1000
      return fastDeepEquals(actualResult, expectedResult)
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (numbers) => {
      return fastDeepEquals(
        allBy(traverseArray<number>(), (n) => n > 1000, numbers),
        numbers.every((n) => n > 1000),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('modify', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(
      FastCheck.tuple(FastCheck.boolean(), FastCheck.boolean()),
      ([bool, newValue]) => {
        return fastDeepEquals(
          modify(not, () => newValue, bool),
          !newValue,
        )
      },
    )
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        modify(
          fromField<FromFieldTest, 'otherField'>('otherField'),
          (n) => n + 1000,
          fromFieldTest,
        ),
        {
          ...fromFieldTest,
          otherField: fromFieldTest.otherField + 1000,
        },
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      const actualResult = modify(
        filtered((toCheck: number) => toCheck > 1000),
        (n) => n + 100000,
        number,
      )
      const expectedResult = number > 1000 ? number + 100000 : number
      return fastDeepEquals(actualResult, expectedResult)
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (numbers) => {
      return fastDeepEquals(
        modify(traverseArray<number>(), (n) => n + 1000, numbers),
        numbers.map((n) => n + 1000),
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('set', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(
      FastCheck.tuple(FastCheck.boolean(), FastCheck.boolean()),
      ([bool, newValue]) => {
        return fastDeepEquals(set(not, newValue, bool), !newValue)
      },
    )
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(
      FastCheck.tuple(fromFieldTestArbitrary, FastCheck.integer()),
      ([fromFieldTest, newValue]) => {
        return fastDeepEquals(
          set(fromField<FromFieldTest, 'otherField'>('otherField'), newValue, fromFieldTest),
          {
            ...fromFieldTest,
            otherField: newValue,
          },
        )
      },
    )
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(
      FastCheck.tuple(FastCheck.integer(), FastCheck.integer()),
      ([number, newValue]) => {
        const actualResult = set(
          filtered((toCheck: number) => toCheck > 1000),
          newValue,
          number,
        )
        const expectedResult = number > 1000 ? newValue : number
        return fastDeepEquals(actualResult, expectedResult)
      },
    )
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(
      FastCheck.tuple(FastCheck.array(FastCheck.integer()), FastCheck.integer()),
      ([numbers, newValue]) => {
        return fastDeepEquals(
          set(traverseArray<number>(), newValue, numbers),
          numbers.map(() => newValue),
        )
      },
    )
    FastCheck.assert(property, { verbose: true })
  })
})

describe('exists', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      return fastDeepEquals(exists(not, bool), true)
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      return fastDeepEquals(
        exists(fromField<FromFieldTest, 'testField'>('testField'), fromFieldTest),
        true,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      return fastDeepEquals(
        exists(
          filtered((toCheck: number) => toCheck > 1000),
          number,
        ),
        number > 1000,
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (integerArray) => {
      return fastDeepEquals(exists(traverseArray(), integerArray), integerArray.length > 0)
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('forEachOf', () => {
  it('returns the expected value with an ISO', () => {
    const property = FastCheck.property(FastCheck.boolean(), (bool) => {
      let result: Array<boolean> = []
      forEachOf(not, bool, (b) => {
        result.push(b)
      })
      return fastDeepEquals(result, [!bool])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a LENS', () => {
    const property = FastCheck.property(fromFieldTestArbitrary, (fromFieldTest) => {
      let result: Array<string> = []
      forEachOf(fromField<FromFieldTest, 'testField'>('testField'), fromFieldTest, (s) => {
        result.push(s)
      })
      return fastDeepEquals(result, [fromFieldTest.testField])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a PRISM', () => {
    const property = FastCheck.property(FastCheck.integer(), (number) => {
      let result: Array<number> = []
      forEachOf(
        filtered((toCheck: number) => toCheck > 1000),
        number,
        (n) => {
          result.push(n)
        },
      )
      return fastDeepEquals(result, number > 1000 ? [number] : [])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the expected value with a TRAVERSAL', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (integerArray) => {
      let result: Array<number> = []
      forEachOf(traverseArray<number>(), integerArray, (n) => {
        result.push(n)
      })
      return fastDeepEquals(result, integerArray)
    })
    FastCheck.assert(property, { verbose: true })
  })
})
