import type { Compare } from './compare'
import {
  comparePrimitive,
  compareNullable,
  compareUndefined,
  compareOn,
  compareField,
  compareArray,
  compareCompose,
} from './compare'

interface InnerTestObject {
  someString: string
  trueOrFalse: boolean
}

interface TestObject {
  aNumber: number
  anInnerValue: InnerTestObject
}

function testObject(someString: string, trueOrFalse: boolean, aNumber: number): TestObject {
  return {
    aNumber: aNumber,
    anInnerValue: {
      someString: someString,
      trueOrFalse: trueOrFalse,
    },
  }
}

function checkNonEqualValues<T>(lesser: T, greater: T, compare: Compare<T>): void {
  expect(compare(lesser, greater)).toBeLessThan(0)
  expect(compare(greater, lesser)).toBeGreaterThan(0)
}

function checkEqualValues<T>(makeValue: () => T, compare: Compare<T>): void {
  expect(compare(makeValue(), makeValue())).toEqual(0)
}

describe('comparePrimitive', () => {
  it('works for booleans', () => {
    checkNonEqualValues(false, true, comparePrimitive)
    checkEqualValues(() => true, comparePrimitive)
    checkEqualValues(() => false, comparePrimitive)
  })
  it('works for strings', () => {
    checkNonEqualValues('a', 'b', comparePrimitive)
    checkNonEqualValues('aa', 'aaa', comparePrimitive)
    checkEqualValues(() => 'aaa', comparePrimitive)
    checkEqualValues(() => '', comparePrimitive)
  })
  it('works for numbers', () => {
    checkNonEqualValues(-10, 10, comparePrimitive)
    checkNonEqualValues(0, 1000000, comparePrimitive)
    checkNonEqualValues(-1000000, 0, comparePrimitive)
    checkEqualValues(() => 0, comparePrimitive)
    checkEqualValues(() => 1000000, comparePrimitive)
    checkEqualValues(() => -1000000, comparePrimitive)
  })
})

describe('compareNullable', () => {
  it('works the same for non-null values', () => {
    checkNonEqualValues(-10, 10, compareNullable(comparePrimitive))
    checkNonEqualValues(0, 1000000, compareNullable(comparePrimitive))
    checkNonEqualValues(-1000000, 0, compareNullable(comparePrimitive))
    checkEqualValues(() => 0, compareNullable(comparePrimitive))
    checkEqualValues(() => 1000000, compareNullable(comparePrimitive))
    checkEqualValues(() => -1000000, compareNullable(comparePrimitive))
  })
  it('treats null values as less than anything else', () => {
    checkNonEqualValues(null, 10, compareNullable(comparePrimitive))
    checkNonEqualValues(null, 'hat', compareNullable(comparePrimitive))
    checkNonEqualValues(null, false, compareNullable(comparePrimitive))
  })
  it('treats null values as equal', () => {
    checkEqualValues(() => null, compareNullable<string>(comparePrimitive))
  })
})

describe('compareUndefined', () => {
  it('works the same for non-undefined values', () => {
    checkNonEqualValues(-10, 10, compareUndefined(comparePrimitive))
    checkNonEqualValues(0, 1000000, compareUndefined(comparePrimitive))
    checkNonEqualValues(-1000000, 0, compareUndefined(comparePrimitive))
    checkEqualValues(() => 0, compareUndefined(comparePrimitive))
    checkEqualValues(() => 1000000, compareUndefined(comparePrimitive))
    checkEqualValues(() => -1000000, compareUndefined(comparePrimitive))
  })
  it('treats undefined values as less than anything else', () => {
    checkNonEqualValues(undefined, 10, compareUndefined(comparePrimitive))
    checkNonEqualValues(undefined, 'hat', compareUndefined(comparePrimitive))
    checkNonEqualValues(undefined, false, compareUndefined(comparePrimitive))
  })
  it('treats undefined values as equal', () => {
    checkEqualValues(() => undefined, compareUndefined<string>(comparePrimitive))
  })
})

function numberToString(n: number): string {
  return `${n}`
}

describe('compareOn', () => {
  it('works on transformed values', () => {
    checkNonEqualValues(-10, 10, compareOn(numberToString, comparePrimitive))
    checkNonEqualValues(0, 1000000, compareOn(numberToString, comparePrimitive))
    checkNonEqualValues(-1000000, 0, compareOn(numberToString, comparePrimitive))
    checkEqualValues(() => 0, compareOn(numberToString, comparePrimitive))
    checkEqualValues(() => 1000000, compareOn(numberToString, comparePrimitive))
    checkEqualValues(() => -1000000, compareOn(numberToString, comparePrimitive))
    // This shows up the difference of it being string compared.
    checkNonEqualValues(88, 9, compareOn(numberToString, comparePrimitive))
  })
})

describe('compareField', () => {
  it('compares the values when both keys are present', () => {
    checkNonEqualValues({ a: 10 }, { a: 20 }, compareField('a', comparePrimitive))
    checkEqualValues(() => {
      return {
        a: 10,
      }
    }, compareField('a', comparePrimitive))
  })
})

describe('compareArray', () => {
  it('treats shorter arrays as being less than longer arrays', () => {
    checkNonEqualValues([999, 1000], [1, 2, 3], compareArray(comparePrimitive))
    checkNonEqualValues(['x', 'y', 'z'], ['a', 'b', 'c', 'd'], compareArray(comparePrimitive))
  })
  it('for identical length arrays compares the values within', () => {
    checkNonEqualValues([1, 2, 9], [1, 2, 10], compareArray(comparePrimitive))
    checkNonEqualValues(['a', 'b', 'c'], ['a', 'b', 'd'], compareArray(comparePrimitive))
    checkEqualValues(() => [1, 2, 3], compareArray(comparePrimitive))
    checkEqualValues(() => ['a', 'b', 'c'], compareArray(comparePrimitive))
    checkEqualValues(() => [], compareArray<string>(comparePrimitive))
  })
})

const compareTestObject: Compare<TestObject> = compareCompose(
  compareField('aNumber', comparePrimitive),
  compareField(
    'anInnerValue',
    compareCompose(
      compareField('someString', comparePrimitive),
      compareField('trueOrFalse', comparePrimitive),
    ),
  ),
)

describe('compareCompose', () => {
  it('handles unequal values', () => {
    checkNonEqualValues(testObject('a', false, 100), testObject('a', false, 200), compareTestObject)
    checkNonEqualValues(testObject('a', false, 100), testObject('a', true, 100), compareTestObject)
    checkNonEqualValues(testObject('a', false, 100), testObject('b', false, 100), compareTestObject)
  })
  it('handles equal values', () => {
    checkEqualValues(() => testObject('a', false, 100), compareTestObject)
    checkEqualValues(() => testObject('a', false, 100), compareTestObject)
    checkEqualValues(() => testObject('a', false, 100), compareTestObject)
  })
})
