import * as FastCheck from 'fast-check'
import { Either, left, right } from './either'
import {
  eitherLeft,
  eitherRight,
  filtered,
  fromField,
  fromObjectField,
  not,
  traverseArray,
} from './lens-creators'
import { isoLaws, lensLaws, prismLaws, traversalLaws } from './lens-laws'
import { toArrayOf } from './lens-utilies'
import fastDeepEquals from 'fast-deep-equal'

const eitherStringNumberArbitrary: FastCheck.Arbitrary<Either<string, number>> = FastCheck.tuple(
  FastCheck.integer(),
  FastCheck.string(),
  FastCheck.boolean(),
).map(([rightNumber, leftString, isRight]) => {
  return isRight ? right<string, number>(rightNumber) : left<string, number>(leftString)
})

describe('eitherLeft', () => {
  it('obeys the laws', () => {
    prismLaws<Either<string, number>, string>(
      eitherLeft(),
      eitherStringNumberArbitrary,
      FastCheck.string(),
      (s) => s + ' func1 append',
      (s) => s + ' func2 append',
    )
  })
  it('returns the value in the left if it is a left', () => {
    const property = FastCheck.property(FastCheck.string(), (s) => {
      return fastDeepEquals(toArrayOf(eitherLeft(), left(s)), [s])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns nothing if it is a right', () => {
    const property = FastCheck.property(FastCheck.integer(), (s) => {
      return fastDeepEquals(toArrayOf(eitherLeft(), right(s)), [])
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('eitherRight', () => {
  it('obeys the laws', () => {
    prismLaws<Either<string, number>, number>(
      eitherRight(),
      eitherStringNumberArbitrary,
      FastCheck.integer(),
      (s) => s + 100,
      (s) => s + 10,
    )
  })
  it('returns nothing if it is a left', () => {
    const property = FastCheck.property(FastCheck.string(), (s) => {
      return fastDeepEquals(toArrayOf(eitherRight(), right(s)), [s])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns the value in the right if it is a right', () => {
    const property = FastCheck.property(FastCheck.integer(), (s) => {
      return fastDeepEquals(toArrayOf(eitherRight(), left(s)), [])
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('not', () => {
  it('obeys the laws', () => {
    isoLaws(
      not,
      FastCheck.boolean(),
      FastCheck.boolean(),
      (b) => !b,
      (b) => b,
    )
  })
})

describe('filtered', () => {
  it('obeys the laws', () => {
    prismLaws(
      filtered<number>((n) => n > 100),
      FastCheck.integer(),
      FastCheck.integer().filter((n) => n > 100),
      (n) => n + 1,
      (n) => n + 2,
    )
  })
})

describe('traverseArray', () => {
  it('obeys the laws', () => {
    traversalLaws<Array<number>, number>(
      traverseArray(),
      FastCheck.array(FastCheck.integer()),
      (n) => n + 1,
      (n) => n + 2,
    )
  })
  it('gets the same values as the originating array', () => {
    const property = FastCheck.property(FastCheck.array(FastCheck.integer()), (array) => {
      return fastDeepEquals(toArrayOf(traverseArray(), array), array)
    })
    FastCheck.assert(property, { verbose: true })
  })
})

const fromObjectFieldValueArbitrary: FastCheck.Arbitrary<{ [key: string]: number }> =
  FastCheck.tuple(
    FastCheck.integer(),
    FastCheck.boolean(),
    FastCheck.integer(),
    FastCheck.boolean(),
  ).map(([testFieldValue, testFieldExists, otherFieldValue, otherFieldExists]) => {
    let result: { [key: string]: number } = {}
    if (testFieldExists) {
      result['testfield'] = testFieldValue
    }
    if (otherFieldExists) {
      result['otherfield'] = otherFieldValue
    }
    return result
  })

describe('fromObjectField', () => {
  it('obeys the laws', () => {
    traversalLaws<{ [key: string]: number }, number>(
      fromObjectField('testfield'),
      fromObjectFieldValueArbitrary,
      (n) => n + 1,
      (n) => n + 2,
    )
  })
})

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

describe('fromField', () => {
  it('obeys the laws', () => {
    lensLaws<FromFieldTest, string>(
      fromField('testField'),
      fromFieldTestArbitrary,
      FastCheck.string(),
      (s) => s + 'func1 append',
      (s) => s + 'func2 append',
    )
  })
})
