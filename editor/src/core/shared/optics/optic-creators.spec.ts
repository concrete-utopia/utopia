import * as FastCheck from 'fast-check'
import type { Either } from '../either'
import { left, right } from '../either'
import {
  eitherLeft,
  eitherRight,
  filtered,
  fromField,
  fromObjectField,
  fromTypeGuard,
  not,
  notNull,
  notNullOrUndefined,
  notUndefined,
  traverseArray,
} from './optic-creators'
import { isoLaws, lensLaws, prismLaws, traversalLaws } from './optic-laws.test-utils'
import fastDeepEquals from 'fast-deep-equal'
import { exists, toArrayOf } from './optic-utilities'

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
  it('returns the value in the right if it is a right', () => {
    const property = FastCheck.property(FastCheck.string(), (s) => {
      return fastDeepEquals(toArrayOf(eitherRight(), right(s)), [s])
    })
    FastCheck.assert(property, { verbose: true })
  })
  it('returns nothing if it is a left', () => {
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
      (s) => s.concat('func1 append'),
      (s) => s.concat('func2 append'),
    )
  })
})

interface FirstType {
  type: 'FIRST'
  aNumber: number
}

interface SecondType {
  type: 'SECOND'
  aString: string
}

type UnionType = FirstType | SecondType

function isFirstType(unionType: UnionType): unionType is FirstType {
  return unionType.type === 'FIRST'
}

function isSecondType(unionType: UnionType): unionType is SecondType {
  return unionType.type === 'SECOND'
}

const firstTypeArbitrary: FastCheck.Arbitrary<FirstType> = FastCheck.integer().map((aNumber) => {
  return {
    type: 'FIRST',
    aNumber: aNumber,
  }
})

const secondTypeArbitrary: FastCheck.Arbitrary<SecondType> = FastCheck.string().map((aString) => {
  return {
    type: 'SECOND',
    aString: aString,
  }
})

const unionTypeArbitrary: FastCheck.Arbitrary<UnionType> = FastCheck.oneof<UnionType>(
  firstTypeArbitrary,
  secondTypeArbitrary,
)

describe('fromTypeGuard', () => {
  it('obeys the laws', () => {
    prismLaws<UnionType, SecondType>(
      fromTypeGuard(isSecondType),
      unionTypeArbitrary,
      secondTypeArbitrary,
      (a) => {
        return {
          ...a,
          aString: a.aString.concat(' func1 append'),
        }
      },
      (a) => {
        return {
          ...a,
          aString: a.aString.concat(' func2 append'),
        }
      },
    )
  })
  it('matches the guard behaviour', () => {
    const property = FastCheck.property(unionTypeArbitrary, (unionType) => {
      return (
        exists(fromTypeGuard(isFirstType), unionType) === isFirstType(unionType) &&
        exists(fromTypeGuard(isSecondType), unionType) === isSecondType(unionType)
      )
    })
    FastCheck.assert(property, { verbose: true })
  })
})

describe('notNull', () => {
  it('obeys the laws', () => {
    prismLaws<string | null, string>(
      notNull(),
      FastCheck.oneof(FastCheck.string(), FastCheck.constant(null)),
      FastCheck.string(),
      (s) => s.concat('func1 append'),
      (s) => s.concat('func2 append'),
    )
  })
})

describe('notUndefined', () => {
  it('obeys the laws', () => {
    prismLaws<string | undefined, string>(
      notUndefined(),
      FastCheck.oneof(FastCheck.string(), FastCheck.constant(undefined)),
      FastCheck.string(),
      (s) => s.concat('func1 append'),
      (s) => s.concat('func2 append'),
    )
  })
})

describe('notNullOrUndefined', () => {
  it('obeys the laws', () => {
    prismLaws<string | null | undefined, string>(
      notNullOrUndefined(),
      FastCheck.oneof(FastCheck.string(), FastCheck.constant(null), FastCheck.constant(undefined)),
      FastCheck.string(),
      (s) => s.concat('func1 append'),
      (s) => s.concat('func2 append'),
    )
  })
})
