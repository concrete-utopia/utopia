import type { Either } from './either'
import {
  left,
  isLeft,
  right,
  isRight,
  foldEither,
  flatMapEither,
  mapEither,
  bimapEither,
  eitherToMaybe,
  eitherFromMaybe,
  sequenceEither,
  equalEither,
  traverseEither,
} from './either'

describe('isLeft', () => {
  it('returns true for a left', () => {
    expect(isLeft(left(1))).toEqual(true)
  })
  it('returns false for a right', () => {
    expect(isLeft(right(1))).toEqual(false)
  })
})

describe('isRight', () => {
  it('returns false for a left', () => {
    expect(isRight(left(1))).toEqual(false)
  })
  it('returns true for a right', () => {
    expect(isRight(right(1))).toEqual(true)
  })
})

describe('foldEither', () => {
  it('applies the left fold on a left', () => {
    const actualResult = foldEither(
      (l: number) => l + 777,
      (r: number) => {
        throw new Error()
      },
      left(200),
    )
    expect(actualResult).toEqual(977)
  })
  it('applies the right fold on a right', () => {
    const actualResult = foldEither(
      (l: number) => {
        throw new Error()
      },
      (r: number) => r + 777,
      right(200),
    )
    expect(actualResult).toEqual(977)
  })
})

describe('flatMapEither', () => {
  it('does nothing on a left, with a left returned', () => {
    const actualResult = flatMapEither((r: number) => left(r + 100), left(200))
    expect(actualResult).toEqual(left(200))
  })
  it('does nothing on a left, with a right returned', () => {
    const actualResult = flatMapEither((r: number) => right(r + 100), left(200))
    expect(actualResult).toEqual(left(200))
  })
  it('applies the transform on a right, with a left returned', () => {
    const actualResult = flatMapEither((r: number) => left(r + 100), right(200))
    expect(actualResult).toEqual(left(300))
  })
  it('applies the transform on a right, with a right returned', () => {
    const actualResult = flatMapEither((r: number) => right(r + 100), right(200))
    expect(actualResult).toEqual(right(300))
  })
})

describe('mapEither', () => {
  it('does nothing on a left', () => {
    const actualResult = mapEither((r: number) => r + 100, left(200))
    expect(actualResult).toEqual(left(200))
  })
  it('applies the transform on a right', () => {
    const actualResult = mapEither((r: number) => r + 100, right(200))
    expect(actualResult).toEqual(right(300))
  })
})

describe('bimapEither', () => {
  it('applies the transform on a left', () => {
    const actualResult = bimapEither(
      (l: number) => l + 99,
      (r: number) => r + 100,
      left(200),
    )
    expect(actualResult).toEqual(left(299))
  })
  it('applies the transform on a right', () => {
    const actualResult = bimapEither(
      (l: number) => l + 99,
      (r: number) => r + 100,
      right(200),
    )
    expect(actualResult).toEqual(right(300))
  })
})

describe('eitherToMaybe', () => {
  it('returns a null for a left', () => {
    expect(eitherToMaybe(left(100))).toEqual(null)
  })
  it('returns the value for a right', () => {
    expect(eitherToMaybe(right(100))).toEqual(100)
  })
})

describe('eitherFromMaybe', () => {
  it('returns a left for a null', () => {
    expect(eitherFromMaybe('fail', null)).toEqual(left('fail'))
  })
  it('returns a right for a non-null value', () => {
    expect(eitherFromMaybe('fail', 100)).toEqual(right(100))
  })
})

describe('sequenceEither', () => {
  it('if a left is present, return it', () => {
    const eithers: Array<Either<string, number>> = [
      right(100),
      left('first fail'),
      right(200),
      left('second fail'),
    ]
    const actualResult = sequenceEither(eithers)
    expect(actualResult).toEqual(left('first fail'))
  })
  it('if no lefts are present, return all the values in the rights', () => {
    const eithers: Array<Either<string, number>> = [right(100), right(200)]
    const actualResult = sequenceEither(eithers)
    expect(actualResult).toEqual(right([100, 200]))
  })
})

function numbersEqual(n1: number, n2: number): boolean {
  return n1 === n2
}

function stringsEqual(s1: string, s2: string): boolean {
  return s1 === s2
}

describe('equalEither', () => {
  it('with a left and a right, return false', () => {
    const actualResult = equalEither(stringsEqual, numbersEqual, left('fail'), right(100))
    expect(actualResult).toEqual(false)
  })
  it('with a right and a left, return false', () => {
    const actualResult = equalEither(stringsEqual, numbersEqual, right(100), left('fail'))
    expect(actualResult).toEqual(false)
  })
  it('with inequal lefts, return false', () => {
    const actualResult = equalEither(stringsEqual, numbersEqual, left('hat'), left('fail'))
    expect(actualResult).toEqual(false)
  })
  it('with inequal rights, return false', () => {
    const actualResult = equalEither(stringsEqual, numbersEqual, right(100), right(200))
    expect(actualResult).toEqual(false)
  })
  it('with equal lefts, return true', () => {
    const actualResult = equalEither(stringsEqual, numbersEqual, left('fail'), left('fail'))
    expect(actualResult).toEqual(true)
  })
  it('with equal rights, return true', () => {
    const actualResult = equalEither(stringsEqual, numbersEqual, right(200), right(200))
    expect(actualResult).toEqual(true)
  })
})

function failWithOddNumbers(n: number): Either<string, number> {
  if (n % 2 === 0) {
    return right(n)
  } else {
    return left('Odd numbers suck.')
  }
}

describe('traverseEither', () => {
  it('if any transform returns a left, returns that', () => {
    const actualResult = traverseEither(failWithOddNumbers, [2, 4, 6, 9, 10, 11])
    expect(actualResult).toEqual(left('Odd numbers suck.'))
  })
  it('if all the transforms return a right, return the accumulated values', () => {
    const actualResult = traverseEither(failWithOddNumbers, [2, 4, 6, 10])
    expect(actualResult).toEqual(right([2, 4, 6, 10]))
  })
})
