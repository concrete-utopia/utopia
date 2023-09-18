import { memoize } from './memoize'

describe('memoize', () => {
  type AandB = { a: number; b: number }
  type CandD = { c: number; d: number }

  function isAandB(v: AandB | CandD): v is AandB {
    return v.hasOwnProperty('a')
  }

  // eslint-disable-next-line @typescript-eslint/ban-types
  const testFunctionCount = new Map<Function, number>()

  function createTestFn() {
    const testFn = (first: AandB, second: CandD): { e: number } => {
      const callCount = testFunctionCount.get(testFn) ?? 0
      testFunctionCount.set(testFn, callCount + 1)
      return {
        e: first.a + first.b,
      }
    }

    return testFn
  }

  it('memoizes using shallow equality by default', () => {
    const testFn = createTestFn()
    const memoized = memoize(testFn)
    const firstRun = memoized({ a: 1, b: 2 }, { c: 3, d: 4 })
    const secondRun = memoized({ a: 1, b: 2 }, { c: 3, d: 4 })
    const thirdRun = memoized({ a: 1, b: 2 }, { c: 4, d: 5 })

    expect(firstRun).toBe(secondRun)
    expect(firstRun).not.toBe(thirdRun)
    expect(testFunctionCount.get(testFn)).toEqual(2)
  })

  it('supports a custom maxSize', () => {
    const testFn = createTestFn()
    const memoized = memoize(testFn, { maxSize: 1 })
    const firstRun = memoized({ a: 1, b: 2 }, { c: 3, d: 4 })
    const secondRun = memoized({ a: 1, b: 2 }, { c: 4, d: 5 })
    const thirdRun = memoized({ a: 1, b: 2 }, { c: 3, d: 4 })

    expect(firstRun).not.toBe(secondRun)
    expect(firstRun).not.toBe(thirdRun)
    expect(testFunctionCount.get(testFn)).toEqual(3)

    const memoizedWithLargerMaxSize = memoize(testFn, { maxSize: 2 })
    const fourthRun = memoizedWithLargerMaxSize({ a: 1, b: 2 }, { c: 3, d: 4 })
    const fifthRun = memoizedWithLargerMaxSize({ a: 1, b: 2 }, { c: 4, d: 5 })
    const sixthRun = memoizedWithLargerMaxSize({ a: 1, b: 2 }, { c: 3, d: 4 })

    expect(fourthRun).not.toBe(fifthRun)
    expect(fourthRun).toBe(sixthRun)
    expect(testFunctionCount.get(testFn)).toEqual(5)
  })

  it('can use a custom equality function', () => {
    const testFn = createTestFn()
    const memoized = memoize(testFn, {
      matchesArg: (l: AandB | CandD, r: AandB | CandD) => {
        if (isAandB(l) && isAandB(r)) {
          return l.a === r.a && l.b === r.b
        } else {
          return true
        }
      },
    })
    const firstRun = memoized({ a: 1, b: 2 }, { c: 3, d: 4 })
    const secondRun = memoized({ a: 1, b: 2 }, { c: 3, d: 4 })
    const thirdRun = memoized({ a: 1, b: 2 }, { c: 4, d: 5 })

    expect(firstRun).toBe(secondRun)
    expect(firstRun).toBe(thirdRun)
    expect(testFunctionCount.get(testFn)).toEqual(1)
  })
})
