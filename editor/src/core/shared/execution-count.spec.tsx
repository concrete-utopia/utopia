import type { LimitExecutionCountReset } from './execution-count'
import { limitExecutionCount } from './execution-count'

describe('limitExecutionCount', () => {
  it('the wrapped function should retain its type', () => {
    function putNumberOnString(str: string, num: number): string {
      return `${str}-${num}`
    }
    const wrappedFunction = limitExecutionCount(
      { maximumExecutionCount: 1 },
      putNumberOnString,
    ).wrappedFunction
    expect(wrappedFunction('hello', 1)).toBe('hello-1')
  })
  it('should let a function be called the maximum number of times', () => {
    const fn = jest.fn()
    const { wrappedFunction } = limitExecutionCount({ maximumExecutionCount: 3 }, fn)
    wrappedFunction()
    wrappedFunction()
    wrappedFunction()
    expect(fn).toHaveBeenCalledTimes(3)
  })
  it('should let a function be called the maximum number of times, then reset and called the maximum number of times again', () => {
    const fn = jest.fn()
    const { wrappedFunction, resetCount } = limitExecutionCount({ maximumExecutionCount: 3 }, fn)
    wrappedFunction()
    wrappedFunction()
    wrappedFunction()
    expect(fn).toHaveBeenCalledTimes(3)
    resetCount()
    wrappedFunction()
    wrappedFunction()
    wrappedFunction()
    expect(fn).toHaveBeenCalledTimes(6)
  })
  it('should let a function be called the maximum number of times, then reset via the reset array and called the maximum number of times again', () => {
    const fn = jest.fn()
    const resetArray: Array<LimitExecutionCountReset> = []
    const { wrappedFunction } = limitExecutionCount(
      { maximumExecutionCount: 3, addToResetArray: resetArray },
      fn,
    )
    wrappedFunction()
    wrappedFunction()
    wrappedFunction()
    expect(fn).toHaveBeenCalledTimes(3)
    resetArray.forEach((reset) => reset())
    wrappedFunction()
    wrappedFunction()
    wrappedFunction()
    expect(fn).toHaveBeenCalledTimes(6)
  })
  it('should throw an exception if the function is called more than the maximum number of times', () => {
    const fn = jest.fn()
    const { wrappedFunction } = limitExecutionCount({ maximumExecutionCount: 2 }, fn)
    wrappedFunction()
    wrappedFunction()
    expect(() => {
      wrappedFunction()
    }).toThrowErrorMatchingInlineSnapshot(`"Function exceeded maximum execution count of 2."`)
  })
  it('should throw an exception if the function is called more than the maximum number of times, after a reset', () => {
    const fn = jest.fn()
    const { wrappedFunction, resetCount } = limitExecutionCount({ maximumExecutionCount: 2 }, fn)
    wrappedFunction()
    wrappedFunction()
    resetCount()
    wrappedFunction()
    wrappedFunction()
    expect(() => {
      wrappedFunction()
    }).toThrowErrorMatchingInlineSnapshot(`"Function exceeded maximum execution count of 2."`)
  })
  it('should throw an exception if the function is called more than the maximum number of times, after a reset via the reset array', () => {
    const fn = jest.fn()
    const resetArray: Array<LimitExecutionCountReset> = []
    const { wrappedFunction } = limitExecutionCount(
      { maximumExecutionCount: 2, addToResetArray: resetArray },
      fn,
    )
    wrappedFunction()
    wrappedFunction()
    resetArray.forEach((reset) => reset())
    wrappedFunction()
    wrappedFunction()
    expect(() => {
      wrappedFunction()
    }).toThrowErrorMatchingInlineSnapshot(`"Function exceeded maximum execution count of 2."`)
  })
})
