import { TimedCacheMap } from './timed-cache-map'

jest.useFakeTimers()

describe('Adding values to a TimedCacheMap', () => {
  it('Retains all values if none have gone stale', () => {
    const timeToClean = 200
    const timeToStale = 60000
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    const input = [
      ['a', 'first'],
      ['b', 'second'],
      ['c', 'third'],
    ]
    input.forEach(([k, v]) => {
      cache.set(k, v)
    })

    jest.advanceTimersByTime(timeToClean + 10)

    const result = Array.from(cache.entries())
    expect(result).toEqual(input)
  })

  it('Removes all stale values', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    const input = [
      ['a', 'first'],
      ['b', 'second'],
      ['c', 'third'],
    ]
    input.forEach(([k, v]) => {
      cache.set(k, v)
    })

    jest.advanceTimersByTime(timeToClean + 10)

    // All still there
    const result1 = Array.from(cache.entries())
    expect(result1).toEqual(input)

    jest.advanceTimersByTime(timeToClean + 10)

    // Ensure one specific value remains
    cache.get('b')

    jest.advanceTimersByTime(timeToClean + 10)

    // All others have gone
    const result2 = Array.from(cache.entries())
    expect(result2).toEqual([['b', 'second']])
  })

  it('Calling cache.get() retains a value', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    cache.set('a', 'first')
    cache.set('b', 'second')

    jest.advanceTimersByTime(timeToClean + 10)

    // Still there
    const result1 = cache.get('a')
    expect(result1).toEqual('first')

    jest.advanceTimersByTime(timeToClean + 10)

    // Still there after the other value has gone stale
    const result2 = cache.get('a')
    expect(result2).toEqual('first')

    const expectedMissing = cache.get('b')
    expect(expectedMissing).toBeUndefined()
  })

  it('Calling cache.set() retains a value', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    cache.set('a', 'first')
    cache.set('b', 'second')

    jest.advanceTimersByTime(timeToClean + 10)

    // Update the value
    cache.set('a', 'third')

    jest.advanceTimersByTime(timeToClean + 10)

    // Still there after the other value has gone stale
    const result = cache.get('a')
    expect(result).toEqual('third')

    const expectedMissing = cache.get('b')
    expect(expectedMissing).toBeUndefined()
  })

  it('Calling cache.clear() behaves as expected', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    const input = [
      ['a', 'first'],
      ['b', 'second'],
      ['c', 'third'],
    ]
    input.forEach(([k, v]) => {
      cache.set(k, v)
    })

    const result1 = Array.from(cache.entries())
    expect(result1).toEqual(input)

    cache.clear()

    const result2 = Array.from(cache.entries())
    expect(result2).toEqual([])
  })

  it('Calling cache.entries() behaves as expected', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    const input = [
      ['a', 'first'],
      ['b', 'second'],
      ['c', 'third'],
    ]
    input.forEach(([k, v]) => {
      cache.set(k, v)
    })

    const result1 = Array.from(cache.entries())
    expect(result1).toEqual(input)

    jest.advanceTimersByTime(timeToStale + 10)

    const result2 = Array.from(cache.entries())
    expect(result2).toEqual([])
  })

  it('Calling cache.keys() behaves as expected', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    const input = [
      ['a', 'first'],
      ['b', 'second'],
      ['c', 'third'],
    ]
    input.forEach(([k, v]) => {
      cache.set(k, v)
    })

    const result1 = Array.from(cache.keys())
    expect(result1).toEqual(['a', 'b', 'c'])

    jest.advanceTimersByTime(timeToStale + 10)

    const result2 = Array.from(cache.keys())
    expect(result2).toEqual([])
  })

  it('Calling cache.values() behaves as expected', () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    const input = [
      ['a', 'first'],
      ['b', 'second'],
      ['c', 'third'],
    ]
    input.forEach(([k, v]) => {
      cache.set(k, v)
    })

    const result1 = Array.from(cache.values())
    expect(result1).toEqual(['first', 'second', 'third'])

    jest.advanceTimersByTime(timeToStale + 10)

    const result2 = Array.from(cache.values())
    expect(result2).toEqual([])
  })
})
