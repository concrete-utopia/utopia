import { delay } from '../../utils/utils.test-utils'
import { TimedCacheMap } from './timed-cache-map'

describe('Adding values to a TimedCacheMap', () => {
  it('Retains all values if none have gone stale', async () => {
    const timeToClean = 20
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

    await delay(timeToClean + 10)

    const result = Array.from(cache.entries())
    expect(result).toEqual(input)
  })

  it('Removes all stale values', async () => {
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

    await delay(timeToClean + 10)

    // All still there
    const result1 = Array.from(cache.entries())
    expect(result1).toEqual(input)

    await delay(timeToClean + 10)

    // Ensure one specific value remains
    cache.get('b')

    await delay(timeToClean + 10)

    // All others have gone
    const result2 = Array.from(cache.entries())
    expect(result2).toEqual([['b', 'second']])
  })

  it('Calling cache.get() retains a value', async () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    cache.set('a', 'first')
    cache.set('b', 'second')

    await delay(timeToClean + 10)

    // Still there
    const result1 = cache.get('a')
    expect(result1).toEqual('first')

    await delay(timeToClean + 10)

    // Still there after the other value has gone stale
    const result2 = cache.get('a')
    expect(result2).toEqual('first')

    const expectedMissing = cache.get('b')
    expect(expectedMissing).toBeUndefined()
  })

  it('Calling cache.set() retains a value', async () => {
    const timeToClean = 20
    const timeToStale = timeToClean * 2
    let cache = new TimedCacheMap<string, string>(timeToStale, timeToClean)
    cache.set('a', 'first')
    cache.set('b', 'second')

    await delay(timeToClean + 10)

    // Update the value
    cache.set('a', 'third')

    await delay(timeToClean + 10)

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

  it('Calling cache.entries() behaves as expected', async () => {
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

    await delay(timeToStale + 10)

    const result2 = Array.from(cache.entries())
    expect(result2).toEqual([])
  })

  it('Calling cache.keys() behaves as expected', async () => {
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

    await delay(timeToStale + 10)

    const result2 = Array.from(cache.keys())
    expect(result2).toEqual([])
  })

  it('Calling cache.values() behaves as expected', async () => {
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

    await delay(timeToStale + 10)

    const result2 = Array.from(cache.values())
    expect(result2).toEqual([])
  })
})
