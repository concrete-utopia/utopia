import { fastForEach } from './utils'

export class TimedCacheMap<K, V> {
  private innerMap: Map<K, V>
  private lastAccessTimes: Map<K, number>
  private timeOfLastClean: number

  constructor(
    private timeToStaleInMs: number = 60000,
    private timeToCleanInMs: number = timeToStaleInMs,
  ) {
    this.innerMap = new Map<K, V>()
    this.lastAccessTimes = new Map<K, number>()
    this.timeOfLastClean = Date.now()
  }

  clear() {
    this.innerMap.clear()
    this.lastAccessTimes.clear()
    this.timeOfLastClean = Date.now()
  }

  delete(k: K) {
    this.innerMap.delete(k)
    this.lastAccessTimes.delete(k)
  }

  has(k: K): boolean {
    return this.innerMap.has(k)
  }

  private updateLastAccessTimeForAllKeys() {
    const now = Date.now()
    for (const key of this.lastAccessTimes.keys()) {
      this.lastAccessTimes.set(key, now)
    }
  }

  keys(): IterableIterator<K> {
    this.optionallyClean()

    this.updateLastAccessTimeForAllKeys()
    return this.innerMap.keys()
  }

  values(): IterableIterator<V> {
    this.optionallyClean()

    this.updateLastAccessTimeForAllKeys()
    return this.innerMap.values()
  }

  entries(): IterableIterator<[K, V]> {
    this.optionallyClean()

    this.updateLastAccessTimeForAllKeys()
    return this.innerMap.entries()
  }

  forEach(callback: (value: V, key: K) => void) {
    this.optionallyClean()

    const now = Date.now()
    this.innerMap.forEach((v, k) => {
      this.lastAccessTimes.set(k, now)
      callback(v, k)
    })
  }

  get(k: K): V | undefined {
    this.optionallyClean()

    const v = this.innerMap.get(k)
    if (v != null) {
      this.lastAccessTimes.set(k, Date.now())
    }
    return v
  }

  set(k: K, v: V) {
    this.optionallyClean()

    this.innerMap.set(k, v)
    this.lastAccessTimes.set(k, Date.now())
  }

  private getStaleKeys(): Array<K> {
    const now = Date.now()

    let staleKeys: Array<K> = []
    this.lastAccessTimes.forEach((lastAccessed, k) => {
      if (now - lastAccessed > this.timeToStaleInMs) {
        staleKeys.push(k)
      }
    })

    return staleKeys
  }

  private optionallyClean() {
    const now = Date.now()
    if (now - this.timeOfLastClean > this.timeToCleanInMs) {
      this.timeOfLastClean = now
      const staleKeys = this.getStaleKeys()
      fastForEach(staleKeys, (k) => {
        this.innerMap.delete(k)
        this.lastAccessTimes.delete(k)
      })
    }
  }
}
