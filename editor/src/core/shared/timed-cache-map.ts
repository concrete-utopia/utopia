import { fastForEach } from './utils'

export class TimedCacheMap<K, V> {
  private innerMap: Map<K, V>
  private lastAccessTimes: Map<K, number>
  private timeSinceLastClean: number

  constructor(
    private timeToStaleInMs: number = 60000,
    private timeToCleanInMs: number = timeToStaleInMs,
  ) {
    this.innerMap = new Map<K, V>()
    this.lastAccessTimes = new Map<K, number>()
    this.timeSinceLastClean = Date.now()
  }

  clear() {
    this.innerMap.clear()
    this.lastAccessTimes.clear()
    this.timeSinceLastClean = Date.now()
  }

  delete(k: K) {
    this.innerMap.delete(k)
    this.lastAccessTimes.delete(k)
  }

  has(k: K): boolean {
    return this.innerMap.has(k)
  }

  keys(): IterableIterator<K> {
    // TODO Should this update lastAccessTimes?
    this.optionallyClean()

    return this.innerMap.keys()
  }

  values(): IterableIterator<V> {
    // TODO Should this update lastAccessTimes?
    this.optionallyClean()

    return this.innerMap.values()
  }

  entries(): IterableIterator<[K, V]> {
    // TODO Should this update lastAccessTimes?
    this.optionallyClean()

    return this.innerMap.entries()
  }

  forEach(callback: (value: V, key: K) => void) {
    // TODO Should this update lastAccessTimes?
    this.optionallyClean()

    this.innerMap.forEach(callback)
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
    if (Date.now() - this.timeSinceLastClean > this.timeToCleanInMs) {
      this.timeSinceLastClean = Date.now()
      const staleKeys = this.getStaleKeys()
      fastForEach(staleKeys, (k) => {
        this.innerMap.delete(k)
        this.lastAccessTimes.delete(k)
      })
    }
  }
}
