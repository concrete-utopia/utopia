let CachedPromises: { [key: string]: Promise<any> } = {}

export function cachedPromise<T>(promiseId: string, constructor: () => Promise<T>): Promise<T> {
  if (CachedPromises[promiseId] != null) {
    return CachedPromises[promiseId]
  } else {
    const newPromise = constructor()
    CachedPromises[promiseId] = newPromise
    return newPromise
  }
}
