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

export async function waitUntil(limitMS: number, check: () => boolean): Promise<boolean> {
  if (check()) {
    return true
  } else if (limitMS <= 0) {
    return false
  } else {
    return new Promise((resolve) => {
      setTimeout(resolve, 5)
    }).then(() => {
      return waitUntil(limitMS - 5, check)
    })
  }
}
