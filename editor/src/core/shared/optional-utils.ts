export function defaultIfNull<T>(defaultValue: T, value: T | null | undefined): T {
  if (value == null) {
    return defaultValue
  } else {
    return value
  }
}

export function defaultIfNullLazy<T>(value: T | null | undefined, defaultValueGetter: () => T): T {
  if (value == null) {
    return defaultValueGetter()
  } else {
    return value
  }
}

export function forceNotNull<T>(description: string, value: T | null | undefined): T {
  if (value == null) {
    throw new Error(`Found to be null when it shouldn't be: ${description}`)
  } else {
    return value
  }
}

// FIXME This shouldn't be converting null into undefined
export function optionalMap<T, U>(fn: (param: T) => U, t: T | null | undefined): U | null {
  if (t == null) {
    return null
  } else {
    return fn(t)
  }
}

export function forEachOptional<T>(fn: (param: T) => void, t: T | null | undefined): void {
  if (t != null) {
    fn(t)
  }
}

export function optionalFlatMap<T, U>(
  fn: (param: T) => U | null | undefined,
  t: T | null | undefined,
): U | null {
  if (t == null) {
    return null
  } else {
    return defaultIfNull(null, fn(t))
  }
}

export function maybeToArray<T>(value: T | null | undefined): Array<T> {
  if (value == null) {
    return []
  } else {
    return [value]
  }
}

export function arrayToMaybe<T>(array: Array<T>): T | null {
  if (array.length > 0) {
    return array[0]
  } else {
    return null
  }
}

export function isNotNull<T>(value: T | null | undefined): value is T {
  return value != null
}
