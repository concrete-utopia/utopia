export function defaultIfNull<T>(defaultValue: T, value: T | null | undefined): T {
  if (value == null) {
    return defaultValue
  } else {
    return value
  }
}

export function fastForEach<T>(a: readonly T[], fn: (t: T, index: number) => void) {
  for (var i = 0, len = a.length; i < len; i++) {
    if (i in a) {
      fn(a[i]!, i)
    }
  }
}
