export function asNumber(value?: unknown): number {
  try {
    return parseInt(value as string)
  } catch (error) {
    return NaN
  }
}

export function mapArrayToDictionary<From, Values, Keys extends string | number>(
  arr: ReadonlyArray<From>,
  keyFn: (t: From, index: number) => Keys,
  mapFn: (t: From, index: number) => Values,
): {
  [key in Keys]: Values
} {
  return arr.reduce(
    (working, next, index) => {
      const key = keyFn(next, index)
      working[key] = mapFn(next, index)
      return working
    },
    {} as {
      [key in Keys]: Values
    },
  )
}

export function urlToRelative(url: string): string {
  const parsed = new URL(url)
  return parsed.href.replace(parsed.origin, '')
}

// ported from editor/src/core/shared/utils.ts
export function fastForEach<T>(a: readonly T[], fn: (t: T, index: number) => void): void {
  for (var i = 0, len = a.length; i < len; i++) {
    if (i in a) {
      fn(a[i]!, i)
    }
  }
}

// ported from editor/src/core/shared/array-utils.ts
export function mapDropNulls<T, U>(
  fn: (t: T, i: number) => U | null | undefined,
  a: ReadonlyArray<T>,
): Array<U> {
  let result: Array<U> = []
  fastForEach(a, (t, i) => {
    const u = fn(t, i)
    if (u != null) {
      result.push(u)
    }
  })
  return result
}
