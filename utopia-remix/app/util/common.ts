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
