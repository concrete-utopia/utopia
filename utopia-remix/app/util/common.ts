export function asNumber(value?: unknown): number {
  try {
    return parseInt(value as string)
  } catch (error) {
    return NaN
  }
}

export function mapValues<U, V>(
  transform: (u: U, k: string) => V,
  map: {
    [key: string]: U
  },
): {
  [key: string]: V
} {
  var result: {
    [key: string]: V
  } = {}
  Object.keys(map).forEach((key) => {
    result[key] = transform(map[key], key)
  })
  return result
}
