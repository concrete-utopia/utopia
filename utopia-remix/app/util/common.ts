export function asNumber(value?: unknown): number {
  try {
    return parseInt(value as string)
  } catch (error) {
    return NaN
  }
}
