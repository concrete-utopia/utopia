export function asNumber(value?: unknown): number {
  try {
    return parseInt(value as string)
  } catch (error) {
    return NaN
  }
}

export function asString(value?: unknown): string | null {
  try {
    return String(value)
  } catch (error) {
    return null
  }
}
