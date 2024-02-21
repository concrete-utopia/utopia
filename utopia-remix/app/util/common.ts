export function asNumber(value?: unknown): number {
  if (!['string', 'number'].includes(typeof value) || value === '') {
    return NaN
  }
  return Number(value)
}
