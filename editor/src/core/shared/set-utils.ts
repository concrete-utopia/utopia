// Should allow us to guard against a type being refactored into something
// which Set allows but gives us nonsense results.
export function emptySet<T extends string | boolean | number | null | undefined>(): Set<T> {
  return new Set()
}
