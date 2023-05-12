// Should allow us to guard against a type being refactored into something
// which Set allows but gives us nonsense results.
export function emptySet<T extends string | boolean | number | null | undefined>(): Set<T> {
  return new Set()
}

export function setsEqual<T>(first: Set<T>, second: Set<T>): boolean {
  if (first === second) {
    return true
  } else {
    if (first.size === second.size) {
      for (const firstValue of first) {
        if (!second.has(firstValue)) {
          return false
        }
      }
      return true
    } else {
      return false
    }
  }
}

export function intersection<T>(sets: Array<Set<T>>): Set<T> {
  const [first, ...rest] = sets
  if (first == null) {
    return new Set()
  }

  return rest.reduce((acc, v) => new Set([...acc].filter((x) => v.has(x))), first)
}
