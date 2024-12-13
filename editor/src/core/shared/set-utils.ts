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

// Values that are in the first set but not in the second set.
export function difference<T>(firstSet: Set<T>, secondSet: Set<T>): Set<T> {
  let result = new Set(firstSet)
  secondSet.forEach((value) => result.delete(value))
  return result
}

export function getSingleValueOnly<T>(set: Set<T>): T {
  if (set.size === 1) {
    for (const value of set) {
      return value
    }
  }
  throw new Error(`Set had ${set.size} when it was expected to have just 1.`)
}

export function addAll<T>(set: Set<T>, values: Array<T>): void {
  for (const value of values) {
    set.add(value)
  }
}
