export type Compare<T> = (first: T, second: T) => number

export function comparePrimitive<T extends string | number | boolean>(first: T, second: T): number {
  if (first < second) {
    return -1
  } else if (first > second) {
    return 1
  } else {
    return 0
  }
}

export function compareNullable<T>(compareWith: Compare<T>): Compare<T | null> {
  // Assumes null is less than anything else.
  return (first: T | null, second: T | null) => {
    if (first === null) {
      if (second === null) {
        return 0
      } else {
        return -1
      }
    } else {
      if (second === null) {
        return 1
      } else {
        return compareWith(first, second)
      }
    }
  }
}

export function compareUndefined<T>(compareWith: Compare<T>): Compare<T | undefined> {
  // Assumes undefined is less than anything else.
  return (first: T | undefined, second: T | undefined) => {
    if (first === undefined) {
      if (second === undefined) {
        return 0
      } else {
        return -1
      }
    } else {
      if (second === undefined) {
        return 1
      } else {
        return compareWith(first, second)
      }
    }
  }
}

export function compareOn<T, U>(getPart: (value: T) => U, compareWith: Compare<U>): Compare<T> {
  return (first: T, second: T) => {
    return compareWith(getPart(first), getPart(second))
  }
}

export function compareField<T, K extends keyof T>(
  field: K,
  compareWith: Compare<T[K]>,
): Compare<T> {
  return compareOn((value: T) => value[field], compareWith)
}

export function compareArray<T>(compareWith: Compare<T>): Compare<Array<T>> {
  return (first: Array<T>, second: Array<T>) => {
    const lengthCompareResult = comparePrimitive(first.length, second.length)
    if (lengthCompareResult === 0) {
      for (let index = 0; index < first.length; index++) {
        // These two values must exist because of the bounds check and that the arrays are of equal length.
        const firstElem = first[index]!
        const secondElem = second[index]!
        const elemResult = compareWith(firstElem, secondElem)
        if (elemResult !== 0) {
          return elemResult
        }
      }
      return 0
    } else {
      return lengthCompareResult
    }
  }
}

export function compareCompose<T>(...comparisons: Array<Compare<T>>): Compare<T> {
  return (first: T, second: T) => {
    for (const comparison of comparisons) {
      const comparisonResult = comparison(first, second)
      if (comparisonResult !== 0) {
        return comparisonResult
      }
    }
    return 0
  }
}

export function compareIfIs<T, U extends T>(
  isCheck: (t: T) => t is U,
  compare: Compare<U>,
): Compare<T> {
  return (first: T, second: T) => {
    if (isCheck(first) && isCheck(second)) {
      return compare(first, second)
    } else {
      return 0
    }
  }
}
