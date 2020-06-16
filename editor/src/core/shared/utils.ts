// This file shouldn't import anything as it is for exporting simple shared utility functions between various projects
export const EditorID = 'editor'
export const PortalTargetID = 'portal-target'

export const RETURN_TO_PREPEND = 'return '

// eslint-disable-next-line @typescript-eslint/no-empty-function
export const NO_OP = () => {}

export type ValueOf<T> = T[keyof T]

export type PrimitiveType = number | string | boolean | bigint | symbol | null | undefined

export function identity<T>(t: T): T {
  return t
}

export function fastForEach<T>(a: readonly T[], fn: (t: T, index: number) => void) {
  for (var i = 0, len = a.length; i < len; i++) {
    if (i in a) {
      fn(a[i]!, i)
    }
  }
}

export function arrayEquals<T>(a: Array<T>, b: Array<T>, eq?: (l: T, r: T) => boolean): boolean {
  if (a === b) {
    return true
  } else {
    const equals = eq == null ? (l: T, r: T) => l === r : eq
    if (a.length === b.length) {
      for (let i = 0; i < a.length; i++) {
        if (!equals(a[i], b[i])) {
          return false
        }
      }
      return true
    } else {
      return false
    }
  }
}

// Uses === as the semantics, which should be safe for the given types.
export function arrayContains<T extends string | boolean | number>(
  array: Array<T>,
  possibleEntry: T,
): boolean {
  return array.indexOf(possibleEntry) >= 0
}

export function longestCommonArray<T>(
  a: Array<T>,
  b: Array<T>,
  eq?: (l: T, r: T) => boolean,
): Array<T> {
  const equals = eq == null ? (l: T, r: T) => l === r : eq
  const commonMaxLength = Math.min(a.length, b.length)
  let commonArray: Array<T> = []
  for (let i = 0; i < commonMaxLength; i++) {
    if (equals(a[i], b[i])) {
      commonArray.push(a[i])
    } else {
      break
    }
  }
  return commonArray
}

export function getProjectLockedKey(projectId: string) {
  return `${projectId}-locked`
}

export function xor(a: boolean, b: boolean): boolean {
  return a != b
}
