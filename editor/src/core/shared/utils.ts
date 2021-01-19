import { MapLike } from 'typescript'
import { replaceAll } from './string-utils'
import urljoin = require('url-join')

// This file shouldn't import anything as it is for exporting simple shared utility functions between various projects
export const EditorID = 'editor'
export const PortalTargetID = 'portal-target'
export const CanvasContextMenuPortalTargetID = 'canvas-contextmenu-portal-target'

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

export function objectEquals<T>(
  a: MapLike<T>,
  b: MapLike<T>,
  eq?: (l: T, r: T) => boolean,
): boolean {
  if (a === b) {
    return true
  } else {
    const equals = eq == null ? (l: T, r: T) => l === r : eq
    for (const aKey of Object.keys(a)) {
      if (aKey in b) {
        if (!equals(a[aKey], b[aKey])) {
          // Values in each object for the same key differ.
          return false
        }
      } else {
        // Value for key cannot be found in object 'b'.
        return false
      }
    }
    for (const bKey of Object.keys(b)) {
      if (!(bKey in a)) {
        // This key from 'b' isn't in 'a'.
        return false
      }
    }
    return true
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

export const isBrowserEnvironment = process.env.JEST_WORKER_ID == undefined // If true this means we're not running in a Jest environment

export function urlSafeText(text: string): string {
  return encodeURIComponent(replaceAll(text.toLowerCase(), ' ', '-'))
}

function getProjectURLSuffix(projectId: string, projectName: string): string {
  return urlSafeText(`${projectId}-${projectName}`)
}

export function projectURLForProject(projectId: string, projectName: string): string {
  return `/p/${getProjectURLSuffix(projectId, projectName)}/`
}

export function shareURLForProject(prefix: string, projectId: string, projectName: string): string {
  return urljoin(prefix, `/share/${getProjectURLSuffix(projectId, projectName)}`)
}

export function unknownObjectProperty(o: unknown, key: string): any {
  if (typeof o === 'object' && o != null) {
    if (key in o) {
      // at this point we know `o` is an object and it has the prop, but Typescript threw an error
      return (o as any)[key]
    }
  }
  return undefined
}
