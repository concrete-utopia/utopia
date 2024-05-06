import type { MapLike } from 'typescript'
import { replaceAll } from './string-utils'
import urljoin from 'url-join'
import { appendHash } from './dom-utils'
import type { Either } from './either'
import { flatMapEither, left, mapEither, right } from './either'

// This file shouldn't import anything as it is for exporting simple shared utility functions between various projects
export const EditorID = 'utopia-editor-root'
export const PortalTargetID = 'portal-target'
export const CanvasContextMenuPortalTargetID = 'canvas-contextmenu-portal-target'
export const BodyMenuOpenClass = 'context-menu-open'

export const RETURN_TO_PREPEND = 'return '

// eslint-disable-next-line @typescript-eslint/no-empty-function
export function NO_OP() {}

export type ValueOf<T> = T[keyof T]

export type PrimitiveType = number | string | boolean | bigint | symbol | null | undefined

export function identity<T>(t: T): T {
  return t
}

export function fastForEach<T>(a: readonly T[], fn: (t: T, index: number) => void): void {
  for (var i = 0, len = a.length; i < len; i++) {
    if (i in a) {
      fn(a[i]!, i)
    }
  }
}

export function arrayEqualsByReference<T>(a: Array<T>, b: Array<T>): boolean {
  if (a === b) {
    return true
  } else {
    if (a.length === b.length) {
      for (let i = 0; i < a.length; i++) {
        if (a[i] !== b[i]) {
          return false
        }
      }
      return true
    } else {
      return false
    }
  }
}

export function arrayEqualsByValue<T>(
  a: Array<T>,
  b: Array<T>,
  equals: (l: T, r: T) => boolean,
): boolean {
  if (a === b) {
    return true
  } else {
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

export function createIframeUrl(base: string, assetName: string): string {
  return appendHash(urljoin(base, 'editor', assetName))
}

export function assertNever(n: never): never {
  throw new Error(`Expected \`never\`, got ${JSON.stringify(n)}`)
}

export function projectIdFromURL(projectURL: string): Either<string, string> {
  try {
    const url = new URL(projectURL)
    const projectIDMatch = url.pathname.match(/^\/(p|project)\/([A-Za-z0-9]+)/)
    if (projectIDMatch == null) {
      return left(`URL does not appear to have the project ID or be for a project.`)
    } else {
      return right(projectIDMatch[2])
    }
  } catch (error) {
    return left(`Invalid value passed that isn't a URL.`)
  }
}

export interface ContentsAndProjectRootResult {
  contentsURL: string
  projectRootURL: string
}

export function contentsJSONURLFromProjectURL(
  projectURL: string,
): Either<string, ContentsAndProjectRootResult> {
  try {
    return mapEither((projectID) => {
      const contentsURL = new URL(projectURL)
      contentsURL.pathname = `/v1/project/${projectID}/contents.json`
      const projectRootURL = new URL(projectURL)
      projectRootURL.pathname = `/project/${projectID}`
      return {
        contentsURL: contentsURL.toString(),
        projectRootURL: projectRootURL.toString(),
      }
    }, projectIdFromURL(projectURL))
  } catch (error) {
    return left(`Invalid value passed that isn't a URL.`)
  }
}

const imgPattern = /\.(jpe?g|png|gif|bmp|svg)(\?.*)?$/i

export function isImage(str: string): boolean {
  const lowerCaseStr = str.toLowerCase()
  return imgPattern.test(lowerCaseStr) || lowerCaseStr.startsWith('data:image/')
}
