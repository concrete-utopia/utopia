import { mapDropNulls } from './array-utils'
import type {
  Comment,
  JSXAttributesEntry,
  JSXAttributesPart,
  JSXElementChild,
  ParsedComments,
} from './element-template'
import {
  emptyComments,
  isJSXAttributeValue,
  isJSXAttributesEntry,
  isJSXElement,
  isWithComments,
  jsExpressionValue,
  jsxAttributesEntry,
  singleLineComment,
} from './element-template'
import { assertNever } from './utils'

const UtopiaCommentFlagPrefix = '@utopia/'

const UtopiaPropFlagPrefix = 'data-'

export type UtopiaPropOrCommentFlagTypeConditional = 'conditional'

export type UtopiaPropOrCommentFlagTypeMapCount = 'map-count'

export type UtopiaPropOrCommentFlagTypeUid = 'uid'

export type UtopiaPropOrCommentFlagTypeGroup = 'group'

export type UtopiaPropOrCommentFlagTypeCanCondense = 'can-condense'

export type UtopiaPropFlagType = `data-${UtopiaPropOrCommentFlagType}`

interface UtopiaPropFlag extends JSXAttributesEntry {
  key: UtopiaPropFlagType
}

function createUtopiaPropFlagKey(type: UtopiaPropOrCommentFlagType): UtopiaPropFlagType {
  return `${UtopiaPropFlagPrefix}${type}`
}

export type UtopiaPropOrCommentFlagConditional = {
  type: UtopiaPropOrCommentFlagTypeConditional
  value: boolean | null
}

export type UtopiaPropOrCommentFlagMapCount = {
  type: UtopiaPropOrCommentFlagTypeMapCount
  value: number | null
}

export type UtopiaPropOrCommentFlagUid = {
  type: UtopiaPropOrCommentFlagTypeUid
  value: string
}

export type UtopiaPropOrCommentFlagGroup = {
  type: UtopiaPropOrCommentFlagTypeGroup
  value: boolean | null
}

export type UtopiaPropOrCommentFlagCanCondense = {
  type: UtopiaPropOrCommentFlagTypeCanCondense
  value: boolean | null
}

export type UtopiaPropOrCommentFlagType =
  | UtopiaPropOrCommentFlagTypeConditional
  | UtopiaPropOrCommentFlagTypeMapCount
  | UtopiaPropOrCommentFlagTypeUid
  | UtopiaPropOrCommentFlagTypeGroup
  | UtopiaPropOrCommentFlagTypeCanCondense

export type UtopiaPropOrCommentFlag =
  | UtopiaPropOrCommentFlagConditional
  | UtopiaPropOrCommentFlagMapCount
  | UtopiaPropOrCommentFlagUid
  | UtopiaPropOrCommentFlagGroup
  | UtopiaPropOrCommentFlagCanCondense

export function isUtopiaPropOrCommentFlagConditional(
  flag: UtopiaPropOrCommentFlag | null,
): flag is UtopiaPropOrCommentFlagConditional {
  return flag?.type === 'conditional'
}

export function isUtopiaPropOrCommentFlagMapCount(
  flag: UtopiaPropOrCommentFlag | null,
): flag is UtopiaPropOrCommentFlagMapCount {
  return flag?.type === 'map-count'
}

export function isUtopiaPropOrCommentFlagUid(
  flag: UtopiaPropOrCommentFlag | null,
): flag is UtopiaPropOrCommentFlagUid {
  return flag?.type === 'uid'
}

export function isUtopiaPropOrCommentFlagGroup(
  flag: UtopiaPropOrCommentFlag | null,
): flag is UtopiaPropOrCommentFlagGroup {
  return flag?.type === 'group'
}

function utopiaCommentFlagKey(type: UtopiaPropOrCommentFlagType): string {
  return `${UtopiaCommentFlagPrefix}${type}`
}

export function makeUtopiaFlagComment(flag: UtopiaPropOrCommentFlag): Comment {
  const comment = ` ${utopiaCommentFlagKey(flag.type)}=${flag.value}`
  return singleLineComment(comment, comment, true, 0)
}

function makeUtopiaFlagProp(flag: UtopiaPropOrCommentFlag): UtopiaPropFlag {
  return jsxAttributesEntry(
    createUtopiaPropFlagKey(flag.type),
    jsExpressionValue(flag.value, emptyComments),
    emptyComments,
  ) as UtopiaPropFlag
}

export function isUtopiaPropOrCommentFlag(c: Comment, type: UtopiaPropOrCommentFlagType): boolean {
  return commentString(c).startsWith(utopiaCommentFlagKey(type) + '=')
}

export function isUtopiaPropFlag(
  prop: JSXAttributesPart,
  type: UtopiaPropOrCommentFlagType,
): prop is UtopiaPropFlag {
  return isJSXAttributesEntry(prop) && prop.key === `${UtopiaPropFlagPrefix}${type}`
}

function commentString(c: Comment): string {
  return c.comment.trim().toLowerCase()
}

function createCommentFlagFromValue(
  type: UtopiaPropOrCommentFlagType,
  stringValue: string,
): UtopiaPropOrCommentFlag | null {
  function parseBooleanOrNull(value: string): boolean | null {
    if (value === 'true') {
      return true
    }
    if (value === 'false') {
      return false
    }
    return null
  }

  function parseNonNegativeIntOrNull(value: string): number | null {
    const intValue = parseInt(value)
    if (isFinite(intValue) && intValue >= 0) {
      return intValue
    }
    return null
  }
  switch (type) {
    case 'conditional':
      return {
        type: 'conditional',
        value: parseBooleanOrNull(stringValue),
      }
    case 'uid':
      return {
        type: 'uid',
        value: stringValue,
      }
    case 'group':
      return {
        type: 'group',
        value: parseBooleanOrNull(stringValue),
      }
    case 'map-count':
      return {
        type: 'map-count',
        value: parseNonNegativeIntOrNull(stringValue),
      }
    case 'can-condense':
      return {
        type: 'can-condense',
        value: parseBooleanOrNull(stringValue),
      }
    default:
      assertNever(type)
  }
  return null
}

function getUtopiaCommentFlag(
  c: Comment,
  type: UtopiaPropOrCommentFlagType,
): UtopiaPropOrCommentFlag | null {
  const comment = commentString(c)
  const prefix = utopiaCommentFlagKey(type) + '='

  if (comment.startsWith(prefix)) {
    const value = comment.slice(prefix.length)
    return createCommentFlagFromValue(type, value)
  }
  return null
}

export function findUtopiaCommentFlag(
  comments: ParsedComments,
  key: UtopiaPropOrCommentFlagType,
): UtopiaPropOrCommentFlag | null {
  const commentConds = mapDropNulls(
    (c) => getUtopiaCommentFlag(c, key),
    [...comments.leadingComments, ...comments.trailingComments],
  )
  return commentConds.length > 0 ? commentConds[0] : null
}

export function allComments(comments: ParsedComments | null): Comment[] {
  if (comments == null) {
    return []
  }
  return [
    ...comments.leadingComments,
    ...comments.trailingComments,
    ...allComments(comments.questionTokenComments ?? null),
  ]
}

export function mergeComments(comments: ParsedComments[]): ParsedComments {
  if (comments.length === 0) {
    return emptyComments
  }
  const leadingComments = comments.flatMap((c) => c.leadingComments)
  const trailingComments = comments.flatMap((c) => c.trailingComments)
  const questionTokenComments = mergeComments(
    comments.flatMap((c) => c.questionTokenComments ?? []),
  )
  return { leadingComments, trailingComments, questionTokenComments }
}

export function deepFindUtopiaCommentFlag(
  comments: ParsedComments | null,
  key: UtopiaPropOrCommentFlagType,
): UtopiaPropOrCommentFlag | null {
  const commentConds = mapDropNulls((c) => getUtopiaCommentFlag(c, key), allComments(comments))
  return commentConds.length > 0 ? commentConds[0] : null
}

export function getFromPropOrFlagComment(
  element: JSXElementChild,
  key: UtopiaPropOrCommentFlagType,
): UtopiaPropOrCommentFlag | null {
  if (isJSXElement(element)) {
    const prop = element.props.find((p): p is UtopiaPropFlag => isUtopiaPropFlag(p, key))
    if (prop != null && isJSXAttributeValue(prop.value)) {
      return {
        type: key,
        value: prop.value.value,
      }
    }
  }
  if (isWithComments(element)) {
    return deepFindUtopiaCommentFlag(element.comments, key)
  }
  return null
}

export function saveToPropOrFlagComment<T extends JSXElementChild>(
  element: T,
  flag: UtopiaPropOrCommentFlag,
): T | null {
  if (isJSXElement(element)) {
    const newProps = element.props.filter((p) => !isUtopiaPropFlag(p, flag.type))
    const newProp = makeUtopiaFlagProp(flag)
    newProps.push(newProp)
    return {
      ...element,
      props: newProps,
    }
  }
  if (isWithComments(element)) {
    const newLeadingComments = element.comments.leadingComments.filter(
      (c) => !isUtopiaPropOrCommentFlag(c, flag.type),
    )

    newLeadingComments.push(makeUtopiaFlagComment(flag))

    return {
      ...element,
      comments: { ...element.comments, leadingComments: newLeadingComments },
    }
  }
  // we don't return the element if we could not annotate it
  return null
}

export function removePropOrFlagComment<T extends JSXElementChild>(
  element: T,
  type: UtopiaPropOrCommentFlagType,
): T {
  if (isJSXElement(element)) {
    const newProps = element.props.filter((p) => !isUtopiaPropFlag(p, type))

    return {
      ...element,
      props: newProps,
    }
  }
  if (isWithComments(element)) {
    const newLeadingComments = element.comments.leadingComments.filter(
      (c) => !isUtopiaPropOrCommentFlag(c, type),
    )

    return {
      ...element,
      comments: { ...element.comments, leadingComments: newLeadingComments },
    }
  }
  return element
}
