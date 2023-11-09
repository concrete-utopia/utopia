import { mapDropNulls } from './array-utils'
import type { Comment, ParsedComments } from './element-template'
import { emptyComments, singleLineComment } from './element-template'
import { assertNever } from './utils'

const UtopiaCommentFlagPrefix = '@utopia/'

export type UtopiaCommentFlagTypeConditional = 'conditional'

export type UtopiaCommentFlagTypeMapCount = 'map-count'

export type UtopiaCommentFlagTypeUid = 'uid'

export type UtopiaCommentFlagTypeGroup = 'group'

export type UtopiaCommentFlagTypeComment = 'comment'

export type UtopiaCommentFlagConditional = {
  type: UtopiaCommentFlagTypeConditional
  value: boolean | null
}

export type UtopiaCommentFlagMapCount = {
  type: UtopiaCommentFlagTypeMapCount
  value: number | null
}

export type UtopiaCommentFlagUid = {
  type: UtopiaCommentFlagTypeUid
  value: string
}

export type UtopiaCommentFlagGroup = {
  type: UtopiaCommentFlagTypeGroup
  value: boolean | null
}

export type UtopiaCommentFlagComment = {
  type: UtopiaCommentFlagTypeComment
  value: string
}

export type UtopiaCommentFlagType =
  | UtopiaCommentFlagTypeConditional
  | UtopiaCommentFlagTypeMapCount
  | UtopiaCommentFlagTypeUid
  | UtopiaCommentFlagTypeGroup
  | UtopiaCommentFlagTypeComment

export type UtopiaCommentFlag =
  | UtopiaCommentFlagConditional
  | UtopiaCommentFlagMapCount
  | UtopiaCommentFlagUid
  | UtopiaCommentFlagGroup
  | UtopiaCommentFlagComment

export function isUtopiaCommentFlagConditional(
  flag: UtopiaCommentFlag | null,
): flag is UtopiaCommentFlagConditional {
  return flag?.type === 'conditional'
}

export function isUtopiaCommentFlagMapCount(
  flag: UtopiaCommentFlag | null,
): flag is UtopiaCommentFlagMapCount {
  return flag?.type === 'map-count'
}

export function isUtopiaCommentFlagUid(
  flag: UtopiaCommentFlag | null,
): flag is UtopiaCommentFlagUid {
  return flag?.type === 'uid'
}

export function isUtopiaCommentFlagGroup(
  flag: UtopiaCommentFlag | null,
): flag is UtopiaCommentFlagGroup {
  return flag?.type === 'group'
}

export function isUtopiaCommentFlagComment(
  flag: UtopiaCommentFlag | null,
): flag is UtopiaCommentFlagComment {
  return flag?.type === 'comment'
}

function utopiaCommentFlagKey(type: UtopiaCommentFlagType): string {
  return `${UtopiaCommentFlagPrefix}${type}`
}

export function makeUtopiaFlagComment(flag: UtopiaCommentFlag): Comment {
  const comment = ` ${utopiaCommentFlagKey(flag.type)}=${flag.value}`
  return singleLineComment(comment, comment, true, 0)
}

export function isUtopiaCommentFlag(c: Comment, type: UtopiaCommentFlagType): boolean {
  return commentString(c).startsWith(utopiaCommentFlagKey(type) + '=')
}

function commentString(c: Comment): string {
  return c.comment.trim()
}

function getUtopiaCommentFlag(c: Comment, type: UtopiaCommentFlagType): UtopiaCommentFlag | null {
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

  const comment = commentString(c)
  const prefix = utopiaCommentFlagKey(type) + '='

  if (comment.startsWith(prefix)) {
    const value = comment.slice(prefix.length)
    switch (type) {
      case 'conditional':
        return {
          type: 'conditional',
          value: parseBooleanOrNull(value),
        }
      case 'uid':
        return {
          type: 'uid',
          value,
        }
      case 'group':
        return {
          type: 'group',
          value: parseBooleanOrNull(value),
        }
      case 'map-count':
        return {
          type: 'map-count',
          value: parseNonNegativeIntOrNull(value),
        }
      case 'comment':
        return {
          type: 'comment',
          value: value,
        }
      default:
        assertNever(type)
    }
  }
  return null
}

export function findUtopiaCommentFlag(
  comments: ParsedComments,
  key: UtopiaCommentFlagType,
): UtopiaCommentFlag | null {
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
  key: UtopiaCommentFlagType,
): UtopiaCommentFlag | null {
  const commentConds = mapDropNulls((c) => getUtopiaCommentFlag(c, key), allComments(comments))
  return commentConds.length > 0 ? commentConds[0] : null
}
