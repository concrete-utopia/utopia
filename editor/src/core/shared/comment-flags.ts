import { mapDropNulls } from './array-utils'
import { Comment, ParsedComments, singleLineComment } from './element-template'

const UtopiaCommentFlagPrefix = '@utopia/'

export type UtopiaCommentFlagTypeConditional = 'conditional'

export type UtopiaCommentFlagConditional = {
  type: UtopiaCommentFlagTypeConditional
  value: boolean | null
}

export type UtopiaCommentFlagType = UtopiaCommentFlagTypeConditional

export type UtopiaCommentFlag = UtopiaCommentFlagConditional

export function isUtopiaCommentFlagConditional(
  flag: UtopiaCommentFlag | null,
): flag is UtopiaCommentFlagConditional {
  return flag?.type === 'conditional'
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
  return c.comment.trim().toLowerCase()
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
