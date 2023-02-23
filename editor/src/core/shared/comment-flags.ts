import { mapDropNulls } from './array-utils'
import { Comment, ParsedComments } from './element-template'

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

function utopiaFlagKey(type: UtopiaCommentFlagType): string {
  return `${UtopiaCommentFlagPrefix}${type}`
}

export function utopiaCommentFlag(flag: UtopiaCommentFlag): string {
  return `${utopiaFlagKey(flag.type)}=${flag.value}`
}

export function isUtopiaCommentFlag(c: Comment, type: UtopiaCommentFlagType): boolean {
  return commentString(c).startsWith(utopiaFlagKey(type) + '=')
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
  const prefix = utopiaFlagKey(type) + '='

  if (comment.startsWith(prefix)) {
    const value = comment
      .replace(new RegExp(`^${prefix}`), '')
      .trim()
      .toLowerCase()
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
