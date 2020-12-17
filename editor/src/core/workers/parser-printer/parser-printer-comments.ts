import * as TS from 'typescript'
import { Comment, multiLineComment, singleLineComment } from '../../shared/element-template'
import { fastForEach } from '../../shared/utils'

export interface ParsedComments {
  leadingComments: Array<Comment>
  trailingComments: Array<Comment>
}

export const emptyComments: ParsedComments = {
  leadingComments: [],
  trailingComments: [],
}

export function parsedComments(
  leadingComments: Array<Comment>,
  trailingComments: Array<Comment>,
): ParsedComments {
  return {
    leadingComments: leadingComments,
    trailingComments: trailingComments,
  }
}

function parseComment(
  sourceText: string,
  pos: number,
  end: number,
  commentKind: TS.CommentKind,
  hasTrailingNewLine: boolean,
): Comment {
  switch (commentKind) {
    case TS.SyntaxKind.SingleLineCommentTrivia:
      return singleLineComment(sourceText.slice(pos + 2, end), hasTrailingNewLine)
    case TS.SyntaxKind.MultiLineCommentTrivia:
      return multiLineComment(sourceText.slice(pos + 2, end - 2), hasTrailingNewLine)
    default:
      const _exhaustiveCheck: never = commentKind
      throw new Error(`Unhandled comment kind ${commentKind}`)
  }
}

function getLeadingComments(sourceText: string, node: TS.Node): Array<Comment> {
  let result: Array<Comment> = []
  const parseAndPushComment = (
    pos: number,
    end: number,
    commentKind: TS.CommentKind,
    hasTrailingNewLine: boolean,
  ) => {
    result.push(parseComment(sourceText, pos, end, commentKind, hasTrailingNewLine))
  }

  TS.forEachLeadingCommentRange(sourceText, node.getFullStart(), parseAndPushComment)
  return result
}

function getTrailingComments(sourceText: string, node: TS.Node): Array<Comment> {
  let result: Array<Comment> = []
  const parseAndPushComment = (
    pos: number,
    end: number,
    commentKind: TS.CommentKind,
    hasTrailingNewLine: boolean,
  ) => {
    result.push(parseComment(sourceText, pos, end, commentKind, hasTrailingNewLine))
  }

  TS.forEachTrailingCommentRange(sourceText, node.getEnd(), parseAndPushComment)
  return result
}

export function getComments(sourceText: string, node: TS.Node): ParsedComments {
  const leadingComments = getLeadingComments(sourceText, node)
  const trailingComments = getTrailingComments(sourceText, node)

  return parsedComments(leadingComments, trailingComments)
}

function createTSComments(comments: Array<Comment>): Array<TS.SynthesizedComment> {
  let result: Array<TS.SynthesizedComment> = []
  fastForEach(comments, (comment) => {
    let commentKind: TS.CommentKind
    switch (comment.type) {
      case 'MULTI_LINE_COMMENT':
        commentKind = TS.SyntaxKind.MultiLineCommentTrivia
        break
      case 'SINGLE_LINE_COMMENT':
        commentKind = TS.SyntaxKind.SingleLineCommentTrivia
        break
      default:
        const _exhaustiveCheck: never = comment
        throw new Error(`Unhandled comment type ${comment}`)
    }
    const tsComment: TS.SynthesizedComment = {
      kind: commentKind,
      hasTrailingNewLine: comment.trailingNewLine,
      text: comment.comment,
      pos: -1,
      end: -1,
    }
    result.push(tsComment)
  })
  return result
}

export function addCommentsToNode(node: TS.Node, comments: ParsedComments): void {
  const leadingTSComments = createTSComments(comments.leadingComments)
  const trailingTSComments = createTSComments(comments.trailingComments)

  if (leadingTSComments.length > 0) {
    TS.setSyntheticLeadingComments(node, leadingTSComments)
  }
  if (trailingTSComments.length > 0) {
    TS.setSyntheticTrailingComments(node, trailingTSComments)
  }
}
