import * as TS from 'typescript'
import { Comment, multiLineComment, singleLineComment } from '../../shared/element-template'
import { fastForEach } from '../../shared/utils'

export interface CommentsResult {
  leadingComments: Array<Comment>
}

export function getComments(sourceText: string, node: TS.Node): CommentsResult {
  let leadingComments: Array<Comment> = []
  TS.forEachLeadingCommentRange(
    sourceText,
    node.getFullStart(),
    (pos: number, end: number, commentKind: TS.CommentKind, hasTrailingNewLine: boolean) => {
      switch (commentKind) {
        case TS.SyntaxKind.SingleLineCommentTrivia:
          leadingComments.push(
            singleLineComment(sourceText.slice(pos + 2, end), hasTrailingNewLine),
          )
          break
        case TS.SyntaxKind.MultiLineCommentTrivia:
          leadingComments.push(
            multiLineComment(sourceText.slice(pos + 2, end - 2), hasTrailingNewLine),
          )
          break
        default:
          const _exhaustiveCheck: never = commentKind
          throw new Error(`Unhandled comment kind ${commentKind}`)
      }
    },
  )
  return {
    leadingComments: leadingComments,
  }
}

export function addCommentsToNode(node: TS.Node, leadingComments: Array<Comment>): void {
  let leadingTSComments: Array<TS.SynthesizedComment> = []
  fastForEach(leadingComments, (leadingComment) => {
    let commentKind: TS.CommentKind
    switch (leadingComment.type) {
      case 'MULTI_LINE_COMMENT':
        commentKind = TS.SyntaxKind.MultiLineCommentTrivia
        break
      case 'SINGLE_LINE_COMMENT':
        commentKind = TS.SyntaxKind.SingleLineCommentTrivia
        break
      default:
        const _exhaustiveCheck: never = leadingComment
        throw new Error(`Unhandled comment type ${leadingComment}`)
    }
    const tsComment: TS.SynthesizedComment = {
      kind: commentKind,
      hasTrailingNewLine: leadingComment.trailingNewLine,
      text: leadingComment.comment,
      pos: -1,
      end: -1,
    }
    leadingTSComments.push(tsComment)
  })
  if (leadingTSComments.length > 0) {
    TS.setSyntheticLeadingComments(node, leadingTSComments)
  }
}
