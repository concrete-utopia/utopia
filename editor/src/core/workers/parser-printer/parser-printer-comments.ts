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

export function mergeParsedComments(first: ParsedComments, second: ParsedComments): ParsedComments {
  return parsedComments(
    first.leadingComments.concat(second.leadingComments),
    first.trailingComments.concat(second.trailingComments),
  )
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
      return singleLineComment(
        sourceText.slice(pos + 2, end),
        sourceText.slice(pos, end),
        hasTrailingNewLine,
        pos,
      )
    case TS.SyntaxKind.MultiLineCommentTrivia:
      return multiLineComment(
        sourceText.slice(pos + 2, end - 2),
        sourceText.slice(pos, end),
        hasTrailingNewLine,
        pos,
      )
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

  TS.forEachLeadingCommentRange(sourceText, node.pos, parseAndPushComment)
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

  TS.forEachTrailingCommentRange(sourceText, node.end, parseAndPushComment)
  return result
}

export function getComments(sourceText: string, node: TS.Node): ParsedComments {
  // const leadingComments = getLeadingComments(sourceText, node)
  // const trailingComments = getTrailingComments(sourceText, node)

  // return parsedComments(leadingComments, trailingComments)
  return emptyComments
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

// Warning: Mutates the node, but also returns it.
export function addCommentsToNode(node: TS.Node, comments: ParsedComments): TS.Node {
  // const leadingTSComments = createTSComments(comments.leadingComments)
  // const trailingTSComments = createTSComments(comments.trailingComments)

  // if (leadingTSComments.length > 0) {
  //   TS.setSyntheticLeadingComments(node, leadingTSComments)
  // }
  // if (trailingTSComments.length > 0) {
  //   TS.setSyntheticTrailingComments(node, trailingTSComments)
  // }

  return node
}

// Comments just inside the opening brace of a JSX expression are treated as trailing comments
// against the open brace token, because of some reason that I'm unable to fathom.
export function getJSXExpressionLeadingComments(
  sourceText: string,
  sourceFile: TS.SourceFile,
  expression: TS.JsxExpression,
): Array<Comment> {
  const expressionFirstChild = expression.getChildAt(0, sourceFile)
  if (expressionFirstChild != null && expressionFirstChild.kind === TS.SyntaxKind.OpenBraceToken) {
    return getTrailingComments(sourceText, expressionFirstChild)
  } else {
    return []
  }
}
