import { testPrintParsedTextFile } from '../../../components/canvas/ui-jsx.test-utils'
import { Utils } from '../../../uuiui-deps'
import { forEachValue } from '../../shared/object-utils'
import {
  clearParseResultUniqueIDs,
  parseThenPrint,
  testParseCode,
} from './parser-printer.test-utils'
import { applyPrettier } from './prettier-utils'
import * as R from 'ramda'

describe('parseCode', () => {
  it('should parse a component with comments in front of it', () => {
    const code = applyPrettier(
      `
    // Single-line comment.
    /*
      Multi
      Line
      Comment.
    */
    export var whatever = (props) => {
      return <div data-uid='aaa' />
    }
`,
      false,
    ).formatted
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(testPrintParsedTextFile(actualResult)).toEqual(code)
    const leadingComments = Utils.path(
      ['topLevelElements', 0, 'comments', 'leadingComments'],
      actualResult,
    )
    expect(leadingComments).toMatchInlineSnapshot(`
      Array [
        Object {
          "comment": " Single-line comment.",
          "pos": 0,
          "rawText": "// Single-line comment.",
          "trailingNewLine": true,
          "type": "SINGLE_LINE_COMMENT",
        },
        Object {
          "comment": "
            Multi
            Line
            Comment.
          ",
          "pos": 24,
          "rawText": "/*
            Multi
            Line
            Comment.
          */",
          "trailingNewLine": true,
          "type": "MULTI_LINE_COMMENT",
        },
      ]
    `)
  })
})

describe('Parsing and printing code with comments', () => {
  const comments = {
    commentBeforeImports: '// Comment before imports',
    commentInsideImports: '// Comment inside imports',
    commentAfterImports: '// Comment after imports',
    commentBeforeTopLevelJS: '// Comment before top level JS block',
    commentInsideTopLevelJS: '// Comment inside top level JS block',
    commentAfterTopLevelJS: '// Comment after top level JS block',
    inlineCommentBeforeFirstConst: '/* Inline comment before first const */',
    inlineCommentAfterFirstConst: '/* Inline comment after first const */',
    inlineCommentBeforeSecondConst: '/* Inline comment before second const */',
    inlineCommentAfterSecondConst: '/* Inline comment after second const */',
    commentBeforeComponent: '// Comment before component',
    commentInComponent: '// Comment in component',
    commentAfterComponent: '// Comment after component',
    commentBeforeInnerJS: '// Comment before inner JS',
    commentInsideInnerJS: '// Comment inside inner JS',
    commentAfterInnerJS: '// Comment after inner JS',
    commentBeforeReturnStatement: '// Comment before return statement',
    commentAfterReturnStatement: '// Comment after return statement',
    commentAtStartOfJSXAttribute: '/* Comment at start of JSX attribute */',
    commentAtEndOfJSXAttribute: '/* Comment at end of JSX attribute */',
    commentAtStartOfJSXExpression: '/* Comment at start of JSX expression */',
    commentInsideJSXExpression: '/* Comment inside JSX expression */',
    commentAtEndOfJSXExpression: '/* Comment at end of JSX expression */',
    commentBeforeExports: '// Comment before exports',
    commentInsideExports: '// Comment inside exports',
    commentAfterExports: '// Comment after exports',
    finalLineComment: '// Final line comment',
    commentBeforeObjectKey: '/* Comment before object key */',
    commentAfterObjectKey: '/* Comment after object key */',
    commentBeforeObjectValue: '/* Comment before object value */',
    commentAfterObjectValue: '/* Comment after object value */',
  }

  const notYetSupported: Array<keyof typeof comments> = [
    'commentBeforeObjectKey',
    'commentAfterObjectKey',
    'commentBeforeObjectValue',
    'commentAfterObjectValue',
    'finalLineComment',
  ]

  const code = `
    ${comments.commentBeforeImports}
    import * as React from 'react'
    ${comments.commentInsideImports}
    import { Cat } from './honestly-not-dogs' ${comments.commentAfterImports}

    ${comments.commentBeforeTopLevelJS}
    function iAmAFunction() {
      ${comments.commentInsideTopLevelJS}
      return 1
    } ${comments.commentAfterTopLevelJS}

    ${comments.inlineCommentBeforeFirstConst} const a = 10 ${comments.inlineCommentAfterFirstConst}
    ${comments.inlineCommentBeforeSecondConst} const b = 10 ${comments.inlineCommentAfterSecondConst}

    ${comments.commentBeforeComponent}
    var whatever = (props) => {
      ${comments.commentInComponent}

      ${comments.commentBeforeInnerJS}
      function iAmAnInnerFunction() {
        ${comments.commentInsideInnerJS}
        return 2
      } ${comments.commentAfterInnerJS}

      ${comments.commentBeforeReturnStatement}
      return (
        <div
          data-uid={'aaa'}
          someProp={${comments.commentAtStartOfJSXAttribute} 1000 ${comments.commentAtEndOfJSXAttribute}}
          someProp2={{
            ${comments.commentBeforeObjectKey} someKey ${comments.commentAfterObjectKey} : ${comments.commentBeforeObjectValue} 'someValue' ${comments.commentAfterObjectValue},
            someKey2: 'someValue2'
          }}
          >
          {
            ${comments.commentAtStartOfJSXExpression}
            true ${comments.commentInsideJSXExpression}
            ? <div/>
            : <div/>
            ${comments.commentAtEndOfJSXExpression}
          }
        </div>
      ) ${comments.commentAfterReturnStatement}
    } ${comments.commentAfterComponent}

    ${comments.commentBeforeExports}
    export const theFunction = iAmAFunction
    ${comments.commentInsideExports}
    export const theComponent = whatever ${comments.commentAfterExports}

    ${comments.finalLineComment}
  `

  const parsedThenPrinted = parseThenPrint(code)

  forEachValue((commentText, commentKey) => {
    const testFn = notYetSupported.includes(commentKey) ? xit : it
    testFn(`should retain the comment '${commentText}'`, () => {
      expect(parsedThenPrinted.includes(commentText)).toBeTruthy()
      const firstIndex = parsedThenPrinted.indexOf(commentText)
      const lastIndex = parsedThenPrinted.lastIndexOf(commentText)
      if (firstIndex !== lastIndex) {
        fail(`Found more than one instance of ${commentText}`)
      }
    })
  }, comments)
})
