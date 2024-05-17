import type {
  JSExpressionOtherJavaScript,
  JSXElement,
  UtopiaJSXComponent,
} from '../../shared/element-template'
import { forEachValue } from '../../shared/object-utils'
import type { ParseSuccess } from '../../shared/project-file-types'
import { parseThenPrint, testParseModifyPrint } from './parser-printer.test-utils'
import { applyPrettier } from 'utopia-vscode-common'

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
    commentInsideAnEmptyJSXExpression: '/* Comment inside an empty JSX expression */',
    commentBeforeExports: '// Comment before exports',
    commentInsideExports: '// Comment inside exports',
    commentAfterExports: '// Comment after exports',
    finalLineComment: '// Final line comment',
    commentBeforeObjectKey: '/* Comment before object key */',
    commentAfterObjectKey: '/* Comment after object key */',
    commentBeforeObjectValue: '/* Comment before object value */',
    commentAfterObjectValue: '/* Comment after object value */',
    commentAfterObjectSeparator: '/* Comment after object separator */',
    commentBeforeArrayValue: '/* Comment before array value */',
    commentAfterArrayValue: '/* Comment after array value */',
    commentAfterArraySeparator: '/* Comment after array separator */',
    commentBeforeAllAttributes: '/* Comment before all attributes */',
    commentInsideAttributes: '/* Comment inside attributes */',
    commentAfterAllAttributes: '/* Comment after all attributes */',
  }

  const notYetSupported: Array<string> = []

  const code = applyPrettier(
    `
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
          ${comments.commentBeforeAllAttributes}
          data-uid={'aaa'}
          ${comments.commentInsideAttributes}
          someProp={${comments.commentAtStartOfJSXAttribute} 1000 ${comments.commentAtEndOfJSXAttribute}}
          someProp2={{
            ${comments.commentBeforeObjectKey} someKey ${comments.commentAfterObjectKey} : ${comments.commentBeforeObjectValue} 'someValue' ${comments.commentAfterObjectValue}, ${comments.commentAfterObjectSeparator}
            someKey2: 'someValue2'
          }}
          someProp3={[
            ${comments.commentBeforeArrayValue} 100 ${comments.commentAfterArrayValue} , ${comments.commentAfterArraySeparator}
            200
          ]}
          ${comments.commentAfterAllAttributes}
          >
          {
            ${comments.commentAtStartOfJSXExpression}
            [].length === 0 ${comments.commentInsideJSXExpression}
            ? <div/>
            : <div/>
            ${comments.commentAtEndOfJSXExpression}
          }
          {${comments.commentInsideAnEmptyJSXExpression}}
        </div>
      ) ${comments.commentAfterReturnStatement}
    } ${comments.commentAfterComponent}

    ${comments.commentBeforeExports}
    export const theFunction = iAmAFunction
    ${comments.commentInsideExports}
    export const theComponent = whatever ${comments.commentAfterExports}

    ${comments.finalLineComment}
  `,
    false,
  ).formatted

  const parsedThenPrinted = parseThenPrint('/index.js', code)

  forEachValue((commentText, commentKey) => {
    const testFn = notYetSupported.includes(commentKey) ? xit : it
    testFn(`should retain the comment '${commentText}'`, () => {
      // eslint-disable-next-line jest/no-standalone-expect
      expect(parsedThenPrinted.includes(commentText)).toBeTruthy()
      const firstIndex = parsedThenPrinted.indexOf(commentText)
      const lastIndex = parsedThenPrinted.lastIndexOf(commentText)
      if (firstIndex !== lastIndex) {
        throw new Error(`Found more than one instance of ${commentText}`)
      }
    })
  }, comments)

  it('comments are in the expected places', () => {
    expect(parsedThenPrinted).toMatchInlineSnapshot(`
      "// Comment before imports
      import * as React from 'react'
      // Comment inside imports
      import { Cat } from './honestly-not-dogs' // Comment after imports

      // Comment before top level JS block
      function iAmAFunction() {
        // Comment inside top level JS block
        return 1
      } // Comment after top level JS block

      /* Inline comment before first const */ const a = 10 /* Inline comment after first const */
      /* Inline comment before second const */ const b = 10 /* Inline comment after second const */

      // Comment before component
      var whatever = (props) => {
        // Comment in component

        // Comment before inner JS
        function iAmAnInnerFunction() {
          // Comment inside inner JS
          return 2
        } // Comment after inner JS

        // Comment before return statement
        return (
          <div /* Comment before all attributes */
            data-uid='aaa' /* Comment inside attributes */
            someProp={
              /* Comment at start of JSX attribute */ 1000 /* Comment at end of JSX attribute */
            }
            someProp2={{
              /* Comment before object key */ someKey /* Comment after object key */:
                /* Comment before object value */ 'someValue' /* Comment after object value */ /* Comment after object separator */,
              someKey2: 'someValue2',
            }}
            someProp3={[
              /* Comment before array value */ 100 /* Comment after array value */ /* Comment after array separator */,
              200,
            ]} /* Comment after all attributes */
          >
            {
              /* Comment at start of JSX expression */
              [].length ===
              0 /* Comment inside JSX expression */ ? (
                <div data-uid='8eb' />
              ) : (
                <div data-uid='a87' />
              ) /* Comment at end of JSX expression */
            }
            {/* Comment inside an empty JSX expression */}
          </div>
        ) // Comment after return statement
      } // Comment after component

      // Comment before exports
      export const theFunction = iAmAFunction
      // Comment inside exports
      export const theComponent = whatever // Comment after exports

      // Final line comment
      "
    `)
  })

  it('Correctly supports actually parsing and updating comments in JS Expressions', () => {
    const startingCode = applyPrettier(
      `const App = () => <div>{/* ingredients */}</div>`,
      false,
    ).formatted
    testParseModifyPrint(
      'test.jsx',
      applyPrettier(`const App = () => <div>{/* ingredients */}</div>`, false).formatted,
      applyPrettier(`const App = () => <div>{/* cake */}</div>`, false).formatted,
      (parsed) => {
        const component = (parsed as ParseSuccess).topLevelElements[0]
        const rootElement = (component as UtopiaJSXComponent).rootElement
        const jsxBlock = (rootElement as JSXElement).children[0] as JSExpressionOtherJavaScript

        // Check the comment was parsed
        expect(jsxBlock.comments.leadingComments[0].comment).toEqual(' ingredients ')

        // Replace it
        jsxBlock.comments.leadingComments[0].comment = ' cake '

        return parsed
      },
      true,
    )
  })
})
