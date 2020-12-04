import { testPrintParsedTextFile } from '../../../components/canvas/ui-jsx.test-utils'
import { Utils } from '../../../uuiui-deps'
import { clearParseResultUniqueIDs, testParseCode } from './parser-printer.test-utils'
import { applyPrettier } from './prettier-utils'

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
      return <div data-uid={'aaa'} />
    }
`,
      false,
    ).formatted
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(testPrintParsedTextFile(actualResult)).toEqual(code)
    const leadingComments = Utils.path(['topLevelElements', 0, 'leadingComments'], actualResult)
    expect(leadingComments).toMatchInlineSnapshot(`
      Array [
        Object {
          "comment": " Single-line comment.",
          "trailingNewLine": true,
          "type": "SINGLE_LINE_COMMENT",
        },
        Object {
          "comment": "
            Multi
            Line
            Comment.
          ",
          "trailingNewLine": true,
          "type": "MULTI_LINE_COMMENT",
        },
      ]
    `)
  })
})
