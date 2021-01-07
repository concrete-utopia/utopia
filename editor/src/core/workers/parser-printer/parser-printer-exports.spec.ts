import { clearParseResultUniqueIDs, testParseCode } from './parser-printer.test-utils'
import Utils from '../../../utils/utils'
import { applyPrettier } from './prettier-utils'
import { testPrintParsedTextFile } from '../../../components/canvas/ui-jsx.test-utils'

describe('parseCode', () => {
  it('should parse a directly exported component', () => {
    const code = applyPrettier(
      `
    export var whatever = (props) => {
      return <div data-uid='aaa' />
    }
`,
      false,
    ).formatted
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(testPrintParsedTextFile(actualResult)).toEqual(code)
    const exports = Utils.path(['exportsDetail'], actualResult)
    expect(exports).toMatchInlineSnapshot(`
      Object {
        "defaultExport": null,
        "namedExports": Object {
          "whatever": Object {
            "type": "EXPORT_DETAIL_MODIFIER",
          },
        },
      }
    `)
  })
  it('should parse a component exported handled with an external export clause', () => {
    const code = applyPrettier(
      `
    var whatever = (props) => {
      return <div data-uid='aaa' />
    }
    export { whatever }
`,
      false,
    ).formatted
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(testPrintParsedTextFile(actualResult)).toEqual(code)
    const exports = Utils.path(['exportsDetail'], actualResult)
    expect(exports).toMatchInlineSnapshot(`
      Object {
        "defaultExport": null,
        "namedExports": Object {
          "whatever": Object {
            "name": "whatever",
            "type": "EXPORT_DETAIL_NAMED",
          },
        },
      }
    `)
  })
  it('should parse a component exported handled with an external export clause, with a different name', () => {
    const code = applyPrettier(
      `
    var whatever = (props) => {
      return <div data-uid='aaa' />
    }
    export { whatever as otherThing }
`,
      false,
    ).formatted
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(testPrintParsedTextFile(actualResult)).toEqual(code)
    const exports = Utils.path(['exportsDetail'], actualResult)
    expect(exports).toMatchInlineSnapshot(`
      Object {
        "defaultExport": null,
        "namedExports": Object {
          "otherThing": Object {
            "name": "whatever",
            "type": "EXPORT_DETAIL_NAMED",
          },
        },
      }
    `)
  })
})
