import { clearParseResultUniqueIDsAndEmptyBlocks, testParseCode } from './parser-printer.test-utils'
import Utils from '../../../utils/utils'
import { applyPrettier } from 'utopia-vscode-common'
import { testPrintParsedTextFile } from '../../../components/canvas/ui-jsx.test-utils'
import { isParseSuccess } from '../../shared/project-file-types'

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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    expect(testPrintParsedTextFile('/index.js', actualResult)).toEqual(code)
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    expect(testPrintParsedTextFile('/index.js', actualResult)).toEqual(code)
    const exports = Utils.path(['exportsDetail'], actualResult)
    expect(exports).toMatchInlineSnapshot(`
      Object {
        "defaultExport": null,
        "namedExports": Object {
          "whatever": Object {
            "moduleName": undefined,
            "name": "whatever",
            "type": "EXPORT_DETAIL_NAMED",
          },
        },
      }
    `)
  })

  it(`parses an exported default expression`, () => {
    const code = `export default 2 + 2`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": Object {
            "type": "EXPORT_DEFAULT_EXPRESSION",
          },
          "namedExports": Object {},
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses a destructured assignment with renaming`, () => {
    const code = `const entireValue = { name: 'Sean', surname: 'Parsons' }
export const { name: firstName, surname } = entireValue`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "name": Object {
              "moduleName": undefined,
              "name": "firstName",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "surname": Object {
              "moduleName": undefined,
              "name": "surname",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses an export object`, () => {
    const code = `const thing1 = 1
const thing2 = 2
export { thing1, thing2 }`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "thing1": Object {
              "moduleName": undefined,
              "name": "thing1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "thing2": Object {
              "moduleName": undefined,
              "name": "thing2",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses an export object with renamed fields`, () => {
    const code = `const thing1 = 1
const thing2 = 2
export { thing1 as importantThing1, thing2 as importantThing2 }`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "importantThing1": Object {
              "moduleName": undefined,
              "name": "thing1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "importantThing2": Object {
              "moduleName": undefined,
              "name": "thing2",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses an export object with renamed fields, where one is specified as 'default'`, () => {
    const code = `const thing1 = 1
const thing2 = 2
export { thing1 as default, thing2 as importantThing2 }`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": Object {
            "name": "thing1",
            "type": "EXPORT_DEFAULT_NAMED",
          },
          "namedExports": Object {
            "importantThing2": Object {
              "moduleName": undefined,
              "name": "thing2",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses unassigned variable names`, () => {
    const code = `import * as React from "react";
export var exportedVar1, exportedVar2;
export let exportedLet1, exportedLet2;
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "exportedLet1": Object {
              "moduleName": undefined,
              "name": "exportedLet1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedLet2": Object {
              "moduleName": undefined,
              "name": "exportedLet2",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedVar1": Object {
              "moduleName": undefined,
              "name": "exportedVar1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedVar2": Object {
              "moduleName": undefined,
              "name": "exportedVar2",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses assigned variable names`, () => {
    const code = `import * as React from "react";
export var exportedVar1 = 'var1', exportedVar2 = 'var2';
export let exportedLet1 = 'let1', exportedLet2 = 'let2';
export const exportedConst1 = 'const1', exportedConst2 = 'const2';
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "exportedConst1": Object {
              "moduleName": undefined,
              "name": "exportedConst1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedConst2": Object {
              "moduleName": undefined,
              "name": "exportedConst2",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedLet1": Object {
              "moduleName": undefined,
              "name": "exportedLet1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedLet2": Object {
              "moduleName": undefined,
              "name": "exportedLet2",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedVar1": Object {
              "moduleName": undefined,
              "name": "exportedVar1",
              "type": "EXPORT_DETAIL_NAMED",
            },
            "exportedVar2": Object {
              "moduleName": undefined,
              "name": "exportedVar2",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses 'export function' marked values`, () => {
    const code = `import * as React from "react";
export function App() {
  return <div />
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "App": Object {
              "type": "EXPORT_DETAIL_MODIFIER",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses 'export function' marked values`, () => {
    const code = `import * as React from "react";
export function App() {
  return <div />
}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "App": Object {
              "type": "EXPORT_DETAIL_MODIFIER",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses 'export class' marked values`, () => {
    const code = `import * as React from "react";
export class App {}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": null,
          "namedExports": Object {
            "App": Object {
              "moduleName": undefined,
              "name": "App",
              "type": "EXPORT_DETAIL_NAMED",
            },
          },
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses 'export default function'`, () => {
    const code = `import * as React from "react";
export default function() { return 5 }
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": Object {
            "type": "EXPORT_DEFAULT_FUNCTION",
          },
          "namedExports": Object {},
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses 'export default function <functionname>'`, () => {
    const code = `import * as React from "react";
export default function addFive() { return 5 }
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": Object {
            "name": "addFive",
            "type": "EXPORT_DEFAULT_MODIFIER",
          },
          "namedExports": Object {},
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })

  it(`parses 'export default class' marked components`, () => {
    const code = `import * as React from "react";
export default class App {}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Object {
          "defaultExport": Object {
            "name": "App",
            "type": "EXPORT_DEFAULT_MODIFIER",
          },
          "namedExports": Object {},
        }
      `)
    } else {
      fail('Did not parse successfully.')
    }
  })
})
