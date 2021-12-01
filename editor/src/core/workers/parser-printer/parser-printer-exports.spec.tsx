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
      Array [
        Object {
          "functionName": "whatever",
          "type": "EXPORT_FUNCTION",
        },
      ]
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
      Array [
        Object {
          "type": "EXPORT_VARIABLES",
          "variables": Array [
            Object {
              "variableAlias": null,
              "variableName": "whatever",
            },
          ],
        },
      ]
    `)
  })

  it(`parses an exported default expression`, () => {
    const code = `export default 2 + 2`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`Array []`)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses a destructured assignment with renaming`, () => {
    const code = `const entireValue = { name: 'Sean', surname: 'Parsons' }
export const { name: firstName, surname } = entireValue`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "type": "EXPORT_DESTRUCTURED_ASSIGNMENT",
            "variables": Array [
              Object {
                "variableAlias": "name",
                "variableName": "firstName",
              },
              Object {
                "variableAlias": null,
                "variableName": "surname",
              },
            ],
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses an export object`, () => {
    const code = `const thing1 = 1
const thing2 = 2
export { thing1, thing2 }`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "type": "EXPORT_VARIABLES",
            "variables": Array [
              Object {
                "variableAlias": null,
                "variableName": "thing1",
              },
              Object {
                "variableAlias": null,
                "variableName": "thing2",
              },
            ],
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses an export object with renamed fields`, () => {
    const code = `const thing1 = 1
const thing2 = 2
export { thing1 as importantThing1, thing2 as importantThing2 }`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "type": "EXPORT_VARIABLES",
            "variables": Array [
              Object {
                "variableAlias": "importantThing1",
                "variableName": "thing1",
              },
              Object {
                "variableAlias": "importantThing2",
                "variableName": "thing2",
              },
            ],
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses an export object with renamed fields, where one is specified as 'default'`, () => {
    const code = `const thing1 = 1
const thing2 = 2
export { thing1 as default, thing2 as importantThing2 }`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "type": "EXPORT_VARIABLES",
            "variables": Array [
              Object {
                "variableAlias": "default",
                "variableName": "thing1",
              },
              Object {
                "variableAlias": "importantThing2",
                "variableName": "thing2",
              },
            ],
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
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
        Array [
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedVar1",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedVar2",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedLet1",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedLet2",
            ],
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
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
        Array [
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedVar1",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedVar2",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedLet1",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedLet2",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedConst1",
            ],
          },
          Object {
            "type": "EXPORT_VARIABLES_WITH_MODIFIER",
            "variables": Array [
              "exportedConst2",
            ],
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
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
        Array [
          Object {
            "functionName": "App",
            "type": "EXPORT_FUNCTION",
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses 'export class' marked values`, () => {
    const code = `import * as React from "react";
export class App {}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "className": "App",
            "type": "EXPORT_CLASS",
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses 'export default function'`, () => {
    const code = `import * as React from "react";
export default function() { return 5 }
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "name": null,
            "type": "EXPORT_DEFAULT_FUNCTION_OR_CLASS",
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses 'export default function <functionname>'`, () => {
    const code = `import * as React from "react";
export default function addFive() { return 5 }
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "name": "addFive",
            "type": "EXPORT_DEFAULT_FUNCTION_OR_CLASS",
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })

  it(`parses 'export default class' marked components`, () => {
    const code = `import * as React from "react";
export default class App {}
`
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(actualResult)) {
      expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
        Array [
          Object {
            "name": "App",
            "type": "EXPORT_DEFAULT_FUNCTION_OR_CLASS",
          },
        ]
      `)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })
  describe(`re-exports`, () => {
    it(`parses a wildcard re-export from another module`, () => {
      const code = `export * from 'othermodule'`

      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      if (isParseSuccess(actualResult)) {
        expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
          Array [
            Object {
              "namespacedVariable": null,
              "reexportedModule": "othermodule",
              "type": "REEXPORT_WILDCARD",
            },
          ]
        `)
      } else {
        throw new Error(JSON.stringify(actualResult))
      }
    })
    it(`parses a wildcard re-export into a named value from another module`, () => {
      const code = `export * as thatmodule from 'othermodule'`

      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      if (isParseSuccess(actualResult)) {
        expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
          Array [
            Object {
              "namespacedVariable": "thatmodule",
              "reexportedModule": "othermodule",
              "type": "REEXPORT_WILDCARD",
            },
          ]
        `)
      } else {
        throw new Error(JSON.stringify(actualResult))
      }
    })
    it(`parses a re-export of specific named values from another module`, () => {
      const code = `export { thing1, thing2 } from 'othermodule'`

      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      if (isParseSuccess(actualResult)) {
        expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
          Array [
            Object {
              "reexportedModule": "othermodule",
              "type": "REEXPORT_VARIABLES",
              "variables": Array [
                Object {
                  "variableAlias": null,
                  "variableName": "thing1",
                },
                Object {
                  "variableAlias": null,
                  "variableName": "thing2",
                },
              ],
            },
          ]
        `)
      } else {
        throw new Error(JSON.stringify(actualResult))
      }
    })
    it(`parses a re-export of specific named values from another module 2`, () => {
      const code = `export { import1 as thing1, import2 as thing2 } from 'othermodule'`

      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      if (isParseSuccess(actualResult)) {
        expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
          Array [
            Object {
              "reexportedModule": "othermodule",
              "type": "REEXPORT_VARIABLES",
              "variables": Array [
                Object {
                  "variableAlias": "thing1",
                  "variableName": "import1",
                },
                Object {
                  "variableAlias": "thing2",
                  "variableName": "import2",
                },
              ],
            },
          ]
        `)
      } else {
        throw new Error(JSON.stringify(actualResult))
      }
    })
    it(`parses a re-export of the default export from another module`, () => {
      const code = `export { default } from 'othermodule'`

      const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
      if (isParseSuccess(actualResult)) {
        expect(actualResult.exportsDetail).toMatchInlineSnapshot(`
          Array [
            Object {
              "reexportedModule": "othermodule",
              "type": "REEXPORT_VARIABLES",
              "variables": Array [
                Object {
                  "variableAlias": null,
                  "variableName": "default",
                },
              ],
            },
          ]
        `)
      } else {
        throw new Error(JSON.stringify(actualResult))
      }
    })
  })
})
