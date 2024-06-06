import {
  isArbitraryJSBlock,
  type ArbitraryJSBlock,
  type UtopiaJSXComponent,
  isUtopiaJSXComponent,
} from '../../shared/element-template'
import { isParseSuccess } from '../../shared/project-file-types'
import { printCode, printCodeOptions } from './parser-printer'
import {
  clearParseResultSourceMapsUniqueIDsAndEmptyBlocks,
  clearParseResultUniqueIDsAndEmptyBlocks,
  testParseCode,
} from './parser-printer.test-utils'

function getLoneMainTopLevelElement(code: string): UtopiaJSXComponent | ArbitraryJSBlock {
  const parseResult = clearParseResultSourceMapsUniqueIDsAndEmptyBlocks(testParseCode(code))
  if (isParseSuccess(parseResult)) {
    const topLevelElements = parseResult.topLevelElements
    const mainTopLevelElements = topLevelElements.filter(
      (element): element is UtopiaJSXComponent | ArbitraryJSBlock =>
        isUtopiaJSXComponent(element) || isArbitraryJSBlock(element),
    )
    switch (mainTopLevelElements.length) {
      case 0:
        throw new Error(`No main top level elements found.`)
      case 1:
        return mainTopLevelElements[0]
      default:
        throw new Error(
          `More than one main top level element found: ${JSON.stringify(
            topLevelElements,
            null,
            2,
          )}`,
        )
    }
  } else {
    throw new Error(`Not a successful parse: ${JSON.stringify(parseResult, null, 2)}`)
  }
}

function testParsePrintParse(code: string): string {
  const firstParse = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))

  if (!isParseSuccess(firstParse)) {
    throw new Error(JSON.stringify(firstParse))
  }

  const firstAsParseSuccess = firstParse

  const printed = printCode(
    '/index.js',
    printCodeOptions(false, true, true),
    firstAsParseSuccess.imports,
    firstAsParseSuccess.topLevelElements,
    firstAsParseSuccess.jsxFactoryFunction,
    firstAsParseSuccess.exportsDetail,
  )

  const secondParse = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printed))

  if (!isParseSuccess(secondParse)) {
    throw new Error(JSON.stringify(secondParse))
  }

  const secondAsParseSuccess = firstParse
  expect(secondAsParseSuccess.topLevelElements).toEqual(firstAsParseSuccess.topLevelElements)

  return printed
}

const simpleReactMemoComponentCode = `import React from "react";
export const Wrapped = React.memo(() => {
  return <div data-uid={'wrapped'} />
})`

const forwardRefComponentCode = `import {forwardRef} from "react";
export const Wrapped = forwardRef(({left, width}, ref) => {
  return <div style={{left: left, width: width}} ref={ref} data-uid={'wrapped'} />
})`

describe('parseCode', () => {
  it('simple function component wrapped in a React.memo', () => {
    const element = getLoneMainTopLevelElement(simpleReactMemoComponentCode)
    expect(element.type).toEqual('UTOPIA_JSX_COMPONENT')
    expect(element).toMatchInlineSnapshot(`
      Object {
        "arbitraryJSBlock": null,
        "blockOrExpression": "block",
        "declarationSyntax": "const",
        "functionWrapping": Array [
          Object {
            "functionExpression": Object {
              "comments": Object {
                "leadingComments": Array [],
                "trailingComments": Array [],
              },
              "onValue": Object {
                "comments": Object {
                  "leadingComments": Array [],
                  "trailingComments": Array [],
                },
                "name": "React",
                "sourceMap": null,
                "type": "JS_IDENTIFIER",
                "uid": "",
              },
              "optionallyChained": "not-optionally-chained",
              "originalJavascript": "React.memo",
              "property": "memo",
              "sourceMap": null,
              "type": "JS_PROPERTY_ACCESS",
              "uid": "",
            },
            "type": "SIMPLE_FUNCTION_WRAP",
          },
        ],
        "isFunction": true,
        "name": "Wrapped",
        "params": Array [],
        "propsUsed": Array [],
        "returnStatementComments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "rootElement": Object {
          "children": Array [],
          "name": Object {
            "baseVariable": "div",
            "propertyPath": Object {
              "propertyElements": Array [],
            },
          },
          "props": Array [
            Object {
              "comments": Object {
                "leadingComments": Array [],
                "trailingComments": Array [],
              },
              "key": "data-uid",
              "type": "JSX_ATTRIBUTES_ENTRY",
              "value": Object {
                "comments": Object {
                  "leadingComments": Array [],
                  "trailingComments": Array [],
                },
                "type": "ATTRIBUTE_VALUE",
                "uid": "",
                "value": "wrapped",
              },
            },
          ],
          "type": "JSX_ELEMENT",
          "uid": "",
        },
        "type": "UTOPIA_JSX_COMPONENT",
        "usedInReactDOMRender": false,
      }
    `)
  })

  it('handles a more complicated forwardRef case', () => {
    const element = getLoneMainTopLevelElement(forwardRefComponentCode)
    expect(element.type).toEqual('UTOPIA_JSX_COMPONENT')
    expect(element).toMatchInlineSnapshot(`
      Object {
        "arbitraryJSBlock": null,
        "blockOrExpression": "block",
        "declarationSyntax": "const",
        "functionWrapping": Array [
          Object {
            "functionExpression": Object {
              "comments": Object {
                "leadingComments": Array [],
                "trailingComments": Array [],
              },
              "name": "forwardRef",
              "sourceMap": null,
              "type": "JS_IDENTIFIER",
              "uid": "",
            },
            "type": "SIMPLE_FUNCTION_WRAP",
          },
        ],
        "isFunction": true,
        "name": "Wrapped",
        "params": Array [
          Object {
            "boundParam": Object {
              "parts": Array [
                Object {
                  "defaultExpression": null,
                  "param": Object {
                    "boundParam": Object {
                      "defaultExpression": null,
                      "paramName": "left",
                      "type": "REGULAR_PARAM",
                    },
                    "dotDotDotToken": false,
                    "type": "PARAM",
                  },
                  "propertyName": undefined,
                },
                Object {
                  "defaultExpression": null,
                  "param": Object {
                    "boundParam": Object {
                      "defaultExpression": null,
                      "paramName": "width",
                      "type": "REGULAR_PARAM",
                    },
                    "dotDotDotToken": false,
                    "type": "PARAM",
                  },
                  "propertyName": undefined,
                },
              ],
              "type": "DESTRUCTURED_OBJECT",
            },
            "dotDotDotToken": false,
            "type": "PARAM",
          },
          Object {
            "boundParam": Object {
              "defaultExpression": null,
              "paramName": "ref",
              "type": "REGULAR_PARAM",
            },
            "dotDotDotToken": false,
            "type": "PARAM",
          },
        ],
        "propsUsed": Array [
          "left",
          "width",
        ],
        "returnStatementComments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "rootElement": Object {
          "children": Array [],
          "name": Object {
            "baseVariable": "div",
            "propertyPath": Object {
              "propertyElements": Array [],
            },
          },
          "props": Array [
            Object {
              "comments": Object {
                "leadingComments": Array [],
                "trailingComments": Array [],
              },
              "key": "style",
              "type": "JSX_ATTRIBUTES_ENTRY",
              "value": Object {
                "comments": Object {
                  "leadingComments": Array [],
                  "trailingComments": Array [],
                },
                "content": Array [
                  Object {
                    "comments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "key": "left",
                    "keyComments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "type": "PROPERTY_ASSIGNMENT",
                    "value": Object {
                      "comments": Object {
                        "leadingComments": Array [],
                        "trailingComments": Array [],
                      },
                      "name": "left",
                      "sourceMap": null,
                      "type": "JS_IDENTIFIER",
                      "uid": "",
                    },
                  },
                  Object {
                    "comments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "key": "width",
                    "keyComments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "type": "PROPERTY_ASSIGNMENT",
                    "value": Object {
                      "comments": Object {
                        "leadingComments": Array [],
                        "trailingComments": Array [],
                      },
                      "name": "width",
                      "sourceMap": null,
                      "type": "JS_IDENTIFIER",
                      "uid": "",
                    },
                  },
                ],
                "type": "ATTRIBUTE_NESTED_OBJECT",
                "uid": "",
              },
            },
            Object {
              "comments": Object {
                "leadingComments": Array [],
                "trailingComments": Array [],
              },
              "key": "ref",
              "type": "JSX_ATTRIBUTES_ENTRY",
              "value": Object {
                "comments": Object {
                  "leadingComments": Array [],
                  "trailingComments": Array [],
                },
                "name": "ref",
                "sourceMap": null,
                "type": "JS_IDENTIFIER",
                "uid": "",
              },
            },
            Object {
              "comments": Object {
                "leadingComments": Array [],
                "trailingComments": Array [],
              },
              "key": "data-uid",
              "type": "JSX_ATTRIBUTES_ENTRY",
              "value": Object {
                "comments": Object {
                  "leadingComments": Array [],
                  "trailingComments": Array [],
                },
                "type": "ATTRIBUTE_VALUE",
                "uid": "",
                "value": "wrapped",
              },
            },
          ],
          "type": "JSX_ELEMENT",
          "uid": "",
        },
        "type": "UTOPIA_JSX_COMPONENT",
        "usedInReactDOMRender": false,
      }
    `)
  })
})

describe('printCode', () => {
  it('simple function component wrapped in a React.memo', () => {
    const actualResult = testParsePrintParse(simpleReactMemoComponentCode)
    expect(actualResult).toMatchInlineSnapshot(`
      "import React from 'react'
      export const Wrapped = React.memo(() => {
        return <div data-uid='wrapped' />
      })
      "
    `)
  })
  it('forwardRef wrapped component', () => {
    const actualResult = testParsePrintParse(forwardRefComponentCode)
    expect(actualResult).toMatchInlineSnapshot(`
      "import { forwardRef } from 'react'
      export const Wrapped = forwardRef(
        ({ left, width }, ref) => {
          return (
            <div
              style={{ left: left, width: width }}
              ref={ref}
              data-uid='wrapped'
            />
          )
        },
      )
      "
    `)
  })
})
