import {
  isArbitraryJSBlock,
  type ArbitraryJSBlock,
  type UtopiaJSXComponent,
  isUtopiaJSXComponent,
} from '../../shared/element-template'
import { isParseSuccess } from '../../shared/project-file-types'
import {
  clearParseResultSourceMapsUniqueIDsAndEmptyBlocks,
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
        throw new Error(`More than one main top level element found: ${topLevelElements}`)
    }
  } else {
    throw new Error(`Not a successful parse: ${parseResult}`)
  }
}

describe('parseCode', () => {
  it('simple function component wrapped in a React.memo', () => {
    const code = `import React from "react";
export const Wrapped = React.memo(() => {
  return <div data-uid={'wrapped'} />
})`
    const element = getLoneMainTopLevelElement(code)
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
        "param": null,
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
})
