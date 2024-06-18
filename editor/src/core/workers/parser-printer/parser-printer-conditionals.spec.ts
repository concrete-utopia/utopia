/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectConditionalExpressionAsChild", "expectOtherJavascriptAsChild"] }] */
import { applyPrettier } from 'utopia-vscode-common'
import { testParseCode, elementsStructure } from './parser-printer.test-utils'
import type { ParsedTextFile } from '../../shared/project-file-types'
import { isParseSuccess } from '../../shared/project-file-types'
import {
  NestedTernariesExample,
  SimpleConditionalsExample,
} from './parser-printer-conditionals.test-utils'
import { printCode, printCodeOptions } from './parser-printer'
import type { JSXElementChild } from '../../shared/element-template'
import { isJSXElement } from '../../shared/element-template'
import { findJSXElementChildAtPath } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import { fromStringStatic } from '../../shared/element-path'

describe('Conditonals JSX parser', () => {
  it('ensure that conditionals get the same UID each time', () => {
    const code = applyPrettier(SimpleConditionalsExample, false).formatted
    const firstParseResult = testParseCode(code)
    if (isParseSuccess(firstParseResult)) {
      expect(elementsStructure(firstParseResult.topLevelElements)).toMatchInlineSnapshot(`
        "IMPORT_STATEMENT
        UNPARSED_CODE
        IMPORT_STATEMENT
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - App
          JSX_ELEMENT - div - div
            JSX_CONDITIONAL_EXPRESSION - conditional
              JSX_ELEMENT - div - hello
                JSX_TEXT_BLOCK - c58
              JSX_ELEMENT - div - world
                JSX_TEXT_BLOCK - 935
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
                  ATTRIBUTE_VALUE - 753
                  ATTRIBUTE_VALUE - 2c3
                  ATTRIBUTE_VALUE - 2c3.1
                  ATTRIBUTE_VALUE - 2c3.2
                  ATTRIBUTE_VALUE - 2c3.3
                ATTRIBUTE_VALUE - 753.1
                ATTRIBUTE_VALUE - d23
                ATTRIBUTE_VALUE - 2c3.4
                ATTRIBUTE_VALUE - c79
                ATTRIBUTE_VALUE - 2c3.5
        UNPARSED_CODE"
      `)

      const secondParseResult = testParseCode(code)
      if (isParseSuccess(secondParseResult)) {
        expect(elementsStructure(secondParseResult.topLevelElements)).toEqual(
          elementsStructure(firstParseResult.topLevelElements),
        )
      } else {
        throw new Error(JSON.stringify(secondParseResult, null, 2))
      }
    } else {
      throw new Error(JSON.stringify(firstParseResult, null, 2))
    }
  })

  it('handles nested ternaries', () => {
    const code = applyPrettier(NestedTernariesExample, false).formatted
    const firstParseResult = testParseCode(code)
    if (isParseSuccess(firstParseResult)) {
      expect(elementsStructure(firstParseResult.topLevelElements)).toMatchInlineSnapshot(`
        "IMPORT_STATEMENT
        UNPARSED_CODE
        IMPORT_STATEMENT
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - App
          JSX_ELEMENT - div - div
            JSX_CONDITIONAL_EXPRESSION - conditional1
              JSX_CONDITIONAL_EXPRESSION - conditional2
                JSX_ELEMENT - div - middle
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
                  ATTRIBUTE_VALUE - 753
                  ATTRIBUTE_VALUE - 2c3
                  ATTRIBUTE_VALUE - 2c3.1
                  ATTRIBUTE_VALUE - 2c3.2
                  ATTRIBUTE_VALUE - 2c3.3
                ATTRIBUTE_VALUE - 753.1
                ATTRIBUTE_VALUE - d23
                ATTRIBUTE_VALUE - 2c3.4
                ATTRIBUTE_VALUE - c79
                ATTRIBUTE_VALUE - 2c3.5
        UNPARSED_CODE"
      `)

      const secondParseResult = testParseCode(code)
      if (isParseSuccess(secondParseResult)) {
        expect(elementsStructure(secondParseResult.topLevelElements)).toEqual(
          elementsStructure(firstParseResult.topLevelElements),
        )
      } else {
        throw new Error(JSON.stringify(secondParseResult, null, 2))
      }
    } else {
      throw new Error(JSON.stringify(firstParseResult, null, 2))
    }
  })
})

describe('Conditonals JSX printer', () => {
  it('handles nested ternaries', () => {
    const code = applyPrettier(NestedTernariesExample, false).formatted
    const parseResult = testParseCode(code)
    if (isParseSuccess(parseResult)) {
      expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
        "IMPORT_STATEMENT
        UNPARSED_CODE
        IMPORT_STATEMENT
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - App
          JSX_ELEMENT - div - div
            JSX_CONDITIONAL_EXPRESSION - conditional1
              JSX_CONDITIONAL_EXPRESSION - conditional2
                JSX_ELEMENT - div - middle
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
                  ATTRIBUTE_VALUE - 753
                  ATTRIBUTE_VALUE - 2c3
                  ATTRIBUTE_VALUE - 2c3.1
                  ATTRIBUTE_VALUE - 2c3.2
                  ATTRIBUTE_VALUE - 2c3.3
                ATTRIBUTE_VALUE - 753.1
                ATTRIBUTE_VALUE - d23
                ATTRIBUTE_VALUE - 2c3.4
                ATTRIBUTE_VALUE - c79
                ATTRIBUTE_VALUE - 2c3.5
        UNPARSED_CODE"
      `)

      const printedCode = printCode(
        'code.tsx',
        printCodeOptions(false, true, true, false),
        parseResult.imports,
        parseResult.topLevelElements,
        parseResult.jsxFactoryFunction,
        parseResult.exportsDetail,
      )
      expect(printedCode).toMatchInlineSnapshot(`
        "import * as React from 'react'
        import { Scene, Storyboard, View, Group } from 'utopia-api'
        export var App = (props) => {
          return (
            <div data-uid='div'>
              {
                // @utopia/uid=conditional1
                [0, 1].length > 1 ? (
                  // @utopia/uid=conditional2
                  [0, 1].length === 0 ? (
                    <div data-uid='middle' />
                  ) : null
                ) : null
              }
            </div>
          )
        }
        export var storyboard = (
          <Storyboard data-uid='eee'>
            <Scene
              style={{
                position: 'absolute',
                height: 812,
                left: 0,
                width: 375,
                top: 0,
              }}
              data-uid='fff'
            >
              <App
                data-uid='app'
                style={{
                  position: 'absolute',
                  bottom: 0,
                  left: 0,
                  right: 0,
                  top: 0,
                }}
              />
            </Scene>
          </Storyboard>
        )
        "
      `)
    } else {
      throw new Error(JSON.stringify(parseResult, null, 2))
    }
  })
})

describe('Conditional elements are parse as conditionals', () => {
  it('both branches are regular strings', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : 'The book has been unsealed'
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('one string, one null', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : null
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('one null, one string', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? null
        : 'The book is sealed'
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('both null parses as a full conditional expression with slots', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? null
        : null
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('two template string literals parse as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? \`The book is sealed\`
        : \`The book has been unsealed\`
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('one string, one template string literal parse as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : \`The book has been unsealed\`
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('one template literal, one string', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? \`The book is sealed\`
        : 'The book has been unsealed'
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('two template literals, both use vars parse as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
          ? \`\${bookStatusOpen}\`
          : \`\${bookStatusClosed}\`
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('string and var parse as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
          ? bookStatusOpen
          : "No ceremony"
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('string and span parse as full conditional', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue ? (
          'Descend into the pyramid'
        ) : (
          <span>Pyramid closed for repairs</span>
        )
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })
  it('string literal as span parse as full conditional', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue ? (
          \`Descend into the pyramid\`
        ) : (
          <span>Pyramid closed for repairs</span>
        )
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('string literal with inner expression and span parse as full conditional', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue ? (
          \`\${bookStatusOpen}\`
        ) : (
          <span>No ceremony</span>
        )
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('var and span parse as full conditional', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue ? (
          bookStatusOpen
        ) : (
          <span>No ceremony</span>
        )
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('string and string-only nested conditional', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : sacrificialAnimal === 'goat' // @utopia/uid=conditional2
        ? 'the goat book is open'
        : 'the book is open'
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('string and div-containing nested conditional', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : sacrificialAnimal === 'goat' /* @utopia/uid=conditional2 */
        ? <div>the goat book is open</div>
        : 'the book is open'
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })

  it('string and number', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : 5
      }
    `)
    const parseResult = testParseCode(code)

    expectConditionalExpressionAsChild(parseResult)
  })
})

function getSingleChildOfApp(parseResult: ParsedTextFile): JSXElementChild {
  if (!isParseSuccess(parseResult)) {
    throw new Error('expected parse success')
  }

  const rootDiv = findJSXElementChildAtPath(
    getComponentsFromTopLevelElements(parseResult.topLevelElements),
    fromStringStatic('app'),
  )
  if (rootDiv == null || !isJSXElement(rootDiv)) {
    throw new Error('found no rootDiv jsxelement')
  }
  expect(rootDiv.children.length).toBe(1)
  return rootDiv.children[0]
}

function expectConditionalExpressionAsChild(parseResult: ParsedTextFile): void {
  const child = getSingleChildOfApp(parseResult)
  expect(child.type).toBe('JSX_CONDITIONAL_EXPRESSION')
}

function expectOtherJavascriptAsChild(parseResult: ParsedTextFile): void {
  const child = getSingleChildOfApp(parseResult)
  expect(child.type).toBe('ATTRIBUTE_OTHER_JAVASCRIPT')
}

function createCode(code: string): string {
  return applyPrettier(
    `
    import * as React from 'react'
    export var App = (props) => {
      const isTrue = true
      const greetee = 'Balazs'
      const sacrificialAnimal = 'goat'
    
      const bookStatusOpen = 'Behold The Written Truth '
      const bookStatusClosed = 'Thou art not worthy'

      return <div data-uid={'app'}>
        ${code}
      </div>
    }
  `,
    false,
  ).formatted
}
