import { applyPrettier } from 'utopia-vscode-common'
import { testParseCode, elementsStructure } from './parser-printer.test-utils'
import { isParseSuccess } from '../../shared/project-file-types'
import {
  NestedTernariesExample,
  SimpleConditionalsExample,
} from './parser-printer-conditionals.test-utils'
import { setFeatureForUnitTests } from '../../../utils/utils.test-utils'
import { FOR_TESTS_setNextGeneratedUids } from '../../../core/model/element-template-utils.test-utils'
import { printCode, printCodeOptions } from './parser-printer'

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
                JSX_TEXT_BLOCK - 9e1
              JSX_ELEMENT - div - world
                JSX_TEXT_BLOCK - 3d0
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
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
        import { Scene, Storyboard, View } from 'utopia-api'
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

describe('Conditional elements text parsing cases', () => {
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - c58
      UNPARSED_CODE"
    `)
  })

  it('both branches are regular strings parse as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : 'The book has been unsealed'
      }
    `)
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - c58
      UNPARSED_CODE"
    `)
  })

  it('one string, one null parses as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : null
      }
    `)
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 087
      UNPARSED_CODE"
    `)
  })

  it('one null, one string parses as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? null
        : 'The book is sealed'
      }
    `)
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - f9b
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          JSX_CONDITIONAL_EXPRESSION - conditional1
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 293
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 2df
      UNPARSED_CODE"
    `)
  })

  it('one template literal, one string parses as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? \`The book is sealed\`
        : 'The book has been unsealed'
      }
    `)
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 1b3
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 1d1
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: the ATTRIBUTE_OTHER_JAVASCRIPT should be parsed with the correct uid
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 2fb
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    // TODO: notice we are missing the true branch text expression from this snapshot!!!!
    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          JSX_CONDITIONAL_EXPRESSION - conditional1
            JSX_ELEMENT - span - dcf
              JSX_TEXT_BLOCK - 0d5
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          JSX_CONDITIONAL_EXPRESSION - conditional1
            ATTRIBUTE_OTHER_JAVASCRIPT - 4ef
            JSX_ELEMENT - span - dcf
              JSX_TEXT_BLOCK - 0d5
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          JSX_CONDITIONAL_EXPRESSION - conditional1
            ATTRIBUTE_OTHER_JAVASCRIPT - 0b7
            JSX_ELEMENT - span - dcf
              JSX_TEXT_BLOCK - a0c
      UNPARSED_CODE"
    `)
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          JSX_CONDITIONAL_EXPRESSION - conditional1
            ATTRIBUTE_OTHER_JAVASCRIPT - 3ba
            JSX_ELEMENT - span - dcf
              JSX_TEXT_BLOCK - a0c
      UNPARSED_CODE"
    `)
  })

  it('string and nested conditional parses as text', () => {
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
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - 1eb
      UNPARSED_CODE"
    `)
  })

  it('string and number parses as text', () => {
    const code = createCode(`
      {
        // @utopia/uid=conditional1
        isTrue
        ? 'The book is sealed'
        : 5
      }
    `)
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      throw new Error('expected parse success')
    }

    expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
      "IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - App
        JSX_ELEMENT - div - app
          ATTRIBUTE_OTHER_JAVASCRIPT - bb5
      UNPARSED_CODE"
    `)
  })
})

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
