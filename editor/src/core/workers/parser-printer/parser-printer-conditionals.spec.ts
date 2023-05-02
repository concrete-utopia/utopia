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

describe('JSX parser', () => {
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

describe('JSX printer', () => {
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
