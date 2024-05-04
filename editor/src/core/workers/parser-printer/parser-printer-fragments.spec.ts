import { printCode, printCodeOptions } from './parser-printer'
import { applyPrettier } from 'utopia-vscode-common'
import { testParseCode, elementsStructure } from './parser-printer.test-utils'
import { AwkwardFragmentsCode } from './parser-printer-fragments.test-utils'
import { isParseSuccess } from '../../shared/project-file-types'

describe('JSX parser', () => {
  it('handle some weird nested fragments', () => {
    const code = applyPrettier(AwkwardFragmentsCode, false).formatted
    const parseResult = testParseCode(code)
    if (isParseSuccess(parseResult)) {
      expect(elementsStructure(parseResult.topLevelElements)).toMatchInlineSnapshot(`
        "IMPORT_STATEMENT
        UNPARSED_CODE
        IMPORT_STATEMENT
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - App
          JSX_ELEMENT - View - aaa
            JSX_FRAGMENT - ggg
              JSX_ELEMENT - div - bbb
                JSX_TEXT_BLOCK - 3ed
                JSX_FRAGMENT - 670
                  JSX_ELEMENT - div - ccc
                      ATTRIBUTE_VALUE - 842
                      ATTRIBUTE_VALUE - 2a9
                  ATTRIBUTE_VALUE - c56
                  ATTRIBUTE_VALUE - f10
            JSX_ELEMENT - div - ddd
              JSX_TEXT_BLOCK - 3d0
              JS_PROPERTY_ACCESS - 296
              ATTRIBUTE_VALUE - 683
              ATTRIBUTE_VALUE - ab2
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
                  ATTRIBUTE_VALUE - 207
                  ATTRIBUTE_VALUE - 791
                  ATTRIBUTE_VALUE - bd1
                  ATTRIBUTE_VALUE - a4b
                  ATTRIBUTE_VALUE - da5
                ATTRIBUTE_VALUE - 09a
                ATTRIBUTE_VALUE - 78a
                ATTRIBUTE_VALUE - d89
                ATTRIBUTE_VALUE - 4bd
                ATTRIBUTE_VALUE - e9e
        UNPARSED_CODE"
      `)

      const printedCode = printCode(
        '/index.js',
        printCodeOptions(false, true, true),
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
            <View
              style={{
                ...props.style,
                position: 'relative',
                backgroundColor: '#FFFFFF',
              }}
              data-uid='aaa'
            >
              <React.Fragment>
                <div
                  data-label='random-div'
                  style={{ width: 100, height: 100 }}
                  data-uid='bbb'
                >
                  Hello
                  <>
                    <div
                      data-label='some-other-div'
                      style={{ width: 100, height: 100 }}
                      data-uid='ccc'
                    />
                  </>
                </div>
              </React.Fragment>
              <div data-uid='ddd'>World</div>
            </View>
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
      throw new Error(JSON.stringify(parseResult))
    }
  })
})
