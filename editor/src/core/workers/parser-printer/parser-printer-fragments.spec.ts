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
                JSX_TEXT_BLOCK - d855313e82f6a104616cd542f6d6b2a8
                JSX_FRAGMENT - 630d1d9bf5d2e0fa9269b79a9a1b202a
                  JSX_ELEMENT - div - ccc
                      ATTRIBUTE_VALUE - a984f3546052ebcff6aa0d36cde2e66d
                      ATTRIBUTE_VALUE - 50877c4d0141572986201f3c3b247c2c
                  ATTRIBUTE_VALUE - b7e28104185c47149ffeaeef530fcf23
                  ATTRIBUTE_VALUE - f115675584486c9eb9f79bb0293e66cc
            JSX_ELEMENT - div - ddd
              JSX_TEXT_BLOCK - 6accbd8440ebcbb7ffd1ad9a4857260a
              JS_PROPERTY_ACCESS - dc7e0cb8ed1b9231081ae27c42ec0e61
              ATTRIBUTE_VALUE - 95fb96714a79e9f1c4800c25d894af3f
              ATTRIBUTE_VALUE - 7ae86db3df4ddea695025314b23f7854
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
                  ATTRIBUTE_VALUE - 63272da6ceb7d1ccffbead026085a2be
                  ATTRIBUTE_VALUE - 3fa3adc4a7efde193f947d5d9f34abec
                  ATTRIBUTE_VALUE - 5c0c2a898922be22b0882aba05a38565
                  ATTRIBUTE_VALUE - f8cf4ce74a1c50585ace20474cd8c97c
                  ATTRIBUTE_VALUE - 7f45cd3681da68c5e4111ec594fbcc05
                ATTRIBUTE_VALUE - 1060b39053f2d0dcd667abcea31063af
                ATTRIBUTE_VALUE - ed628fce0d97adacaf3ea0e8ac3baeb7
                ATTRIBUTE_VALUE - 351152638320d16f1f6ceb53d61a21ed
                ATTRIBUTE_VALUE - 80f69824389410e76b2940c6c9a32843
                ATTRIBUTE_VALUE - a4dcdf0cf50335090eca6720b35adbb7
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
