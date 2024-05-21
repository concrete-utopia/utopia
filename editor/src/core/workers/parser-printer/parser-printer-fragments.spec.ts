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
                JSX_TEXT_BLOCK - 48154125ecd637752f84cdb0b735e80d
                JSX_FRAGMENT - 630d1d9bf5d2e0fa9269b79a9a1b202a
                  JSX_ELEMENT - div - ccc
                      ATTRIBUTE_VALUE - 193eb5d119ec7e4016c54ca4b7884f76
                      ATTRIBUTE_VALUE - 1710a820453844e9a220c16a6e786b37
                  ATTRIBUTE_VALUE - 87e21a9194f54ae3abf7ca162582faba
                  ATTRIBUTE_VALUE - f4d790c606024497a94eb4b1a1aa34f3
            JSX_ELEMENT - div - ddd
              JSX_TEXT_BLOCK - 935a10fa8e883ee481eebed750b0a602
              JS_PROPERTY_ACCESS - 6222c9a0647a3d71c9401146b8ad26aa
              ATTRIBUTE_VALUE - b79e15a1476b088d4cfa3e13fe899b8d
              ATTRIBUTE_VALUE - 4f5ae3bc6243e822f5c8dc9fc9dbf797
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - storyboard
          JSX_ELEMENT - Storyboard - eee
            JSX_ELEMENT - Scene - fff
              JSX_ELEMENT - App - app
                  ATTRIBUTE_VALUE - 7537c2399208a65294ab51886a38a78b
                  ATTRIBUTE_VALUE - 2c341726649814f0c48fee9bf65abc34
                  ATTRIBUTE_VALUE - e8a6fec5f5ca4a338939bf6d8fe20682
                  ATTRIBUTE_VALUE - e799ec9b732b4646bb6441471dd0ff9c
                  ATTRIBUTE_VALUE - 43d76565cb344d2c8b1e5c170925a358
                ATTRIBUTE_VALUE - 5f1f0987b2c343e0aeb535a126df93f0
                ATTRIBUTE_VALUE - d2376ebbafdaf55fec07c9dd0e7b8726
                ATTRIBUTE_VALUE - 1a4139c198e94ec4b49cbe76e0f23e52
                ATTRIBUTE_VALUE - c794958898be6b77e66d4b1c53f086e6
                ATTRIBUTE_VALUE - e565ed4bf1254b3aa713c05883da6278
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
