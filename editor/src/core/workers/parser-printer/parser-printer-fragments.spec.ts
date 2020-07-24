import { printCode, printCodeOptions } from './parser-printer'
import { applyPrettier } from './prettier-utils'
import { isRight } from '../../shared/either'
import { testParseCode, clearParseResultUniqueIDs } from './parser-printer-test-utils'
import { elementsStructure } from '../../../utils/test-utils'
import { AwkwardFragmentsCode } from './parser-printer-fragments-test-utils'

describe('JSX parser', () => {
  it('handle some weird nested fragments', () => {
    const code = applyPrettier(AwkwardFragmentsCode, false).formatted
    const parseResult = clearParseResultUniqueIDs(testParseCode(code))
    if (isRight(parseResult)) {
      expect(elementsStructure(parseResult.value.topLevelElements)).toMatchInlineSnapshot(`
        "  JSX_ELEMENT - aaa
            JSX_TEXT_BLOCK
            JSX_FRAGMENT
            JSX_ELEMENT - bbb
              JSX_TEXT_BLOCK
              JSX_FRAGMENT
              JSX_ELEMENT - ccc
              JSX_TEXT_BLOCK
            JSX_TEXT_BLOCK
            JSX_ELEMENT - ddd
              JSX_TEXT_BLOCK
          JSX_ELEMENT - eee
            JSX_ELEMENT - fff"
      `)

      const printedCode = printCode(
        printCodeOptions(false, true, true),
        parseResult.value.imports,
        parseResult.value.topLevelElements,
        parseResult.value.jsxFactoryFunction,
      )

      expect(printedCode).toMatchInlineSnapshot(`
        "/** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ ...props.style, backgroundColor: '#FFFFFF' }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid={'aaa'}
            >
              <React.Fragment>
                <div data-label={'random-div'} style={{ width: 100, height: 100 }} data-uid={'bbb'}>
                  Hello
                  <>
                    <div
                      data-label={'some-other-div'}
                      style={{ width: 100, height: 100 }}
                      data-uid={'ccc'}
                    />
                  </>
                </div>
              </React.Fragment>
              <div data-uid={'ddd'}>World</div>
            </View>
          )
        }
        export var storyboard = (
          <Storyboard layout={{ layoutSystem: 'pinSystem' }} data-uid={'eee'}>
            <Scene
              style={{ height: 812, left: 0, width: 375, top: 0 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ style: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'fff'}
            />
          </Storyboard>
        )
        "
      `)
    } else {
      fail(parseResult.value)
    }
  })
})
