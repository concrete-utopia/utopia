import { applyPrettier } from 'utopia-vscode-common'
import { testParseCode, elementsStructure } from './parser-printer.test-utils'
import { isParseSuccess } from '../../shared/project-file-types'
import { SimpleConditionalsExample } from './parser-printer-conditionals.test-utils'
import { setFeatureForUnitTests } from '../../../utils/utils.test-utils'

describe('JSX parser', () => {
  setFeatureForUnitTests('Conditional support', true)
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
            JSX_CONDITIONAL_EXPRESSION - 9f9
              JSX_ELEMENT - div - hello
                JSX_TEXT_BLOCK
              JSX_ELEMENT - div - world
                JSX_TEXT_BLOCK
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
        throw new Error(JSON.stringify(secondParseResult))
      }
    } else {
      throw new Error(JSON.stringify(firstParseResult))
    }
  })
})
