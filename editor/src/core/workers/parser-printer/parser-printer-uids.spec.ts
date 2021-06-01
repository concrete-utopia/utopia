import { MajesticBrokerTestCaseCode } from '../../../test-cases/majestic-broker'
import { getAllUniqueUids } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import { uniq } from '../../shared/array-utils'
import {
  isJSXArbitraryBlock,
  isJSXElement,
  isUtopiaJSXComponent,
  jsxArbitraryBlock,
  jsxAttributesFromMap,
  jsxAttributeValue,
  jsxElement,
  utopiaJSXComponent,
} from '../../shared/element-template'
import {
  foldParsedTextFile,
  RevisionsState,
  textFile,
  textFileContents,
} from '../../shared/project-file-types'
import { parseCode, printCode, printCodeOptions } from './parser-printer'
import { emptyComments } from './parser-printer-comments'
import { testParseCode } from './parser-printer.test-utils'
import { applyPrettier } from 'utopia-vscode-common'
import { addFileToProjectContents } from '../../../components/assets'
import { StoryboardFilePath } from '../../../components/editor/store/editor-state'
import { emptySet } from '../../shared/set-utils'

describe('parseCode', () => {
  it('produces unique IDs for every element', () => {
    const parseResult = testParseCode(MajesticBrokerTestCaseCode)
    foldParsedTextFile(
      (_) => fail('Is a failure.'),
      (success) => {
        const projectContents = addFileToProjectContents(
          {},
          StoryboardFilePath,
          textFile(
            textFileContents(MajesticBrokerTestCaseCode, success, RevisionsState.BothMatch),
            null,
            0,
          ),
        )
        const uniqueIDs = getAllUniqueUids(projectContents, 'Unique IDs failure.')
        expect(uniq(uniqueIDs).length).toMatchInlineSnapshot(`77`)
      },
      (_) => fail('Is unparsed.'),
      parseResult,
    )
  })
})

describe('printCode', () => {
  it('applies changes back into the original code', () => {
    const startingCode = `
import * as react from 'react'
import { scene, storyboard, view } from 'utopia-api'

export var app = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', position: 'relative' }}
    >
      {
        <div
          style={{
            backgroundColor: 'red',
            position: 'absolute',
            width: 86,
            height: 130,
            left: 45,
            top: 87,
          }}
        />
      }
    </div>
  )
}
    `
    const parsedCode = parseCode('test.js', startingCode, null, emptySet())
    const actualResult = foldParsedTextFile(
      (_) => 'FAILURE',
      (success) => {
        const updatedTopLevelElements = success.topLevelElements.map((tle) => {
          if (isUtopiaJSXComponent(tle)) {
            const rootElement = tle.rootElement
            if (isJSXElement(rootElement)) {
              const firstChild = rootElement.children[0]
              if (isJSXArbitraryBlock(firstChild)) {
                const firstKey = Object.keys(firstChild.elementsWithin)[0]
                const firstElementWithin = firstChild.elementsWithin[firstKey]
                const updatedAttributes = jsxAttributesFromMap({
                  style: jsxAttributeValue(
                    {
                      backgroundColor: 'red',
                      position: 'absolute',
                      left: 0,
                      top: 0,
                      width: 100,
                      height: 200,
                    },
                    emptyComments,
                  ),
                })
                const updatedElementsWithin = {
                  [firstKey]: jsxElement(
                    firstElementWithin.name,
                    firstKey,
                    updatedAttributes,
                    firstElementWithin.children,
                  ),
                }
                const updatedFirstChild = jsxArbitraryBlock(
                  firstChild.originalJavascript,
                  firstChild.javascript,
                  firstChild.transpiledJavascript,
                  firstChild.definedElsewhere,
                  firstChild.sourceMap,
                  updatedElementsWithin,
                )
                const updatedRootElement = jsxElement(
                  rootElement.name,
                  rootElement.uid,
                  rootElement.props,
                  [updatedFirstChild],
                )
                const updatedComponent = utopiaJSXComponent(
                  tle.name,
                  tle.isFunction,
                  tle.declarationSyntax,
                  tle.blockOrExpression,
                  tle.param,
                  tle.propsUsed,
                  updatedRootElement,
                  tle.arbitraryJSBlock,
                  tle.usedInReactDOMRender,
                  tle.returnStatementComments,
                )
                return updatedComponent
              }
            }
          }
          return tle
        })
        return printCode(
          printCodeOptions(false, true, false, true),
          success.imports,
          updatedTopLevelElements,
          success.jsxFactoryFunction,
          success.exportsDetail,
        )
      },
      (_) => 'UNPARSED',
      parsedCode,
    )
    const expectedResult = applyPrettier(
      `
import * as react from 'react'
import { scene, storyboard, view } from 'utopia-api'

export var app = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', position: 'relative' }}
    >
      {
        <div
          style={{
            backgroundColor: 'red',
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 200,
          }}
        />
      }
    </div>
  )
}
    `,
      false,
    ).formatted
    expect(actualResult).toEqual(expectedResult)
  })
})
