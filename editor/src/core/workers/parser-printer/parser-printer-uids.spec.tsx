import { MajesticBrokerTestCaseCode } from '../../../test-cases/majestic-broker'
import {
  emptyComments,
  isJSExpressionMapOrOtherJavaScript,
  isJSXElement,
  isUtopiaJSXComponent,
  jsExpressionOtherJavaScript,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxElement,
  utopiaJSXComponent,
  isJSExpressionOtherJavaScript,
} from '../../shared/element-template'
import type { ProjectContents } from '../../shared/project-file-types'
import { directory } from '../../shared/project-file-types'
import {
  codeFile,
  foldParsedTextFile,
  isParseSuccess,
  RevisionsState,
  TextFile,
  textFile,
  textFileContents,
  unparsed,
} from '../../shared/project-file-types'
import { lintAndParse, parseCode, printCode, printCodeOptions } from './parser-printer'
import { testParseCode } from './parser-printer.test-utils'
import { applyPrettier } from 'utopia-vscode-common'
import type { ProjectContentTreeRoot } from '../../../components/assets'
import { addFileToProjectContents, contentsToTree } from '../../../components/assets'
import {
  DefaultPackageJson,
  StoryboardFilePath,
} from '../../../components/editor/store/editor-state'
import { emptySet } from '../../shared/set-utils'
import { createCodeFile } from '../../../components/custom-code/code-file.test-utils'
import { renderTestEditorWithProjectContent } from '../../../components/canvas/ui-jsx.test-utils'
import { updateFile } from '../../../components/editor/actions/action-creators'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'

function addCodeFileToProjectContents(
  projectContents: ProjectContentTreeRoot,
  path: string,
  contents: string,
): ProjectContentTreeRoot {
  const parseResult = lintAndParse(
    path,
    contents,
    null,
    'trim-bounds',
    'do-not-apply-steganography',
  )
  const file = textFile(
    textFileContents(contents, parseResult, RevisionsState.BothMatch),
    null,
    isParseSuccess(parseResult) ? parseResult : null,
    0,
  )
  return addFileToProjectContents(projectContents, path, file)
}

describe('parseCode', () => {
  it('produces unique IDs for every element', () => {
    const parseResult = testParseCode(MajesticBrokerTestCaseCode)
    foldParsedTextFile(
      (_) => {
        throw new Error('Is a failure.')
      },
      (success) => {
        const projectContents = addFileToProjectContents(
          {},
          StoryboardFilePath,
          textFile(
            textFileContents(MajesticBrokerTestCaseCode, success, RevisionsState.BothMatch),
            null,
            success,
            0,
          ),
        )
        const result = getAllUniqueUids(projectContents)
        expect(result.uniqueIDs).toHaveLength(493)
        expect(result.duplicateIDs).toEqual({})
      },
      (_) => {
        throw new Error('Is unparsed.')
      },
      parseResult,
    )
  })

  it('fixes duplicated UIDs for single file projects', () => {
    const alreadyExistingUIDs_MUTABLE: Set<string> = emptySet()
    let projectContents: ProjectContentTreeRoot = {}

    projectContents = addCodeFileToProjectContents(
      projectContents,
      '/src/app.js',
      `import * as React from 'react'
        import { Card } from './card'

        export const App = (props) => {
          return (
            <div data-uid='duplicated'>
              <div data-uid='duplicated'>Hello World!</div>
              <Card data-uid='aaa' />
            </div>
          )
        }
      `,
    )

    const result = getAllUniqueUids(projectContents)
    expect(result.uniqueIDs).toHaveLength(7)
    expect(result.duplicateIDs).toEqual({})
  })

  it('fixes duplicated UIDs for multifile projects', () => {
    const alreadyExistingUIDs_MUTABLE: Set<string> = emptySet()
    let projectContents = {}

    projectContents = addCodeFileToProjectContents(
      projectContents,
      '/src/app.js',
      `import * as React from 'react'
        import { Card } from './card'

        export const App = (props) => {
          return (
            <div data-uid='duplicated'>
              <div data-uid='duplicated'>Hello World!</div>
              <Card data-uid='aaa' />
            </div>
          )
        }
        `,
    )

    projectContents = addCodeFileToProjectContents(
      projectContents,
      '/src/card.js',
      `import * as React from 'react'

        export const Card = (props) => {
          return (
            <div data-uid='duplicated'>
              <div data-uid='duplicated2'>Hello World!</div>
              <div data-uid='aab'></div>
            </div>
          )
        } `,
    )

    const result = getAllUniqueUids(projectContents)
    expect(result.uniqueIDs).toHaveLength(14)
    expect(result.duplicateIDs).toEqual({})
  })

  it('can successfully load a multifile project with duplicated UIDs', async () => {
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.CodeAhead,
        ),
        null,
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import Utopia, {
    Scene,
    Storyboard,
  } from 'utopia-api'
  import { App } from '/src/app.js'

  export var storyboard = (
    <Storyboard data-uid='storyboard'>
      <Scene
        data-uid='scene'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='app' />
      </Scene>
    </Storyboard>
  )`,
      ),
      '/src/app.js': createCodeFile(
        '/src/app.js',
        `import * as React from 'react'
        import { Card } from './card'

        export const App = (props) => {
          return (
            <div data-uid='duplicated'>
              <div data-uid='duplicated'>Hello World!</div>
              <Card data-uid='aaa' />
            </div>
          )
        }
        `,
      ),
      '/src/card.js': createCodeFile(
        '/src/card.js',
        `import * as React from 'react'

        export const Card = (props) => {
          return (
            <div data-uid='duplicated'>
              <div data-uid='duplicated2'>Hello World!</div>
              <div data-uid='aab'></div>
            </div>
          )
        } `,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(
      contentsToTree(projectContents),
      'dont-await-first-dom-report',
    )

    const result = getAllUniqueUids(renderResult.getEditorState().editor.projectContents)
    expect(result.uniqueIDs).toHaveLength(26)
    expect(result.duplicateIDs).toEqual({})
  })

  it('can successfully handle a multifile project with duplicated UIDs added later', async () => {
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.CodeAhead,
        ),
        null,
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import Utopia, {
    Scene,
    Storyboard,
  } from 'utopia-api'
  import { App } from '/src/app.js'

  export var storyboard = (
    <Storyboard data-uid='storyboard'>
      <Scene
        data-uid='scene'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='duplicated1' />
      </Scene>
    </Storyboard>
  )`,
      ),
      '/src/app.js': createCodeFile(
        '/src/app.js',
        `import * as React from 'react'

        export const App = (props) => {
          return (
            <div data-uid='duplicated2'>
              <div data-uid='duplicated3'>Hello World!</div>
            </div>
          )
        }
        `,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(
      contentsToTree(projectContents),
      'dont-await-first-dom-report',
    )

    const resultBefore = getAllUniqueUids(renderResult.getEditorState().editor.projectContents)
    expect(resultBefore.uniqueIDs).toHaveLength(17)
    expect(resultBefore.duplicateIDs).toEqual({})

    await renderResult.dispatch(
      [
        updateFile(
          '/src/card.js',
          codeFile(
            `
          import * as React from 'react'

          export const Card = (props) => {
            return (
              <div data-uid='duplicated1'>
                <div data-uid='duplicated2'>Hello World!</div>
                <div data-uid='duplicated3'>Hello World!</div>
              </div>
            )
          }
          `,
            null,
          ),
          true,
        ),
      ],
      false,
    )

    const resultAfter = getAllUniqueUids(renderResult.getEditorState().editor.projectContents)
    expect(resultAfter.uniqueIDs).toHaveLength(25)
    expect(resultAfter.duplicateIDs).toEqual({})
  })
})

describe('printCode', () => {
  it('applies changes back into the original code', () => {
    const startingCode = `
import * as react from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
    const parsedCode = parseCode('test.js', startingCode, null, 'do-not-apply-steganography')
    const actualResult = foldParsedTextFile(
      (_) => 'FAILURE',
      (success) => {
        const updatedTopLevelElements = success.topLevelElements.map((tle) => {
          if (isUtopiaJSXComponent(tle)) {
            const rootElement = tle.rootElement
            if (isJSXElement(rootElement)) {
              const firstChild = rootElement.children[0]
              if (isJSExpressionOtherJavaScript(firstChild)) {
                const firstKey = Object.keys(firstChild.elementsWithin)[0]
                const firstElementWithin = firstChild.elementsWithin[firstKey]
                const updatedAttributes = jsxAttributesFromMap({
                  style: jsExpressionValue(
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
                const updatedFirstChild = jsExpressionOtherJavaScript(
                  isJSExpressionOtherJavaScript(firstChild) ? firstChild.params : [],
                  firstChild.originalJavascript,
                  firstChild.javascriptWithUIDs,
                  firstChild.transpiledJavascript,
                  firstChild.definedElsewhere,
                  firstChild.sourceMap,
                  updatedElementsWithin,
                  firstChild.comments,
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
          '/index.js',
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
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
