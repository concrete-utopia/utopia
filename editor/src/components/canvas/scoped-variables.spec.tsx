import json5 from 'json5'
import { createBuiltInDependenciesList } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { getSamplePreviewFile, getSamplePreviewHTMLFile } from '../../core/model/new-project-files'
import * as EP from '../../core/shared/element-path'
import { objectMap } from '../../core/shared/object-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import type { ProjectContents, TextFile } from '../../core/shared/project-file-types'
import {
  textFile,
  textFileContents,
  unparsed,
  RevisionsState,
  directory,
} from '../../core/shared/project-file-types'
import { complexDefaultProjectPreParsed } from '../../sample-projects/sample-project-utils.test-utils'
import type { ProjectContentTreeRoot } from '../assets'
import { contentsToTree } from '../assets'
import type { PersistentModel } from '../editor/store/editor-state'
import {
  DefaultPackageJson,
  StoryboardFilePath,
  persistentModelForProjectContents,
} from '../editor/store/editor-state'
import type { VariableData, VariableMetadata } from './ui-jsx-canvas'
import {
  DefaultStartingFeatureSwitches,
  renderTestEditorWithModel,
  renderTestEditorWithProjectContent,
} from './ui-jsx.test-utils'

function projectWithInlineComponentContents(): ProjectContents {
  function createCodeFile(path: string, contents: string): TextFile {
    return textFile(textFileContents(contents, unparsed, RevisionsState.CodeAhead), null, null, 0)
  }

  return {
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
    '/assets': directory(),
    '/public': directory(),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app-entity' />
    </Scene>
  </Storyboard>
)`,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `import * as React from 'react'


export const FakeAwait = (props) => {
  const computed =
    typeof props.children === 'function'
      ? props.children(
          <div data-uid={'elephant'}>Elephant</div>,
        )
      : 'Oh no'
  return <div data-uid='fakeawait'>{computed}</div>
}

export var App = (props) => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        background: 'white',
        justifyContent: 'center',
        alignItems: 'center',
      }}
    >
      <FakeAwait data-uid={'fakeawaitelement'}>
        {(something) => {
          return (
            <div
              data-uid={'something-div'}
              style={{
                width: '100%',
                height: '100%',
                background: 'white',
                justifyContent: 'center',
                alignItems: 'center',
              }}
            >
              {something}
            </div>
          )
        }}
      </FakeAwait>
    </div>
  )
}`,
    ),
  }
}

function projectWithInlineComponent(): PersistentModel {
  const projectContents: ProjectContents = projectWithInlineComponentContents()
  const persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

function projectWithInlineComponentDestructuredContents(): ProjectContents {
  function createCodeFile(path: string, contents: string): TextFile {
    return textFile(textFileContents(contents, unparsed, RevisionsState.CodeAhead), null, null, 0)
  }

  return {
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
    '/assets': directory(),
    '/public': directory(),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app-entity' />
    </Scene>
  </Storyboard>
)`,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `import * as React from 'react'


export const FakeAwait = (props) => {
  const computed =
    typeof props.children === 'function'
      ? props.children({first: 1, second: {thing: 'a string'}})
      : 'Oh no'
  return <div data-uid='fakeawait'>{computed}</div>
}

export var App = (props) => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        background: 'white',
        justifyContent: 'center',
        alignItems: 'center',
      }}
    >
      <FakeAwait data-uid={'fakeawaitelement'}>
        {({first, second: {thing}}) => {
          return (
            <div
              data-uid={'something-div'}
              style={{
                width: '100%',
                height: '100%',
                background: 'white',
                justifyContent: 'center',
                alignItems: 'center',
              }}
            >
              {first}
            </div>
          )
        }}
      </FakeAwait>
    </div>
  )
}`,
    ),
  }
}

function projectWithInlineComponentDestructured(): PersistentModel {
  const projectContents: ProjectContents = projectWithInlineComponentDestructuredContents()
  const persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

function projectWithDeepNestedScopes(): ProjectContentTreeRoot {
  function createCodeFile(contents: string): TextFile {
    return textFile(textFileContents(contents, unparsed, RevisionsState.CodeAhead), null, null, 0)
  }

  return contentsToTree({
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
    '/assets': directory(),
    '/public': directory(),
    [StoryboardFilePath]: createCodeFile(
      `import * as React from 'react'
  import Utopia, {
    Scene,
    Storyboard,
  } from 'utopia-api'
  import { App } from '/src/app.js'
  
  export var storyboard = (
    <Storyboard data-uid='storyboard-entity'>
      <Scene
        data-label='Imported App'
        data-uid='scene-1-entity'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='app-entity' />
      </Scene>
    </Storyboard>
  )`,
    ),
    '/src/app.js': createCodeFile(
      `import * as React from 'react'
      
      const globalVar = ['alap']
      
      export var App = ({ style }) => {
        const localVar = ['local']
        return (
          <div
            data-uid='app-root'
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
              ...style,
            }}
          >
            {
              // @utopia/uid=globalVarMap
              globalVar.map((global) => (
              <div data-uid='div1'>
                {
                  // @utopia/uid=localVarMap
                  localVar.map((local) => (
                  <div data-uid='div2'>
                    {
                      // @utopia/uid=firstMap
                      ['a', 'b', 'c'].map((first) => (
                      <div data-uid='div3'>
                        {
                          // @utopia/uid=secondMap
                          [1, 2, 3].map((second) => (
                          <div data-uid='div4'>
                            {(() => {
                              const innerVar = ['cica', 'kutya']
                              // @utopia/uid=innerVarMap
                              return innerVar.map((inner) => {
                                return (
                                  <div data-uid='div5'>
                                    {global} -{local} -{first} -
                                    {second} -{inner}
                                  </div>
                                )
                              })
                            })()}
                          </div>
                        ))}
                      </div>
                    ))}
                  </div>
                ))}
              </div>
            ))}
          </div>
        )
      }
      `,
    ),
  })
}

function prettyPrintVariableData(variableData: VariableData) {
  function prettyPrintVariableMetadata(variableMetadata: VariableMetadata) {
    return {
      spiedValue: JSON.stringify(variableMetadata.spiedValue, null, 2),
      insertionCeiling: optionalMap(EP.toString, variableMetadata.insertionCeiling),
    }
  }

  return objectMap(prettyPrintVariableMetadata, variableData)
}

describe('scoped variables', () => {
  it('project with deep nested scopes works', async () => {
    const renderResult = await renderTestEditorWithProjectContent(
      projectWithDeepNestedScopes(),
      'dont-await-first-dom-report',
    )

    expect(
      prettyPrintVariableData(
        renderResult.getEditorState().editor.variablesInScope[
          'storyboard-entity/scene-1-entity/app-entity:app-root/globalvarmap/div1~~~1/localvarmap/div2~~~1/firstmap/div3~~~1/secondmap/div4~~~1/5d9/div5~~~1'
        ],
      ),
    ).toMatchInlineSnapshot(`
      Object {
        "first": Object {
          "insertionCeiling": "storyboard-entity/scene-1-entity/app-entity:app-root/globalvarmap/div1~~~1/localvarmap/div2~~~1/firstmap/div3~~~1",
          "spiedValue": "\\"a\\"",
        },
        "global": Object {
          "insertionCeiling": "storyboard-entity/scene-1-entity/app-entity:app-root/globalvarmap/div1~~~1",
          "spiedValue": "\\"alap\\"",
        },
        "local": Object {
          "insertionCeiling": "storyboard-entity/scene-1-entity/app-entity:app-root/globalvarmap/div1~~~1/localvarmap/div2~~~1",
          "spiedValue": "\\"local\\"",
        },
        "localVar": Object {
          "insertionCeiling": "storyboard-entity/scene-1-entity/app-entity",
          "spiedValue": "[
        \\"local\\"
      ]",
        },
        "second": Object {
          "insertionCeiling": "storyboard-entity/scene-1-entity/app-entity:app-root/globalvarmap/div1~~~1/localvarmap/div2~~~1/firstmap/div3~~~1/secondmap/div4~~~1",
          "spiedValue": "1",
        },
        "style": Object {
          "insertionCeiling": "storyboard-entity/scene-1-entity/app-entity",
          "spiedValue": undefined,
        },
      }
    `)
  })

  it('includes scoped variables for an inline component destructuring the parameter', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithInlineComponentDestructured(),
      'dont-await-first-dom-report',
    )
    expect(renderResult.getEditorState().editor.variablesInScope).toMatchInlineSnapshot(`
      Object {
        "storyboard-entity": Object {},
        "storyboard-entity/scene-1-entity": Object {},
        "storyboard-entity/scene-1-entity/app-entity": Object {},
        "storyboard-entity/scene-1-entity/app-entity:8ba": Object {
          "props": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": Object {
              "data-uid": "app-entity",
            },
          },
        },
        "storyboard-entity/scene-1-entity/app-entity:8ba/fakeawaitelement": Object {
          "props": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": Object {
              "data-uid": "app-entity",
            },
          },
        },
        "storyboard-entity/scene-1-entity/app-entity:8ba/fakeawaitelement/64b/something-div~~~1": Object {
          "first": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
                Array [
                  "8ba",
                  "fakeawaitelement",
                  "64b",
                  "something-div~~~1",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": 1,
          },
          "props": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": Object {
              "data-uid": "app-entity",
            },
          },
          "thing": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
                Array [
                  "8ba",
                  "fakeawaitelement",
                  "64b",
                  "something-div~~~1",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": "a string",
          },
        },
      }
    `)
  })

  it('includes scoped variables for an inline component', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithInlineComponent(),
      'dont-await-first-dom-report',
    )

    expect(renderResult.getEditorState().editor.variablesInScope).toMatchInlineSnapshot(`
      Object {
        "storyboard-entity": Object {},
        "storyboard-entity/scene-1-entity": Object {},
        "storyboard-entity/scene-1-entity/app-entity": Object {},
        "storyboard-entity/scene-1-entity/app-entity:af0": Object {
          "props": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": Object {
              "data-uid": "app-entity",
            },
          },
        },
        "storyboard-entity/scene-1-entity/app-entity:af0/fakeawaitelement": Object {
          "props": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": Object {
              "data-uid": "app-entity",
            },
          },
        },
        "storyboard-entity/scene-1-entity/app-entity:af0/fakeawaitelement/17b/something-div~~~1": Object {
          "props": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": Object {
              "data-uid": "app-entity",
            },
          },
          "something": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
                Array [
                  "af0",
                  "fakeawaitelement",
                  "17b",
                  "something-div~~~1",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": <div
              data-path="storyboard-entity/scene-1-entity/app-entity:af0/fakeawaitelement:fakeawait/elephant~~~1"
              data-uid="elephant~~~1"
            >
              <UtopiaSpiedExoticType(Symbol(react.fragment))>
                <UtopiaSpiedExoticType(Symbol(react.fragment))>
                  Elephant

                </UtopiaSpiedExoticType(Symbol(react.fragment))>
              </UtopiaSpiedExoticType(Symbol(react.fragment))>
            </div>,
          },
        },
      }
    `)
  })
})
