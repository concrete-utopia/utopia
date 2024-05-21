import { createBuiltInDependenciesList } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { getSamplePreviewFile, getSamplePreviewHTMLFile } from '../../core/model/new-project-files'
import type { ProjectContents, TextFile } from '../../core/shared/project-file-types'
import {
  textFile,
  textFileContents,
  unparsed,
  RevisionsState,
  directory,
} from '../../core/shared/project-file-types'
import { complexDefaultProjectPreParsed } from '../../sample-projects/sample-project-utils.test-utils'
import { contentsToTree } from '../assets'
import type { PersistentModel } from '../editor/store/editor-state'
import {
  DefaultPackageJson,
  StoryboardFilePath,
  persistentModelForProjectContents,
} from '../editor/store/editor-state'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from './ui-jsx.test-utils'

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

describe('scoped variables', () => {
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
        "storyboard-entity/scene-1-entity/app-entity:8ba7daac6c3ae7ac62baf4684c147ba7": Object {
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
        "storyboard-entity/scene-1-entity/app-entity:8ba7daac6c3ae7ac62baf4684c147ba7/fakeawaitelement": Object {
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
        "storyboard-entity/scene-1-entity/app-entity:8ba7daac6c3ae7ac62baf4684c147ba7/fakeawaitelement/64b9d91581e426066da472a6ecea0511/something-div~~~1": Object {
          "first": Object {
            "insertionCeiling": Object {
              "parts": Array [
                Array [
                  "storyboard-entity",
                  "scene-1-entity",
                  "app-entity",
                ],
                Array [
                  "8ba7daac6c3ae7ac62baf4684c147ba7",
                  "fakeawaitelement",
                  "64b9d91581e426066da472a6ecea0511",
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
                  "8ba7daac6c3ae7ac62baf4684c147ba7",
                  "fakeawaitelement",
                  "64b9d91581e426066da472a6ecea0511",
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
        "storyboard-entity/scene-1-entity/app-entity:af0bb841073c55643a65afd014c704bf": Object {
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
        "storyboard-entity/scene-1-entity/app-entity:af0bb841073c55643a65afd014c704bf/fakeawaitelement": Object {
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
        "storyboard-entity/scene-1-entity/app-entity:af0bb841073c55643a65afd014c704bf/fakeawaitelement/17b2ac71e71fac9317ce8c1b091c162c/something-div~~~1": Object {
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
                  "af0bb841073c55643a65afd014c704bf",
                  "fakeawaitelement",
                  "17b2ac71e71fac9317ce8c1b091c162c",
                  "something-div~~~1",
                ],
              ],
              "type": "elementpath",
            },
            "spiedValue": <div
              data-path="storyboard-entity/scene-1-entity/app-entity:af0bb841073c55643a65afd014c704bf/fakeawaitelement:fakeawait/elephant~~~1"
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
