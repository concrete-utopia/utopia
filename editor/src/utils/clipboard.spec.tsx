import { contentsToTree } from '../components/assets'
import {
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../components/canvas/ui-jsx.test-utils'
import { createCodeFile } from '../components/custom-code/code-file.test-utils'
import { selectComponents } from '../components/editor/actions/action-creators'
import { DefaultPackageJson, StoryboardFilePath } from '../components/editor/store/editor-state'
import { BakedInStoryboardUID } from '../core/model/scene-utils'
import type { ProjectContents } from '../core/shared/project-file-types'
import { directory } from '../core/shared/project-file-types'
import {
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'
import * as EP from '../core/shared/element-path'
import { createClipboardDataFromSelection } from './clipboard'
import json5 from 'json5'

describe('copy to clipboard', () => {
  it('creates copy data multifile elements', async () => {
    const appFilePath = '/src/app.js'
    const cardFilePath = '/src/card.js'
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
import React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      data-uid='${TestSceneUID}'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
      ),
      [appFilePath]: createCodeFile(
        appFilePath,
        `
import React from 'react'
import { Card } from '/src/card.js'
export var App = (props) => {
  return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
    <Card data-uid='card-element-to-copy'/>
  </div>
}`,
      ),
      [cardFilePath]: createCodeFile(
        cardFilePath,
        `
import React from 'react'
export var Card = (props) => {
  return <div data-uid='card-outer-div'/>
}`,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(
      contentsToTree(projectContents),
      'dont-await-first-dom-report',
    )
    const targetPath1 = EP.appendNewElementPath(TestScenePath, [
      'app-outer-div',
      'card-element-to-copy',
    ])

    await renderResult.dispatch([selectComponents([targetPath1], false)], false)
    const clipboardData = createClipboardDataFromSelection(
      renderResult.getEditorState().editor,
      renderResult.getEditorState().builtInDependencies,
    )

    expect(clipboardData?.data.length).toEqual(1)
    expect(json5.stringify(clipboardData?.data[0]?.copyDataWithPropsPreserved.elements, null, 2))
      .toMatchInlineSnapshot(`
      "[
        {
          element: {
            type: \\"JSX_ELEMENT\\",
            name: {
              baseVariable: \\"Card\\",
              propertyPath: {
                propertyElements: []
              }
            },
            uid: \\"card-element-to-copy\\",
            props: [
              {
                type: \\"JSX_ATTRIBUTES_ENTRY\\",
                key: \\"data-uid\\",
                value: {
                  type: \\"ATTRIBUTE_VALUE\\",
                  value: \\"card-element-to-copy\\",
                  comments: {
                    leadingComments: [],
                    trailingComments: []
                  },
                  uid: \\"6149c7b762b3930ca5073ae2022468fd\\"
                },
                comments: {
                  leadingComments: [],
                  trailingComments: []
                }
              }
            ],
            children: []
          },
          importsToAdd: {
            \\"/src/card.js\\": {
              importedWithName: null,
              importedFromWithin: [
                {
                  name: \\"Card\\",
                  alias: \\"Card\\"
                }
              ],
              importedAs: null
            }
          },
          originalElementPath: {
            type: \\"elementpath\\",
            parts: [
              [
                \\"utopia-storyboard-uid\\",
                \\"scene-aaa\\",
                \\"app-entity\\"
              ],
              [
                \\"app-outer-div\\",
                \\"card-element-to-copy\\"
              ]
            ]
          },
          duplicateNameMap: {}
        }
      ]"
    `)
  })
})
