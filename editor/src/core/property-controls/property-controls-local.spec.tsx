import { renderTestEditorWithModel, TestAppUID } from '../../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { TestScene0UID } from '../model/test-ui-js-file.test-utils'
import { wait } from '../model/performance-scripts'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'

const project = (componentDescriptorFiles: { [filename: string]: string }) =>
  createModifiedProject({
    [StoryboardFilePath]: `import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <div>hello</div>
    )
  }

  export var storyboard = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestScene0UID}'
        >
          <App data-uid='${TestAppUID}' />
        </Scene>
      </Storyboard>
    )
  }`,
    ...componentDescriptorFiles,
  })

describe('registered property controls', () => {
  it('registered controls from sidecar file are in editor state', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
            background: {
              control: 'color',
            },
            visible: {
              control: 'checkbox',
              defaultValue: true,
            },
          },
          variants: [
            {
              code: '<Card />',
              label: 'Card',
            },
            {
              code: '<Card person={DefaultPerson} />',
              label: 'ID Card',
              additionalImports:
                "import { DefaultPerson } from '/src/defaults';",
            },
          ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.propertyControlsInfo['/src/card']).toMatchInlineSnapshot(`
      Object {
        "Card": Object {
          "preferredChildComponents": Array [],
          "properties": Object {
            "background": Object {
              "control": "color",
            },
            "label": Object {
              "control": "string-input",
            },
            "visible": Object {
              "control": "checkbox",
              "defaultValue": true,
            },
          },
          "supportsChildren": false,
          "variants": Array [
            Object {
              "elementToInsert": [Function],
              "importsToAdd": Object {
                "/src/card": Object {
                  "importedAs": null,
                  "importedFromWithin": Array [
                    Object {
                      "alias": "Card",
                      "name": "Card",
                    },
                  ],
                  "importedWithName": null,
                },
              },
              "insertMenuLabel": "Card",
            },
            Object {
              "elementToInsert": [Function],
              "importsToAdd": Object {
                "/src/card": Object {
                  "importedAs": null,
                  "importedFromWithin": Array [
                    Object {
                      "alias": "Card",
                      "name": "Card",
                    },
                  ],
                  "importedWithName": null,
                },
                "/src/defaults": Object {
                  "importedAs": null,
                  "importedFromWithin": Array [
                    Object {
                      "alias": "DefaultPerson",
                      "name": "DefaultPerson",
                    },
                  ],
                  "importedWithName": null,
                },
              },
              "insertMenuLabel": "ID Card",
            },
          ],
        },
      }
    `)
  })
  it('registered controls for multiple components from sidecar file are in editor state', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
            background: {
              control: 'color',
            },
            visible: {
              control: 'checkbox',
              defaultValue: true,
            },
          },
          variants: [
            {
              code: '<Card />',
              label: 'Card',
            },
            {
              code: '<Card person={DefaultPerson} />',
              label: 'ID Card',
              additionalImports:
                "import { DefaultPerson } from '/src/defaults';",
            },
          ],
        },
        Card2: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [
            {
              code: '<Card2 />',
              label: 'Card2',
            },
            {
              code: '<Card2 label={DefaultLabel} />',
              label: 'ID Card',
              additionalImports:
                "import { DefaultLabel } from '/src/defaults';",
            },
          ],
        },
      },
    }
    
    export default Components    
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(Object.keys(editorState.propertyControlsInfo['/src/card'])).toMatchInlineSnapshot(`
      Array [
        "Card",
        "Card2",
      ]
    `)
  })
  it('registered controls for multiple modules from sidecar file are in editor state', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `const Components = {
        '/src/card': {
          Card: {
            supportsChildren: false,
            properties: {
              label: {
                control: 'string-input',
              },
              background: {
                control: 'color',
              },
              visible: {
                control: 'checkbox',
                defaultValue: true,
              },
            },
            variants: [
              {
                code: '<Card />',
                label: 'Card',
              },
              {
                code: '<Card person={DefaultPerson} />',
                label: 'ID Card',
                additionalImports:
                  "import { DefaultPerson } from '/src/defaults';",
              },
            ],
          },
        },
        '/src/card2': {
          Card2: {
            supportsChildren: false,
            properties: {
              label: {
                control: 'string-input',
              },
            },
            variants: [
              {
                code: '<Card2 />',
                label: 'Card2',
              },
              {
                code: '<Card2 label={DefaultLabel} />',
                label: 'ID Card',
                additionalImports:
                  "import { DefaultLabel } from '/src/defaults';",
              },
            ],
          },
        },
      }
      
      export default Components      
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(Object.keys(editorState.propertyControlsInfo['/src/card'])).toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    expect(Object.keys(editorState.propertyControlsInfo['/src/card2'])).toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
  it('registered controls for multiple modules from multiple sidecar files are in editor state', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `const Components = {
          '/src/card': {
            Card: {
              supportsChildren: false,
              properties: {
                label: {
                  control: 'string-input',
                },
                background: {
                  control: 'color',
                },
                visible: {
                  control: 'checkbox',
                  defaultValue: true,
                },
              },
              variants: [
                {
                  code: '<Card />',
                  label: 'Card',
                },
                {
                  code: '<Card person={DefaultPerson} />',
                  label: 'ID Card',
                  additionalImports:
                    "import { DefaultPerson } from '/src/defaults';",
                },
              ],
            },
          },
        }
        
        export default Components        
  `,
        ['/utopia/components2.utopia.js']: `const Components = {
          '/src/card2': {
            Card2: {
              supportsChildren: false,
              properties: {
                label: {
                  control: 'string-input',
                },
              },
              variants: [
                {
                  code: '<Card2 />',
                  label: 'Card2',
                },
                {
                  code: '<Card2 label={DefaultLabel} />',
                  label: 'ID Card',
                  additionalImports:
                    "import { DefaultLabel } from '/src/defaults';",
                },
              ],
            },
          },
        }
        
        export default Components
        `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(Object.keys(editorState.propertyControlsInfo['/src/card'])).toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    expect(Object.keys(editorState.propertyControlsInfo['/src/card2'])).toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
})
