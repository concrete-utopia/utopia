import { renderTestEditorWithModel, TestAppUID } from '../../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { TestScene0UID } from '../model/test-ui-js-file.test-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { deleteFile, updateFile } from '../../components/editor/actions/action-creators'
import { lintAndParse } from '../workers/parser-printer/parser-printer'
import { emptySet } from '../shared/set-utils'
import {
  codeFile,
  isParseSuccess,
  ParsedTextFile,
  RevisionsState,
  TextFile,
  textFile,
  textFileContents,
} from '../shared/project-file-types'
import { forceNotNull } from '../shared/optional-utils'
import { getProjectFileByFilePath } from '../../components/assets'
import { wait } from '../../utils/utils.test-utils'
import { updateFromCodeEditor } from '../../components/editor/actions/actions-from-vscode'

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
          "source": Object {
            "sourceDescriptorFile": "/utopia/components.utopia.js",
            "type": "DESCRIPTOR_FILE",
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

describe('Lifecycle management of registering components', () => {
  it('Deleting a component descriptor file removes the property controls from that file', async () => {
    const descriptorFileName1 = '/utopia/components1.utopia.js'
    const descriptorFileName2 = '/utopia/components2.utopia.js'
    const renderResult = await renderTestEditorWithModel(
      project({
        [descriptorFileName1]: `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components
  `,
        [descriptorFileName2]: `const Components = {
    '/src/card2': {
      Card2: {
        supportsChildren: false,
        properties: {
          label: {
            control: 'string-input',
          },
        },
        variants: [],
      },
    },
  }
  
  export default Components
`,
      }),
      'await-first-dom-report',
    )

    // Property controls from both descriptors files are there
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)

    // delete the first descriptor file
    await renderResult.dispatch([deleteFile(descriptorFileName1)], true)

    // property controls from the first descriptor file are gone
    expect(renderResult.getEditorState().editor.propertyControlsInfo).not.toContain('/src/card')
    // property controls from the second descriptor file are still there
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
  const descriptorFileName1 = '/utopia/components1.utopia.js'
  const descriptorFileName2 = '/utopia/components2.utopia.js'
  const descriptorFileContent2 = `const Components = {
    '/src/card2': {
      Card2: {
        supportsChildren: false,
        properties: {
          label: {
            control: 'string-input',
          },
        },
        variants: [],
      },
    },
  }
  
  export default Components
`

  it('Updating a component in a component descriptor file updates the property controls of that component', async () => {
    const descriptorFileContent1 = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components
  `

    const renderResult = await renderTestEditorWithModel(
      project({
        [descriptorFileName1]: descriptorFileContent1,
        [descriptorFileName2]: descriptorFileContent2,
      }),
      'await-first-dom-report',
    )

    // Card has a label property in the original file
    expect(
      Object.keys(
        renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].properties,
      ),
    ).toMatchInlineSnapshot(`
      Array [
        "label",
      ]
    `)
    // Just to check that the property controls from the second descriptor file are there
    expect(
      Object.keys(
        renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']['Card2'].properties,
      ),
    ).toMatchInlineSnapshot(`
      Array [
        "label",
      ]
    `)

    const updatedDescriptorFileContent = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }

    export default Components`

    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          descriptorFileName1,
          descriptorFileContent1,
          updatedDescriptorFileContent,
        ),
      ],
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished() // property controls from the first descriptor file are gone

    // Card has a label2 property in the updated file
    expect(
      Object.keys(
        renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].properties,
      ),
    ).toMatchInlineSnapshot(`
          Array [
            "label2",
          ]
      `)
    // Card2 has not been changed
    expect(
      Object.keys(
        renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']['Card2'].properties,
      ),
    ).toMatchInlineSnapshot(`
      Array [
        "label",
      ]
    `)
  })
  it('Adding a new component in a component descriptor file adds its property controls', async () => {
    const descriptorFileContent1 = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components
  `
    const renderResult = await renderTestEditorWithModel(
      project({
        [descriptorFileName1]: descriptorFileContent1,
        [descriptorFileName2]: descriptorFileContent2,
      }),
      'await-first-dom-report',
    )
    // The Card component is registered from the first descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    // The Card2 component is registered from the second descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)

    const updatedDescriptorFileContent = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
        NewCard: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }

    export default Components`

    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          descriptorFileName1,
          descriptorFileContent1,
          updatedDescriptorFileContent,
        ),
      ],
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished() // property controls from the first descriptor file are gone

    // The first descriptor file has a new NewCard component in it
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
        "NewCard",
      ]
    `)
    // The second descriptor file has not been changed
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
  it('Removing a component from a component descriptor file removes its property controls', async () => {
    const descriptorFileContent1 = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
        CardToDelete: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components
  `
    const renderResult = await renderTestEditorWithModel(
      project({
        [descriptorFileName1]: descriptorFileContent1,
        [descriptorFileName2]: descriptorFileContent2,
      }),
      'await-first-dom-report',
    )

    // The Card and CardToDelete component is registered from the first descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
        "CardToDelete",
      ]
    `)
    // The Card2 component is registered from the second descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)

    const updatedDescriptorFileContent = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }

    export default Components`

    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          descriptorFileName1,
          descriptorFileContent1,
          updatedDescriptorFileContent,
        ),
      ],
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished() // property controls from the first descriptor file are gone

    // The CardToDelete from the first descriptor file has been deleted
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    // The second descriptor file has not been changed
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
  it('Adding a new module to a component descriptor file adds its components', async () => {
    const descriptorFileContent1 = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components
  `
    const renderResult = await renderTestEditorWithModel(
      project({
        [descriptorFileName1]: descriptorFileContent1,
        [descriptorFileName2]: descriptorFileContent2,
      }),
      'await-first-dom-report',
    )

    // The Card component from /src/card is registered from the first descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    // /src/new-module is not registered yet
    expect(
      renderResult.getEditorState().editor.propertyControlsInfo['/src/new-module'],
    ).toBeUndefined()
    // The Card2 component is registered from the second descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)

    const updatedDescriptorFileContent = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
      '/src/new-module': {
        NewComp: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }

    export default Components`

    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          descriptorFileName1,
          descriptorFileContent1,
          updatedDescriptorFileContent,
        ),
      ],
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished() // property controls from the first descriptor file are gone

    // The Card from /src/card from the first descriptor is still there
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    // The NewComp from the newly added /src/new-module from the first descriptor is registered
    expect(
      Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/new-module']),
    ).toMatchInlineSnapshot(`
      Array [
        "NewComp",
      ]
    `)
    // The second descriptor file has not been changed
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
  it('Deleting a module from a component descriptor file removes it from property controls', async () => {
    const descriptorFileContent1 = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
      
    }
    
    export default Components
  `
    const renderResult = await renderTestEditorWithModel(
      project({
        [descriptorFileName1]: descriptorFileContent1,
        [descriptorFileName2]: descriptorFileContent2,
      }),
      'await-first-dom-report',
    )

    // The Card component from /src/card is registered from the first descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    // The Comp component from /src/module-to-delete is registered from the first descriptor file
    expect(
      renderResult.getEditorState().editor.propertyControlsInfo['/src/module-to-delete'],
    ).toBeUndefined()
    // The Card2 component is registered from the second descriptor file
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)

    const updatedDescriptorFileContent = `const Components = {
      '/src/card': {
        Card: {
          supportsChildren: false,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
          variants: [],
        },
      },
    }

    export default Components`

    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          descriptorFileName1,
          descriptorFileContent1,
          updatedDescriptorFileContent,
        ),
      ],
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished() // property controls from the first descriptor file are gone

    // The Card from /src/card from the first descriptor is still there
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']))
      .toMatchInlineSnapshot(`
      Array [
        "Card",
      ]
    `)
    // The /src/module-to-delete module is deleted
    expect(
      renderResult.getEditorState().editor.propertyControlsInfo['/src/new-module'],
    ).toBeUndefined()
    // The second descriptor file has not been changed
    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']))
      .toMatchInlineSnapshot(`
      Array [
        "Card2",
      ]
    `)
  })
})
