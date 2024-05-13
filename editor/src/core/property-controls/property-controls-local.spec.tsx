import { renderTestEditorWithModel, TestAppUID } from '../../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { TestScene0UID } from '../model/test-ui-js-file.test-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { deleteFile, updateFilePath } from '../../components/editor/actions/action-creators'
import { updateFromCodeEditor } from '../../components/editor/actions/actions-from-vscode'
import { pick } from '../shared/object-utils'

const project = (componentDescriptorFiles: { [filename: string]: string }) =>
  createModifiedProject({
    ['/src/card.js']: `import React from 'react'

    export const Card = ({ label }) => {
      return <div>{label}</div>
    }

    export const NewCard = ({ label }) => {
      return <h1>{label}</h1>
    }

    export const CardToDelete = ({ label }) => {
      return <code>{label}</code>
    }
    `,
    ['/src/card2.js']: `import React from 'react'
    
    export const Card2 = ({ label }) => {
      return <div>{label}</div>
    }
    `,
    ['/src/new-module.js']: `import React from 'react'
    
    export const NewCard = ({ label }) => {
      return <div>{label}</div>
    }
    `,
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
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
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
          focus: 'default',
          inspector: ['visual', 'typography'],
          emphasis: 'regular',
          icon: 'component',
          variants: [
            {
              code: '<Card />',
              imports: 'import { Card } from "/src/card"',
              label: 'Card',
            },
            {
              code: '<Card person={DefaultPerson} />',
              label: 'ID Card',
              imports: [
                'import { Card } from "/src/card"',
                "import { DefaultPerson } from '/src/defaults';",
              ],
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
          "emphasis": "regular",
          "focus": "default",
          "icon": "component",
          "inspector": Array [
            "visual",
            "typography",
          ],
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
          "supportsChildren": true,
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
  it('Can set property control options using the control factory functions', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        import * as Utopia from 'utopia-api'
        
        const Components = {
          '/src/card': {
            Card: {
              component: Card,
              properties: {
                label: Utopia.stringControl('type here', {
                  required: true,
                  defaultValue: 'hello',
                }),
              },
              variants: [],
              inspector: 'all',
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
          "emphasis": "regular",
          "focus": "default",
          "icon": "component",
          "inspector": "all",
          "preferredChildComponents": Array [],
          "properties": Object {
            "label": Object {
              "control": "string-input",
              "defaultValue": "hello",
              "placeholder": "type here",
              "required": true,
            },
          },
          "source": Object {
            "sourceDescriptorFile": "/utopia/components.utopia.js",
            "type": "DESCRIPTOR_FILE",
          },
          "supportsChildren": true,
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
          ],
        },
      }
    `)
  })
  it('control registration fails when the imported component is undefined', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Cart } from '../src/card'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Cart,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message: "Validation failed: Component registered for key 'Card' is undefined",
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when there is an evaluation error', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Cart } from '../src/card'
        
        const foo = undefined.foo
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: `  1 | import { Cart } from '../src/card'
  2 | 
> 3 | const foo = undefined.foo
                            ^
  4 | const Components = {
  5 |   '/src/card': {
  6 |     Card: {`,
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message: "TypeError: Cannot read properties of undefined (reading 'foo')",
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: 25,
            startLine: 4,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when there is an unknown prop in the component descriptor', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
          inpector: 'all',
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message: 'Malformed component registration: Card: Found unknown props: inpector',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when there is an invalid schema error', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
          supportsChildren: false,
          properties: {
            label: 'foo',
          },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message: 'Malformed component registration: Card.properties.label: Not an object.',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when the imported internal component does not match the name of registration key', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Cart: {
          component: Card,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message:
              'Validation failed: Component name (Card) does not match the registration key (Cart)',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when the module name of an imported internal component does not match the name of the registration key', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
      '/src/cardd': {
        Card: {
          component: Card,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message:
              'Validation failed: Module name (/src/card) does not match the module key (/src/cardd)',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when the imported external component does not match the name of registration key', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { View } from 'utopia-api'
        
        const Components = {
      'utopia-api': {
        Vieww: {
          component: View,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message:
              'Validation failed: Component name (View) does not match the registration key (Vieww)',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('control registration fails when the module name of an imported external component does not match the name of registration key', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { View } from 'utopia-api'
        
        const Components = {
      'utopia-apii': {
        View: {
          component: View,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )
    const editorState = renderResult.getEditorState().editor

    expect(editorState.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message:
              'Validation failed: Module name (utopia-api) does not match the module key (utopia-apii)',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()
  })
  it('updating the control registration removes the build errors', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Cart: {
          component: Card,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )

    expect(renderResult.getEditorState().editor.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [
          {
            codeSnippet: '',
            endColumn: null,
            endLine: null,
            errorCode: '',
            fileName: '/utopia/components.utopia.js',
            message:
              'Validation failed: Component name (Card) does not match the registration key (Cart)',
            passTime: null,
            severity: 'fatal',
            source: 'component-descriptor',
            startColumn: null,
            startLine: null,
            type: '',
          },
        ],
      },
    })

    const srcCardKey = Object.keys(renderResult.getEditorState().editor.propertyControlsInfo).find(
      (key) => key === '/src/card',
    )

    expect(srcCardKey).toBeUndefined()

    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          '/utopia/components.utopia.js',
          `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Cart: {
          component: Card,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
          `import { Card } from '../src/card'
              
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: { },
          variants: [ ],
        },
      },
      }

      export default Components
      `,
        ),
      ],
      true,
    )

    expect(renderResult.getEditorState().editor.codeEditorErrors).toEqual({
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {
        '/utopia/components.utopia.js': [],
      },
    })

    expect(Object.keys(renderResult.getEditorState().editor.propertyControlsInfo)).toEqual([
      'utopia-api',
      '@remix-run/react',
      '/src/card',
    ])
  })
  it('can use imports in the sidecar file', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        import * as Utopia from 'utopia-api'
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: Utopia.stringControl(),
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
              imports: 'import { Card } from "/src/card"',
              label: 'Card',
            },
            {
              code: '<Card person={DefaultPerson} />',
              label: 'ID Card',
              imports: [
                'import { Card } from "/src/card"',
                "import { DefaultPerson } from '/src/defaults';",
              ],
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
          "emphasis": "regular",
          "focus": "default",
          "icon": "component",
          "inspector": Array [],
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
          "supportsChildren": true,
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
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        import { Card2 } from '../src/card2'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
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
              imports: 'import { Card } from "/src/card"',
              label: 'Card',
            },
            {
              code: '<Card person={DefaultPerson} />',
              label: 'ID Card',
              imports: [
                'import { Card } from "/src/card"',
                "import { DefaultLabel } from '/src/defaults';",
              ],
            },
          ],
        },
        Card2: {
          component: Card2,
          properties: {
            label: {
              control: 'string-input',
            },
          },
          variants: [
            {
              code: '<Card2 />',
              imports: 'import { Card2 } from "/src/card2"',
              label: 'Card2',
            },
            {
              code: '<Card2 label={DefaultLabel} />',
              label: 'ID Card',
              imports: [
                'import { Card2 } from "/src/card2"',
                "import { DefaultLabel } from '/src/defaults';",
              ],
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
  })
  it('registered controls for multiple modules from sidecar file are in editor state', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        import { Card2 } from '../src/card2'

        const Components = {
        '/src/card': {
          Card: {
            component: Card,
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
                imports: 'import { Card } from "/src/card"',
                label: 'Card',
              },
              {
                code: '<Card person={DefaultPerson} />',
                label: 'ID Card',
                imports: [
                  'import { Card } from "/src/card"',
                  "import { DefaultLabel } from '/src/defaults';",
                ],
              },
            ],
          },
        },
        '/src/card2': {
          Card2: {
            component: Card2,
            properties: {
              label: {
                control: 'string-input',
              },
            },
            variants: [
              {
                code: '<Card2 />',
                imports: 'import { Card2 } from "/src/card2"',
                label: 'Card2',
              },
              {
                code: '<Card2 label={DefaultLabel} />',
                label: 'ID Card',
                imports: [
                  'import { Card2 } from "/src/card2"',
                  "import { DefaultLabel } from '/src/defaults';",
                ],
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
        ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
        const Components = {
          '/src/card': {
            Card: {
              component: Card,
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
                  imports: 'import { Card } from "/src/card"',
                  label: 'Card',
                },
                {
                  code: '<Card person={DefaultPerson} />',
                  label: 'ID Card',
                  imports: [
                    'import { Card } from "/src/card"',
                    "import { DefaultPerson } from '/src/defaults';",
                  ]
                },
              ],
            },
          },
        }
        
        export default Components        
  `,
        ['/utopia/components2.utopia.js']: `import { Card2 } from '../src/card2'
        const Components = {
          '/src/card2': {
            Card2: {
              component: Card2,
              properties: {
                label: {
                  control: 'string-input',
                },
              },
              variants: [
                {
                  code: '<Card2 />',
                  imports: 'import { Card2 } from "/src/card"',
                  label: 'Card2',
                },
                {
                  code: '<Card2 label={DefaultLabel} />',
                  label: 'ID Card',
                  imports: [
                    'import { Card2 } from "/src/card2"',
                    "import { DefaultLabel } from '/src/defaults';",
                  ],
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

  describe('variants', () => {
    it('generates an empty variant if the variants prop is omitted', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['variants'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
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
          ],
        }
      `)
    })
  })

  describe('registering the children prop', () => {
    it('can omit the children prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['preferredChildComponents', 'supportsChildren'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "preferredChildComponents": Array [],
          "supportsChildren": true,
        }
      `)
    })

    it('can use `supported` for the children prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { },
            children: 'supported'
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['preferredChildComponents', 'supportsChildren'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "preferredChildComponents": Array [],
          "supportsChildren": true,
        }
      `)
    })

    it('can use `not-supported` for the children prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { },
            children: 'not-supported'
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['preferredChildComponents', 'supportsChildren'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "preferredChildComponents": Array [],
          "supportsChildren": false,
        }
      `)
    })

    it('can specify text-only children', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { },
            children: {
              preferredContents: 'text'
            }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['preferredChildComponents', 'supportsChildren'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "preferredChildComponents": Array [
            Object {
              "moduleName": null,
              "name": "Card",
              "variants": Array [
                Object {
                  "elementToInsert": [Function],
                  "importsToAdd": Object {},
                  "insertMenuLabel": "text",
                },
              ],
            },
          ],
          "supportsChildren": true,
        }
      `)
    })

    it('can use placeholders and examples for children', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          import { Card2 } from '../src/card2'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { },
            children: {
              preferredContents: [
                { component: 'span', variants: { name: 'span' } },
                {
                  component: 'Card2',
                  moduleName: '/src/card2',
                  variants: [
                    { component: Card2 },
                    {
                      code: '<Card2 label={DefaultLabel} />',
                      label: 'ID Card',
                      imports: [
                        'import { Card2 } from "/src/card2"',
                        "import { DefaultLabel } from '/src/defaults';",
                      ],
                    }
                  ]
                },
              ]
            }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['preferredChildComponents', 'supportsChildren'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "preferredChildComponents": Array [
            Object {
              "moduleName": null,
              "name": "span",
              "variants": Array [
                Object {
                  "elementToInsert": [Function],
                  "importsToAdd": Object {},
                  "insertMenuLabel": "span",
                },
              ],
            },
            Object {
              "moduleName": "/src/card2",
              "name": "Card2",
              "variants": Array [
                Object {
                  "elementToInsert": [Function],
                  "importsToAdd": Object {
                    "/src/card2.js": Object {
                      "importedAs": null,
                      "importedFromWithin": Array [
                        Object {
                          "alias": "Card2",
                          "name": "Card2",
                        },
                      ],
                      "importedWithName": null,
                    },
                  },
                  "insertMenuLabel": "Card2",
                },
                Object {
                  "elementToInsert": [Function],
                  "importsToAdd": Object {
                    "/src/card2": Object {
                      "importedAs": null,
                      "importedFromWithin": Array [
                        Object {
                          "alias": "Card2",
                          "name": "Card2",
                        },
                      ],
                      "importedWithName": null,
                    },
                    "/src/defaults": Object {
                      "importedAs": null,
                      "importedFromWithin": Array [
                        Object {
                          "alias": "DefaultLabel",
                          "name": "DefaultLabel",
                        },
                      ],
                      "importedWithName": null,
                    },
                  },
                  "insertMenuLabel": "ID Card",
                },
              ],
            },
          ],
          "supportsChildren": true,
        }
      `)
    })
    it('can use conditional for children', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          import { Card2 } from '../src/card2'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: { },
            children: {
              preferredContents: [
                { component: 'span', variants: { name: 'span' } },
                {
                  component: 'Conditional',
                  moduleName: '',
                  variants: [
                    {
                      code: 'true ? <div /> : null',
                      label: 'Conditional',
                      imports: '',
                    }
                  ]
                },
              ]
            }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['preferredChildComponents', 'supportsChildren'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "preferredChildComponents": Array [
            Object {
              "moduleName": null,
              "name": "span",
              "variants": Array [
                Object {
                  "elementToInsert": [Function],
                  "importsToAdd": Object {},
                  "insertMenuLabel": "span",
                },
              ],
            },
            Object {
              "moduleName": "",
              "name": "Conditional",
              "variants": Array [
                Object {
                  "elementToInsert": [Function],
                  "importsToAdd": Object {},
                  "insertMenuLabel": "Conditional",
                },
              ],
            },
          ],
          "supportsChildren": true,
        }
      `)

      expect(propsToCheck.preferredChildComponents[1].variants[0].elementToInsert().type).toEqual(
        'JSX_CONDITIONAL_EXPRESSION',
      )
    })
  })

  describe('registering the jsx controls', () => {
    it('can register placeholder', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: {
              label: {
                control: 'jsx',
              },
              header: {
                control: 'jsx',
              },
              footer: {
                control: 'jsx',
              }
            }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['properties'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "properties": Object {
            "footer": Object {
              "control": "jsx",
              "preferredChildComponents": Array [],
            },
            "header": Object {
              "control": "jsx",
              "preferredChildComponents": Array [],
            },
            "label": Object {
              "control": "jsx",
              "preferredChildComponents": Array [],
            },
          },
        }
      `)
    })

    it('can use examples', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
      import { Card2 } from '../src/card2'
      
      const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: {
              label: {
                control: 'jsx',
                preferredContents: [
                  { component: 'span', variants: { name: 'span' } },
                  {
                    component: 'Card2',
                    moduleName: '/src/card2',
                    variants: [
                      { component: Card2 },
                      {
                        code: '<Card2 label={DefaultLabel} />',
                        label: 'ID Card',
                        imports: [
                          'import { Card2 } from "/src/card2"',
                          "import { DefaultLabel } from '/src/defaults';",
                        ],
                      }
                    ]
                  },
                ],
              },
            },
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
            "emphasis": "regular",
            "focus": "default",
            "icon": "component",
            "inspector": Array [],
            "preferredChildComponents": Array [],
            "properties": Object {
              "label": Object {
                "control": "jsx",
                "preferredChildComponents": Array [
                  Object {
                    "moduleName": null,
                    "name": "span",
                    "variants": Array [
                      Object {
                        "elementToInsert": [Function],
                        "importsToAdd": Object {},
                        "insertMenuLabel": "span",
                      },
                    ],
                  },
                  Object {
                    "moduleName": "/src/card2",
                    "name": "Card2",
                    "variants": Array [
                      Object {
                        "elementToInsert": [Function],
                        "importsToAdd": Object {
                          "/src/card2.js": Object {
                            "importedAs": null,
                            "importedFromWithin": Array [
                              Object {
                                "alias": "Card2",
                                "name": "Card2",
                              },
                            ],
                            "importedWithName": null,
                          },
                        },
                        "insertMenuLabel": "Card2",
                      },
                      Object {
                        "elementToInsert": [Function],
                        "importsToAdd": Object {
                          "/src/card2": Object {
                            "importedAs": null,
                            "importedFromWithin": Array [
                              Object {
                                "alias": "Card2",
                                "name": "Card2",
                              },
                            ],
                            "importedWithName": null,
                          },
                          "/src/defaults": Object {
                            "importedAs": null,
                            "importedFromWithin": Array [
                              Object {
                                "alias": "DefaultLabel",
                                "name": "DefaultLabel",
                              },
                            ],
                            "importedWithName": null,
                          },
                        },
                        "insertMenuLabel": "ID Card",
                      },
                    ],
                  },
                ],
                "preferredContents": Array [
                  Object {
                    "component": "span",
                    "variants": Object {
                      "name": "span",
                    },
                  },
                  Object {
                    "component": "Card2",
                    "moduleName": "/src/card2",
                    "variants": Array [
                      Object {
                        "component": [Function],
                      },
                      Object {
                        "code": "<Card2 label={DefaultLabel} />",
                        "imports": Array [
                          "import { Card2 } from \\"/src/card2\\"",
                          "import { DefaultLabel } from '/src/defaults';",
                        ],
                        "label": "ID Card",
                      },
                    ],
                  },
                ],
              },
            },
            "source": Object {
              "sourceDescriptorFile": "/utopia/components.utopia.js",
              "type": "DESCRIPTOR_FILE",
            },
            "supportsChildren": true,
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
            ],
          },
        }
      `)
    })
  })

  describe('folderes', () => {
    it('can specify a folder prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
          
          const Components = {
        '/src/card': {
          Card: {
            component: Card,
            properties: {
              label: {
                control: 'jsx',
              },
              header: {
                control: 'jsx',
                folder: 'Header',
              },
              footer: {
                control: 'jsx',
                folder: 'Footer',
              }
            }
          },
        },
      }
      
      export default Components
    `,
        }),
        'await-first-dom-report',
      )
      const editorState = renderResult.getEditorState().editor

      const cardRegistration = editorState.propertyControlsInfo['/src/card']['Card']
      expect(cardRegistration).not.toBeUndefined()

      const propsToCheck = pick(['properties'], cardRegistration)

      expect(propsToCheck).toMatchInlineSnapshot(`
        Object {
          "properties": Object {
            "footer": Object {
              "control": "jsx",
              "folder": "Footer",
              "preferredChildComponents": Array [],
            },
            "header": Object {
              "control": "jsx",
              "folder": "Header",
              "preferredChildComponents": Array [],
            },
            "label": Object {
              "control": "jsx",
              "preferredChildComponents": Array [],
            },
          },
        }
      `)
    })
  })
})

describe('Lifecycle management of registering components', () => {
  const descriptorFileName1 = '/utopia/components1.utopia.js'
  const descriptorFileName2 = '/utopia/components2.utopia.js'
  describe('Deleting a component descriptor file', () => {
    it('Deleting a component descriptor file removes the property controls from that file', async () => {
      const renderResult = await renderTestEditorWithModel(
        project({
          [descriptorFileName1]: `import { Card } from '../src/card'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
        },
      },
    }
    
    export default Components
  `,
          [descriptorFileName2]: `import { Card2 } from '../src/card2'

          const Components = {
    '/src/card2': {
      Card2: {
        component: Card2,
        properties: {
          label: {
            control: 'string-input',
          },
        },
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
  })
  it('Deleting a component descriptor file removes the error messages of that file', async () => {
    const renderResult = await renderTestEditorWithModel(
      project({
        ['/utopia/components.utopia.js']: `import { Cart } from '../src/card'
        
        const Components = {
      '/src/card': {
        Card: {
          component: Cart,
          properties: { },
          variants: [ ],
        },
      },
    }
    
    export default Components
  `,
      }),
      'await-first-dom-report',
    )

    // Error messages from the descriptor file
    expect(
      renderResult.getEditorState().editor.codeEditorErrors.componentDescriptorErrors[
        '/utopia/components.utopia.js'
      ],
    ).toEqual([
      {
        codeSnippet: '',
        endColumn: null,
        endLine: null,
        errorCode: '',
        fileName: '/utopia/components.utopia.js',
        message: "Validation failed: Component registered for key 'Card' is undefined",
        passTime: null,
        severity: 'fatal',
        source: 'component-descriptor',
        startColumn: null,
        startLine: null,
        type: '',
      },
    ])

    // delete the descriptor file
    await renderResult.dispatch([deleteFile('/utopia/components.utopia.js')], true)

    // error messages are removed
    expect(
      renderResult.getEditorState().editor.codeEditorErrors.componentDescriptorErrors[
        '/utopia/components.utopia.js'
      ] ?? [],
    ).toHaveLength(0)
  })
  describe('Updating a component descriptor file', () => {
    const descriptorFileContent2 = `import { Card2 } from '../src/card2'
    
    const Components = {
      '/src/card2': {
        Card2: {
          component: Card2,
          properties: {
            label: {
              control: 'string-input',
            },
          },
        },
      },
    }
    
    export default Components
  `

    it('Updating a component in a component descriptor file updates the property controls of that component', async () => {
      const descriptorFileContent1 = `import { Card } from '../src/card'

      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
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
          renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']['Card2']
            .properties,
        ),
      ).toMatchInlineSnapshot(`
              Array [
                "label",
              ]
          `)

      const updatedDescriptorFileContent = `import { Card } from '../src/card'

      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
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
          renderResult.getEditorState().editor.propertyControlsInfo['/src/card2']['Card2']
            .properties,
        ),
      ).toMatchInlineSnapshot(`
              Array [
                "label",
              ]
          `)
    })
    it('Adding a new component in a component descriptor file adds its property controls', async () => {
      const descriptorFileContent1 = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
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

      const updatedDescriptorFileContent = `import { Card, NewCard } from '../src/card'
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
        },
        NewCard: {
          component: NewCard,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
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
      const descriptorFileContent1 = `import { Card, CardToDelete } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
        },
        CardToDelete: {
          component: CardToDelete,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
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

      const updatedDescriptorFileContent = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
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
      const descriptorFileContent1 = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
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

      const updatedDescriptorFileContent = `import { Card } from '../src/card'
      import { NewCard } from '../src/new-module'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
        },
      },
      '/src/new-module': {
        NewCard: {
          component: NewCard,
          properties: {
            label: {
              control: 'string-input',
            },
          },
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
    it('Deleting a module from a component descriptor file removes it from property controls', async () => {
      const descriptorFileContent1 = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
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

      const updatedDescriptorFileContent = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label2: {
              control: 'string-input',
            },
          },
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
  describe('Renaming a component descriptor file', () => {
    it('Renaming a component descriptor file to a different name updates the property controls to the new source file', async () => {
      const descriptorFileContent1 = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
        },
      },
    }
    
    export default Components
  `

      const renderResult = await renderTestEditorWithModel(
        project({
          [descriptorFileName1]: descriptorFileContent1,
        }),
        'await-first-dom-report',
      )

      // The Card property control is registered with the correct source descriptor file name
      expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].source)
        .toMatchInlineSnapshot(`
        Object {
          "sourceDescriptorFile": "/utopia/components1.utopia.js",
          "type": "DESCRIPTOR_FILE",
        }
      `)

      await renderResult.dispatch([updateFilePath(descriptorFileName1, descriptorFileName2)], true)

      // The Card property control is registered with the new descriptor file name
      expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].source)
        .toMatchInlineSnapshot(`
        Object {
          "sourceDescriptorFile": "/utopia/components2.utopia.js",
          "type": "DESCRIPTOR_FILE",
        }
      `)
    })
    it('Renaming a component descriptor file to a non-component-descriptor name removes the property controls', async () => {
      const descriptorFileContent1 = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
        },
      },
    }
    
    export default Components
  `

      const renderResult = await renderTestEditorWithModel(
        project({
          [descriptorFileName1]: descriptorFileContent1,
        }),
        'await-first-dom-report',
      )

      // The Card property control is registered with the correct source descriptor file name
      expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].source)
        .toMatchInlineSnapshot(`
        Object {
          "sourceDescriptorFile": "/utopia/components1.utopia.js",
          "type": "DESCRIPTOR_FILE",
        }
      `)

      await renderResult.dispatch([updateFilePath(descriptorFileName1, 'foo.js')], true)

      // The '/src/card' property controls are removed
      expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']).toBeUndefined()
    })
    it('Renaming a regular file to a component descriptor name adds the property controls', async () => {
      const descriptorFileContent1 = `import { Card } from '../src/card'
      
      const Components = {
      '/src/card': {
        Card: {
          component: Card,
          properties: {
            label: {
              control: 'string-input',
            },
          },
        },
      },
    }
    
    export default Components
  `

      const renderResult = await renderTestEditorWithModel(
        project({
          ['foo.js']: descriptorFileContent1,
        }),
        'await-first-dom-report',
      )

      // The '/src/card' property controls are not registered
      expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']).toBeUndefined()

      await renderResult.dispatch([updateFilePath('foo.js', descriptorFileName1)], true)

      // The Card property control is registered with the correct new source descriptor file name
      expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].source)
        .toMatchInlineSnapshot(`
        Object {
          "sourceDescriptorFile": "/utopia/components1.utopia.js",
          "type": "DESCRIPTOR_FILE",
        }
      `)
    })
  })
})
