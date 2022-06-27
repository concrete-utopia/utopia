import { PrettierConfig } from 'utopia-vscode-common'
import { renderTestEditorWithCode, TestAppUID } from '../../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { TestScene0UID } from '../model/test-ui-js-file.test-utils'
import * as Prettier from 'prettier/standalone'
import { wait } from '../model/performance-scripts'

describe('registered property controls', () => {
  it('registered controls are in editor state', async () => {
    const testCode = Prettier.format(
      `
        import * as React from 'react'
        import { Scene, Storyboard, View, registerModule } from 'utopia-api'

        export var App = (props) => {
          return (
            <div>hello</div>
          )
        }

        registerModule(
          '/src/card',
          {
            Card: {
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
                  additionalImports: "import { DefaultPerson } from '/src/defaults';",
                },
              ],
            },
          }
        )

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
      PrettierConfig,
    )

    const renderResult = await renderTestEditorWithCode(testCode, 'dont-await-first-dom-report')
    await wait(10) // this is quite ugly but we want to wait for a timeout(0) in ui-jsx-canvas before calling validateControlsToCheck
    const editorState = renderResult.getEditorState().editor

    expect(editorState.propertyControlsInfo['/src/card']).toMatchInlineSnapshot(`
      Object {
        "Card": Object {
          "properties": Object {
            "background": Object {
              "control": "color",
            },
            "label": Object {
              "control": "string-input",
            },
            "visible": Object {
              "control": "checkbox",
            },
          },
          "variants": Array [
            Object {
              "elementToInsert": Object {
                "children": Array [],
                "name": Object {
                  "baseVariable": "Card",
                  "propertyPath": Object {
                    "propertyElements": Array [],
                  },
                },
                "props": Array [
                  Object {
                    "comments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "key": "data-uid",
                    "type": "JSX_ATTRIBUTES_ENTRY",
                    "value": Object {
                      "comments": Object {
                        "leadingComments": Array [],
                        "trailingComments": Array [],
                      },
                      "type": "ATTRIBUTE_VALUE",
                      "value": "fb0",
                    },
                  },
                ],
                "type": "JSX_ELEMENT",
                "uid": "fb0",
              },
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
              "elementToInsert": Object {
                "children": Array [],
                "name": Object {
                  "baseVariable": "Card",
                  "propertyPath": Object {
                    "propertyElements": Array [],
                  },
                },
                "props": Array [
                  Object {
                    "comments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "key": "person",
                    "type": "JSX_ATTRIBUTES_ENTRY",
                    "value": Object {
                      "definedElsewhere": Array [
                        "DefaultPerson",
                      ],
                      "elementsWithin": Object {},
                      "javascript": "DefaultPerson",
                      "sourceMap": Object {
                        "file": "code.tsx",
                        "mappings": "OAI2BA",
                        "names": Array [
                          "DefaultPerson",
                        ],
                        "sources": Array [
                          "code.tsx",
                        ],
                        "sourcesContent": Array [
                          "import { Card } from '/src/card'; import { DefaultPerson } from '/src/defaults';;

             function Utopia$$$Component(props) {
                return (
                  <Card person={DefaultPerson} />
                )
               }",
                        ],
                        "version": 3,
                      },
                      "transpiledJavascript": "return DefaultPerson;",
                      "type": "ATTRIBUTE_OTHER_JAVASCRIPT",
                      "uniqueID": "",
                    },
                  },
                  Object {
                    "comments": Object {
                      "leadingComments": Array [],
                      "trailingComments": Array [],
                    },
                    "key": "data-uid",
                    "type": "JSX_ATTRIBUTES_ENTRY",
                    "value": Object {
                      "comments": Object {
                        "leadingComments": Array [],
                        "trailingComments": Array [],
                      },
                      "type": "ATTRIBUTE_VALUE",
                      "value": "8c0",
                    },
                  },
                ],
                "type": "JSX_ELEMENT",
                "uid": "8c0",
              },
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
})
