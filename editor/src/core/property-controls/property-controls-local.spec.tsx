import { PrettierConfig } from 'utopia-vscode-common'
import { renderTestEditorWithCode, TestAppUID } from '../../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { TestScene0UID } from '../model/test-ui-js-file.test-utils'
import * as Prettier from 'prettier/standalone'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { dropFileExtension } from '../shared/file-utils'

describe('registered property controls', () => {
  it('registered controls are in editor state', async () => {
    const storyboardPath = dropFileExtension(StoryboardFilePath)
    const testCode = Prettier.format(
      `
        import * as React from 'react'
        import { Scene, Storyboard, View, registerComponent } from 'utopia-api'

        export var App = (props) => {
          return (
            <div>hello</div>
          )
        }

        registerComponent({
          name: 'App',
          moduleName: '${storyboardPath}', 
          controls: {
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
          insert: '<Card  />',
          requiredImports: 'import {Card} from "card"',
        })

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
    const editorState = renderResult.getEditorState().editor

    expect(editorState.propertyControlsInfo[storyboardPath]).toMatchInlineSnapshot(`
      Object {
        "App": Object {
          "componentInfo": Object {
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
              "card": Object {
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
          },
          "propertyControls": Object {
            "type": "RIGHT",
            "value": Object {
              "background": Object {
                "type": "RIGHT",
                "value": Object {
                  "control": "color",
                },
              },
              "label": Object {
                "type": "RIGHT",
                "value": Object {
                  "control": "string-input",
                },
              },
              "visible": Object {
                "type": "RIGHT",
                "value": Object {
                  "control": "checkbox",
                  "defaultValue": true,
                },
              },
            },
          },
        },
      }
    `)
  })
})
