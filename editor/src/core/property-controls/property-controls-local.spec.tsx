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
        import {
          Scene,
          Storyboard,
          View,
          registerModule,
        } from 'utopia-api'
        
        export var App = (props) => {
          return <div>hello</div>
        }
        
        export var Card = ({ person }) => (
          <h1>Hello, {person ?? 'John Doe'}</h1>
        )
        
        registerModule('/src/storyboard.js', {
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
        })
        
        export var storyboard = (props) => {
          return (
            <Storyboard>
              <Scene
                style={{
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 400,
                  height: 400,
                }}
              >
                <App />
                <Card
                  style={{
                    position: 'absolute',
                    left: 100,
                    top: 148,
                    width: 107,
                    height: 52,
                  }}
                />
              </Scene>
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )

    const renderResult = await renderTestEditorWithCode(testCode, 'dont-await-first-dom-report')
    await wait(10) // this is quite ugly but we want to wait for a timeout(0) in ui-jsx-canvas before calling validateControlsToCheck
    const editorState = renderResult.getEditorState().editor

    expect(editorState.propertyControlsInfo['/src/storyboard.js']).toMatchInlineSnapshot(`
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
          "supportsChildren": false,
          "variants": Array [
            Object {
              "elementToInsert": [Function],
              "importsToAdd": Object {
                "/src/storyboard.js": Object {
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
                "/src/storyboard.js": Object {
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
              "insertMenuLabel": "ID Card",
            },
          ],
        },
      }
    `)
  })
})
