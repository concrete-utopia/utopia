import { bimapEither, foldEither, mapEither } from '../../../core/shared/either'
import {
  getJSXElementNameAsString,
  getJSXElementNameNoPathName,
  isJSXElement,
} from '../../../core/shared/element-template'
import { objectMap } from '../../../core/shared/object-utils'
import * as TP from '../../../core/shared/template-path'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'

describe('Spy Wrapper Tests', () => {
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await renderTestEditorWithCode(`
    /** @jsx jsx */
    import * as React from 'react'
    import { Scene, Storyboard, jsx } from 'utopia-api'
    import { View } from 'utopia-api'

    export var App = (props) => {
      return (
        <div
          data-uid='app-root'
          style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF', position: 'relative' }}
        >
          <div data-uid='inner-div'>

          </div>
        </div>
      )
    }
    export var storyboard = (
      <Storyboard data-uid='storyboard-uid'>
        <Scene
          data-uid='scene-uid'
          component={App}
          props={{}}
          style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
        />
      </Storyboard>
    )
  `)

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = objectMap((elementMetadata, key) => {
      return {
        templatePathAsKey: key,
        templatePathAsReportedBySpy: TP.toString(elementMetadata.templatePath),
        name: foldEither(
          (name) => name,
          (element) =>
            isJSXElement(element) ? getJSXElementNameAsString(element.name) : 'not-jsx-element',
          elementMetadata.element,
        ),
        children: elementMetadata.children.map(TP.toString),
      }
    }, spiedMetadata.elements)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        ":storyboard-uid": Object {
          "children": Array [
            ":storyboard-uid/scene-uid",
          ],
          "name": "Storyboard",
          "templatePathAsKey": ":storyboard-uid",
          "templatePathAsReportedBySpy": ":storyboard-uid",
        },
        ":storyboard-uid/scene-uid": Object {
          "children": Array [],
          "name": "Scene",
          "templatePathAsKey": ":storyboard-uid/scene-uid",
          "templatePathAsReportedBySpy": ":storyboard-uid/scene-uid",
        },
        "storyboard-uid/scene-uid:app-root": Object {
          "children": Array [
            "storyboard-uid/scene-uid:app-root/inner-div",
          ],
          "name": "div",
          "templatePathAsKey": "storyboard-uid/scene-uid:app-root",
          "templatePathAsReportedBySpy": "storyboard-uid/scene-uid:app-root",
        },
        "storyboard-uid/scene-uid:app-root/inner-div": Object {
          "children": Array [],
          "name": "div",
          "templatePathAsKey": "storyboard-uid/scene-uid:app-root/inner-div",
          "templatePathAsReportedBySpy": "storyboard-uid/scene-uid:app-root/inner-div",
        },
      }
    `)
  })
})
