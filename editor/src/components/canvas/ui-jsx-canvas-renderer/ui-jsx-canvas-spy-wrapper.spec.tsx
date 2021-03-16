import { bimapEither, foldEither, mapEither } from '../../../core/shared/either'
import {
  getJSXElementNameAsString,
  getJSXElementNameNoPathName,
  isJSXElement,
} from '../../../core/shared/element-template'
import { objectMap } from '../../../core/shared/object-utils'
import * as TP from '../../../core/shared/template-path'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'

const exampleProject = `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
import { View } from 'utopia-api'

const Button = (props) => {
  return <div data-uid='button-root'>{props.children}</div>
}
const Card = () => {
  return (
    <Button data-uid='button-instance' >
      <div data-uid='hi-element'>hi!</div>
    </Button>
  )
}
export var App = (props) => {
  return (
    <div
      data-uid='app-root'
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF', position: 'relative' }}
    >
      <div data-uid='inner-div'>
        <Card
          data-uid='card-instance'
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 70,
            top: 387,
            width: 193,
            height: 221,
          }}
        />
      </div>
    </div>
  )
}
export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      data-uid='scene'
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)`

describe('Spy Wrapper Template Path Tests', () => {
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await renderTestEditorWithCode(exampleProject)

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
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
          "templatePathAsKey": ":storyboard",
          "templatePathAsReportedBySpy": ":storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [],
          "name": "Scene",
          "templatePathAsKey": ":storyboard/scene",
          "templatePathAsReportedBySpy": ":storyboard/scene",
        },
        "storyboard/scene:app-root": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
          "templatePathAsKey": "storyboard/scene:app-root",
          "templatePathAsReportedBySpy": "storyboard/scene:app-root",
        },
        "storyboard/scene:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance",
          ],
          "name": "div",
          "templatePathAsKey": "storyboard/scene:app-root/inner-div",
          "templatePathAsReportedBySpy": "storyboard/scene:app-root/inner-div",
        },
        "storyboard/scene:app-root/inner-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "templatePathAsKey": "storyboard/scene:app-root/inner-div/card-instance",
          "templatePathAsReportedBySpy": "storyboard/scene:app-root/inner-div/card-instance",
        },
      }
    `)
  })
})
