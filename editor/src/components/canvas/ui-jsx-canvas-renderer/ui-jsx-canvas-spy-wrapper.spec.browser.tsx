import { bimapEither, foldEither, mapEither } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  getJSXElementNameAsString,
  getJSXElementNameNoPathName,
  isJSXElement,
  JSXMetadata,
} from '../../../core/shared/element-template'
import { canvasPoint, point } from '../../../core/shared/math-utils'
import { objectMap } from '../../../core/shared/object-utils'
import * as TP from '../../../core/shared/template-path'
import { setFocusedElement } from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
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

function extractTemplatePathStuffFromElementInstanceMetadata(metadata: JSXMetadata) {
  const sanitizedSpyData = objectMap((elementMetadata, key) => {
    const templatePathAsReportedBySpy = TP.toString(elementMetadata.templatePath)
    if (templatePathAsReportedBySpy !== key) {
      fail(`The reported template path should match what was used as key`)
    }

    return {
      name: foldEither(
        (name) => name,
        (element) =>
          isJSXElement(element) ? getJSXElementNameAsString(element.name) : 'not-jsx-element',
        elementMetadata.element,
      ),
      children: elementMetadata.children.map(TP.toString),
    }
  }, metadata.elements)
  return sanitizedSpyData
}

describe('Spy Wrapper Template Path Tests', () => {
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await renderTestEditorWithCode(exampleProject)

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [],
          "name": "Scene",
        },
        "storyboard/scene:app-root": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
        },
      }
    `)
  })

  it('a component instance is focused inside the main App component', async () => {
    const { dispatch, getEditorState } = await renderTestEditorWithCode(exampleProject)
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard', 'scene'],
            ['app-root', 'inner-div', 'card-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [],
          "name": "Scene",
        },
        "storyboard/scene:app-root": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element",
          ],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)
  })

  it('a component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await renderTestEditorWithCode(exampleProject)
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard', 'scene'],
            ['app-root', 'inner-div', 'card-instance'],
            ['button-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [],
          "name": "Scene",
        },
        "storyboard/scene:app-root": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element",
          ],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)
  })
})
