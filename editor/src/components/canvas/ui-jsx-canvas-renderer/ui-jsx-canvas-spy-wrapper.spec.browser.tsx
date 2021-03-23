import { mapArrayToDictionary } from '../../../core/shared/array-utils'
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
import { defaultProjectContentsForNormalising } from '../../custom-code/code-file.test-utils'
import { setFocusedElement } from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
import { renderTestEditorWithCode, renderTestEditorWithProjectContent } from '../ui-jsx.test-utils'

const exampleProject = `/** @jsx jsx */
import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";
import { View } from "utopia-api";

const HiElement = (props) => {
  return <div data-uid="hi-element-root">hi!</div>
}

const Button = (props) => {
  return <div data-uid="button-root">{props.children}</div>;
};
const Card = () => {
  return (
    <Button data-uid="button-instance">
      {[0, 1, 2].map(i => (
        <HiElement data-uid="hi-element" />
      ))}
    </Button>
  );
};
export var App = (props) => {
  return (
    <div
      data-uid="app-root"
      style={{
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
      }}
    >
      <div data-uid="inner-div">
        <Card data-uid="card-instance" />
        
      </div>
    </div>
  );
};

export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene"
      component={App}
      props={{}}
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
);
`

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

function extractTemplatePathStuffFromDomWalkerMetadata(metadata: Array<ElementInstanceMetadata>) {
  return mapArrayToDictionary(
    metadata,
    (elementMetadata: ElementInstanceMetadata) => TP.toString(elementMetadata.templatePath),
    (elementMetadata: ElementInstanceMetadata) => {
      return {
        name: foldEither(
          (name) => name,
          (element) =>
            isJSXElement(element) ? getJSXElementNameAsString(element.name) : 'not-jsx-element',
          elementMetadata.element,
        ),
        children: elementMetadata.children.map(TP.toString),
      }
    },
  )
}

describe('Spy Wrapper Template Path Tests', () => {
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await renderTestEditorWithCode(exampleProject)

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadataKILLME
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadataKILLME
    const sanitizedFinalMetadata = extractTemplatePathStuffFromElementInstanceMetadata(
      finalMetadata,
    )

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

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
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
          "name": "div",
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
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

    const domMetadata = getEditorState().editor.domMetadataKILLME
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadataKILLME
    const sanitizedFinalMetadata = extractTemplatePathStuffFromElementInstanceMetadata(
      finalMetadata,
    )

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
          "children": Array [],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
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
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
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
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
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

    const domMetadata = getEditorState().editor.domMetadataKILLME
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadataKILLME
    const sanitizedFinalMetadata = extractTemplatePathStuffFromElementInstanceMetadata(
      finalMetadata,
    )

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
          "children": Array [],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
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
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
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
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)
  })

  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await renderTestEditorWithCode(exampleProject)
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard', 'scene'],
            ['app-root', 'inner-div', 'card-instance'],
            ['button-instance', 'hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadataKILLME
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadataKILLME
    const sanitizedFinalMetadata = extractTemplatePathStuffFromElementInstanceMetadata(
      finalMetadata,
    )

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
          "children": Array [],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
          "name": "div",
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
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [
            ":storyboard/scene",
          ],
          "name": "Storyboard",
        },
        ":storyboard/scene": Object {
          "children": Array [
            "storyboard/scene:app-root",
            "storyboard/scene:app-root/inner-div",
          ],
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
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard/scene:app-root/inner-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
        },
      }
    `)
  })
})

describe('Spy Wrapper Multifile Template Path Tests', () => {
  it('the Card instance is focused inside the main App component', async () => {
    const { dispatch, getEditorState } = await renderTestEditorWithProjectContent(
      defaultProjectContentsForNormalising(),
    )
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard-entity', 'scene-1-entity'],
            ['app-outer-div', 'card-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadataKILLME
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadataKILLME
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadataKILLME
    const sanitizedFinalMetadata = extractTemplatePathStuffFromElementInstanceMetadata(
      finalMetadata,
    )

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        ":storyboard-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity",
          ],
          "name": "Storyboard",
        },
        ":storyboard-entity/scene-1-entity": Object {
          "children": Array [],
          "name": "Scene",
        },
        "storyboard-entity/scene-1-entity:app-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance",
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div",
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle"
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle": Object {
          "children": Array [],
          "name": "Rectangle",
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity",
          ],
          "name": "div",
        },
        ":storyboard-entity/scene-1-entity": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div"
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance",
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div",
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle"
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle": Object {
          "children": Array [],
          "name": "div",
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity",
          ],
          "name": "Storyboard",
        },
        ":storyboard-entity/scene-1-entity": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div"
          ],
          "name": "Scene",
        },
        "storyboard-entity/scene-1-entity:app-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance",
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div",
            "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle"
          ],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-div": Object {
          "children": Array [],
          "name": "div",
        },
        "storyboard-entity/scene-1-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle": Object {
          "children": Array [],
          "name": "Rectangle",
        },
      }
    `)
  })
})
