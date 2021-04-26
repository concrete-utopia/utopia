import { mapArrayToDictionary } from '../../../core/shared/array-utils'
import { bimapEither, foldEither, mapEither } from '../../../core/shared/either'
import {
  ElementInstanceMetadata,
  getJSXElementNameAsString,
  getJSXElementNameNoPathName,
  isJSXElement,
  ElementInstanceMetadataMap,
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
        <Card data-uid="other-card-instance" />
        
      </div>
    </div>
  );
};

export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid="app" />
    </Scene>
  </Storyboard>
);
`

function extractTemplatePathStuffFromElement(elementMetadata: ElementInstanceMetadata) {
  return {
    name: foldEither(
      (name) => name,
      (element) =>
        isJSXElement(element) ? getJSXElementNameAsString(element.name) : 'not-jsx-element',
      elementMetadata.element,
    ),
    children: elementMetadata.children.map(TP.toString),
    rootElements: elementMetadata.rootElements.map(TP.toString),
  }
}

function extractTemplatePathStuffFromElementInstanceMetadata(metadata: ElementInstanceMetadataMap) {
  const sanitizedSpyData = objectMap((elementMetadata, key) => {
    const templatePathAsReportedBySpy = TP.toString(elementMetadata.templatePath)
    if (templatePathAsReportedBySpy !== key) {
      fail(`The reported template path should match what was used as key`)
    }

    return extractTemplatePathStuffFromElement(elementMetadata)
  }, metadata)
  return sanitizedSpyData
}

function extractTemplatePathStuffFromDomWalkerMetadata(metadata: Array<ElementInstanceMetadata>) {
  return mapArrayToDictionary(
    metadata,
    (elementMetadata: ElementInstanceMetadata) => TP.toString(elementMetadata.templatePath),
    extractTemplatePathStuffFromElement,
  )
}

describe('Spy Wrapper Template Path Tests', () => {
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await renderTestEditorWithCode(exampleProject)

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
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
            ['storyboard', 'scene', 'app'],
            ['app-root', 'inner-div', 'card-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
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
            ['storyboard', 'scene', 'app'],
            ['app-root', 'inner-div', 'card-instance'],
            ['button-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
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
            ['storyboard', 'scene', 'app'],
            ['app-root', 'inner-div', 'card-instance'],
            ['button-instance', 'hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
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
          "rootElements": Array [],
        },
        ":storyboard/scene": Object {
          "children": Array [
            ":storyboard/scene/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/scene/app": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene/app:app-root",
          ],
        },
        "storyboard/scene/app:app-root": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
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
            ['storyboard-entity', 'scene-1-entity', 'app-entity'],
            ['app-outer-div', 'card-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = extractTemplatePathStuffFromElementInstanceMetadata(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = extractTemplatePathStuffFromDomWalkerMetadata(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = extractTemplatePathStuffFromElementInstanceMetadata(
      finalMetadata,
    )

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        ":storyboard-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity",
            ":storyboard-entity/scene-2-entity",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-1-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity/app-entity",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-1-entity/app-entity": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-2-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-2-entity/same-file-app-entity",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-2-entity/same-file-app-entity": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div",
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle": Object {
          "children": Array [],
          "name": "Rectangle",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard-entity": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-1-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity/app-entity",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-1-entity/app-entity": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div",
          ],
        },
        ":storyboard-entity/scene-2-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-2-entity/same-file-app-entity",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-2-entity/same-file-app-entity": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div",
          ],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div",
          ],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div",
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        ":storyboard-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity",
            ":storyboard-entity/scene-2-entity",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-1-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-1-entity/app-entity",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-1-entity/app-entity": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div",
          ],
        },
        ":storyboard-entity/scene-2-entity": Object {
          "children": Array [
            ":storyboard-entity/scene-2-entity/same-file-app-entity",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard-entity/scene-2-entity/same-file-app-entity": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div",
          ],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div",
          ],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div": Object {
          "children": Array [
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div",
            "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-rectangle": Object {
          "children": Array [],
          "name": "Rectangle",
          "rootElements": Array [],
        },
        "storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)
  })
})
