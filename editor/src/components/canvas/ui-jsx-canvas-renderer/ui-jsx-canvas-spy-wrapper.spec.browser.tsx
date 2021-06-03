import { canvasPoint, point } from '../../../core/shared/math-utils'
import {
  ParsedTextFile,
  isParseFailure,
  textFile,
  textFileContents,
  RevisionsState,
} from '../../../core/shared/project-file-types'
import { emptySet } from '../../../core/shared/set-utils'
import * as EP from '../../../core/shared/element-path'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { defaultProject } from '../../../sample-projects/sample-project-utils'
import {
  wait,
  simplifiedMetadataMap,
  domWalkerMetadataToSimplifiedMetadataMap,
} from '../../../utils/utils.test-utils'
import { addFileToProjectContents } from '../../assets'
import { setFocusedElement } from '../../editor/actions/action-creators'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import CanvasActions from '../canvas-actions'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'

const exampleProject = `
import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";
import { View } from "utopia-api";
import { App } from "/src/app";

const HiElement = (props) => {
  return <div data-uid="other-hi-element-root">hi!</div>
}

const Button = (props) => {
  return <div data-uid="other-button-root">{props.children}</div>;
};
const Card = () => {
  return (
    <Button data-uid="other-button-instance">
      {[0, 1, 2].map(i => (
        <HiElement data-uid="other-hi-element" />
      ))}
    </Button>
  );
};
export var SameFileApp = (props) => {
  return (
    <div
      data-uid="other-app-root"
      style={{
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
      }}
    >
      <div data-uid="other-inner-div">
        <Card data-uid="other-card-instance" />
      </div>
    </div>
  );
};

export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene-1"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <SameFileApp data-uid="app" />
    </Scene>
    <Scene
      data-uid="scene-2"
      style={{ position: "absolute", left: 400, top: 0, width: 375, height: 812 }}
    >
      <App data-uid="app2" />
    </Scene>
  </Storyboard>
);
`

function createModifiedProject(modifiedFiles: { [filename: string]: string }) {
  const baseModel = defaultProject()

  const updatedProject = Object.keys(modifiedFiles).reduce((workingProject, modifiedFilename) => {
    const parsedFile = lintAndParse(
      modifiedFilename,
      modifiedFiles[modifiedFilename],
      null,
      emptySet(),
    ) as ParsedTextFile
    if (isParseFailure(parsedFile)) {
      fail('The test file parse failed')
    }

    const updatedProjectContents = addFileToProjectContents(
      workingProject.projectContents,
      modifiedFilename,
      textFile(
        textFileContents(modifiedFiles[modifiedFilename], parsedFile, RevisionsState.BothMatch),
        null,
        Date.now(),
      ),
    )

    return {
      ...baseModel,
      projectContents: updatedProjectContents,
    }
  }, baseModel)
  return renderTestEditorWithModel(updatedProject)
}

function createExampleProject() {
  return createModifiedProject({
    [StoryboardFilePath]: exampleProject,
    '/src/card.js': `import * as React from "react";
import { jsx } from "utopia-api";
const HiElement = (props) => {
  return <div data-uid="hi-element-root">hi!</div>
}

const Button = (props) => {
  return <div data-uid="button-root">{props.children}</div>;
};
export const Card = () => {
  return (
    <Button data-uid="button-instance">
      {[0, 1, 2].map(i => (
        <HiElement data-uid="hi-element" />
      ))}
    </Button>
  );
};`,
  })
}

describe('Spy Wrapper Template Path Tests', () => {
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await createExampleProject()

    await wait(20000)
    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)
  })

  it('a component instance is focused inside the main App component', async () => {
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-1', 'app'],
            ['other-app-root', 'other-inner-div', 'other-card-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)
  })

  it('a component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-1', 'app'],
            ['other-app-root', 'other-inner-div', 'other-card-instance'],
            ['other-button-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)
  })

  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-1', 'app'],
            ['other-app-root', 'other-inner-div', 'other-card-instance'],
            ['other-button-instance', 'other-hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:other-hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:other-hi-element-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:other-hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2",
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:other-hi-element-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:other-hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
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
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-2', 'app2'],
            ['app-outer-div', 'card-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
      }
    `)
  })

  it('a component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-2', 'app2'],
            ['app-outer-div', 'card-instance'],
            ['button-instance'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)
  })

  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-2', 'app2'],
            ['app-outer-div', 'card-instance'],
            ['button-instance', 'hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
      }
    `)
  })
})

describe('Spy Wrapper Multifile With Cyclic Dependencies', () => {
  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await createModifiedProject({
      [StoryboardFilePath]: exampleProject,

      '/src/hi.js': `
        import * as React from "react";
        import { HiValue } from "./card";
        export const HiElement = (props) => {
          return <div data-uid="hi-element-root">{HiValue}</div>
        }`,

      '/src/card.js': `
        import * as React from "react";
        import { HiElement } from "./hi";
        export const HiValue = "hi!"
        
        const Button = (props) => {
          return <div data-uid="button-root">{props.children}</div>;
        };
        export const Card = () => {
          return (
            <Button data-uid="button-instance">
              {[0, 1, 2].map(i => (
                <HiElement data-uid="hi-element" />
              ))}
            </Button>
          );
        };`,
    })

    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-2', 'app2'],
            ['app-outer-div', 'card-instance'],
            ['button-instance', 'hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = domWalkerMetadataToSimplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    expect(sanitizedSpyData).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedDomMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
      }
    `)

    expect(sanitizedFinalMetadata).toMatchInlineSnapshot(`
      Object {
        "storyboard": Object {
          "children": Array [
            "storyboard/scene-1",
            "storyboard/scene-2",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        "storyboard/scene-1": Object {
          "children": Array [
            "storyboard/scene-1/app",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app": Object {
          "children": Array [],
          "name": "SameFileApp",
          "rootElements": Array [
            "storyboard/scene-1/app:other-app-root",
          ],
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "children": Array [
            "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [],
        },
        "storyboard/scene-2": Object {
          "children": Array [
            "storyboard/scene-2/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2": Object {
          "children": Array [],
          "name": "App",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "children": Array [],
          "name": "Card",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2",
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [
            "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root",
          ],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
      }
    `)
  })
})
