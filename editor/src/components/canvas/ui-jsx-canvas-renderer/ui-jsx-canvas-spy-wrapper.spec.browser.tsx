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
import {
  ParsedTextFile,
  isParseFailure,
  textFile,
  textFileContents,
  RevisionsState,
} from '../../../core/shared/project-file-types'
import * as TP from '../../../core/shared/template-path'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { defaultProject } from '../../../sample-projects/sample-project-utils'
import { wait } from '../../../utils/utils.test-utils'
import { addFileToProjectContents } from '../../assets'
import { defaultProjectContentsForNormalising } from '../../custom-code/code-file.test-utils'
import { setFocusedElement } from '../../editor/actions/action-creators'
import {
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../../editor/store/editor-state'
import CanvasActions from '../canvas-actions'
import {
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  renderTestEditorWithProjectContent,
} from '../ui-jsx.test-utils'

const exampleProject = `/** @jsx jsx */
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
      data-uid="scene"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <SameFileApp data-uid="app" />
    </Scene>
    <Scene
      data-uid="scene"
      style={{ position: "absolute", left: 400, top: 0, width: 375, height: 812 }}
    >
      <App data-uid="app2" />
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

function createExampleProject() {
  const baseModel = defaultProject()
  const modifiedFiles = {
    [StoryboardFilePath]: exampleProject,
    '/src/card.js': `import * as React from "react";
import { jsx } from "utopia-api";
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
};`,
  }

  const updatedProject = Object.keys(modifiedFiles).reduce((workingProject, modifiedFilename) => {
    const parsedFile = lintAndParse(
      modifiedFilename,
      modifiedFiles[modifiedFilename],
      null,
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

describe('Spy Wrapper Template Path Tests', () => {
  it('a simple component in a regular scene', async () => {
    // Code kept commented for any future person who needs it.
    const currentWindow = require('electron').remote.getCurrentWindow()
    currentWindow.show()
    currentWindow.setPosition(500, 200)
    currentWindow.setSize(2200, 1000)
    currentWindow.openDevTools()
    // This is necessary because the test code races against the Electron process
    // opening the window it would appear.
    await wait(20000)
    const { getEditorState } = await createExampleProject().catch(() => wait(60000))

    await wait(20000)
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard', 'scene', 'app'],
            ['app-root', 'inner-div', 'other-card-instance'],
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
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
          TP.scenePath([
            ['storyboard', 'scene', 'app'],
            ['app-root', 'inner-div', 'other-card-instance'],
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance:button-root": Object {
          "children": Array [],
          "name": "div",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance:button-root",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance:button-root": Object {
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance:button-root",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance:button-root": Object {
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
          TP.scenePath([
            ['storyboard', 'scene', 'app'],
            ['app-root', 'inner-div', 'other-card-instance'],
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2:hi-element-root",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance": Object {
          "children": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2",
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3",
          ],
          "name": "Button",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~1": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2": Object {
          "children": Array [],
          "name": "HiElement",
          "rootElements": Array [
            "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2:hi-element-root",
          ],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "children": Array [],
          "name": "div",
          "rootElements": Array [],
        },
        "storyboard/scene/app:app-root/inner-div/other-card-instance:button-instance/hi-element~~~3": Object {
          "children": Array [],
          "name": "HiElement",
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
          TP.scenePath([
            ['storyboard', 'scene', 'app2'],
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard', 'scene', 'app2'],
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
    const { dispatch, getEditorState } = await createExampleProject()
    await dispatch(
      [
        setFocusedElement(
          TP.scenePath([
            ['storyboard', 'scene', 'app2'],
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "div",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "div",
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
            ":storyboard/f51",
          ],
          "name": "Storyboard",
          "rootElements": Array [],
        },
        ":storyboard/f51": Object {
          "children": Array [
            ":storyboard/f51/app2",
          ],
          "name": "Scene",
          "rootElements": Array [],
        },
        ":storyboard/f51/app2": Object {
          "children": Array [],
          "name": "App2",
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
