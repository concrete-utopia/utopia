import { canvasPoint, point } from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import { simplifiedMetadataMap } from '../../../utils/utils.test-utils'
import { setFocusedElement } from '../../editor/actions/action-creators'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import CanvasActions from '../canvas-actions'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { matchInlineSnapshotBrowser } from '../../../../test/karma-snapshots'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'

const exampleProject = `
import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";
import { View } from "utopia-api";
import { App } from "/src/app";

const HiElement = (props) => {
  return (
    <>
      <div data-uid="hi-element-fragment-child-1">hi!</div>
      <div data-uid="hi-element-fragment-child-2">hi!</div>
    </>
  )
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

function createAndRenderModifiedProject(modifiedFiles: { [filename: string]: string }) {
  const project = createModifiedProject(modifiedFiles)
  return renderTestEditorWithModel(project, 'await-first-dom-report')
}

function createExampleProject() {
  return createAndRenderModifiedProject({
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

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )
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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )
  })

  it('a component instance is focused inside a component instance inside the main App component', async () => {
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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )
  })

  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e/hi-element-fragment-child-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e/hi-element-fragment-child-2": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e/hi-element-fragment-child-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e/hi-element-fragment-child-2": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e/hi-element-fragment-child-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~2:38e/hi-element-fragment-child-2": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
      }
    `,
    )
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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })

  it('a component instance is focused inside a component instance inside the main App component', async () => {
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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "name": "div",
        },
      }
    `,
    )
  })

  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })
})

describe('Spy Wrapper Multifile With Cyclic Dependencies', () => {
  it('a generated component instance is focused inside a component instance inside the main App component', async () => {
    const { dispatch, getEditorState } = await createAndRenderModifiedProject({
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
          ]),
        ),
      ],
      true,
    )

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
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })

  it('elements inside cyclic imports can still be focused', async () => {
    const { dispatch, getEditorState } = await createAndRenderModifiedProject({
      [StoryboardFilePath]: exampleProject,

      '/src/hi.js': `
        import * as React from "react";
        import { HiElementInner } from "./card";
        export const HiElement = (props) => {
          return (
            <div data-uid="hi-element-root">
              <HiElementInner data-uid="hi-element-inner" />
            </div>
          )
        }`,

      '/src/card.js': `
        import * as React from "react";
        import { HiElement } from "./hi";

        export const HiElementInner = (props) => {
          return <div data-uid="hi-element-inner-root">hi!</div>
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

    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene-2', 'app2'],
            ['app-outer-div', 'card-instance'],
            ['button-instance', 'hi-element~~~2'],
            ['hi-element-root', 'hi-element-inner'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const spiedMetadata = getEditorState().editor.spyMetadata
    const sanitizedSpyData = simplifiedMetadataMap(spiedMetadata)

    const domMetadata = getEditorState().editor.domMetadata
    const sanitizedDomMetadata = simplifiedMetadataMap(domMetadata)

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedSpyData,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root/hi-element-inner": Object {
          "name": "HiElementInner",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root/hi-element-inner:hi-element-inner-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedDomMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root/hi-element-inner": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root/hi-element-inner:hi-element-inner-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "div",
        },
      }
    `,
    )

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene-1": Object {
          "name": "Scene",
        },
        "storyboard/scene-1/app": Object {
          "name": "SameFileApp",
        },
        "storyboard/scene-1/app:other-app-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2": Object {
          "name": "Scene",
        },
        "storyboard/scene-2/app2": Object {
          "name": "App",
        },
        "storyboard/scene-2/app2:app-outer-div": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance": Object {
          "name": "Card",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root/hi-element-inner": Object {
          "name": "HiElementInner",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~2:hi-element-root/hi-element-inner:hi-element-inner-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })
})
