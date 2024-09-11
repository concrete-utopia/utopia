/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert", "matchInlineSnapshotBrowser"] }] */
import { canvasPoint, point } from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import { simplifiedMetadataMap, wait } from '../../../utils/utils.test-utils'
import { setFocusedElement } from '../../editor/actions/action-creators'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import CanvasActions from '../canvas-actions'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { matchInlineSnapshotBrowser } from '../../../../test/karma-snapshots'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { earlyReturnResult } from '../../../core/shared/element-template'

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
      {
        // @utopia/uid=other-expr
        [0, 1, 2].map(i => (
          <HiElement data-uid="other-hi-element" />
        ))
      }
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

const appJSWithRepresentativeComponent = `
import * as React from 'react'

const ActivityCardSmall = ({ activity }) => {
  const [completed, setCompleted] = React.useState(false)
  const toggleComplete = React.useCallback(
    () => setCompleted((completed) => !completed),
    [],
  )
  return (
    <div data-label='Activity Card' data-uid='activity-div'>
      {
      // @utopia/uid=conditional
      completed ? (
        <div data-uid={'completed-true'}>{
          // @utopia/uid=cond-completed-true
          activity
        }</div>
      ) : (
        <div data-uid={'completed-false'}>{
          // @utopia/uid=cond-completed-false
          activity
        }</div>
      )}
    </div>
  )
}

const activities = ['Running', 'Cycling']

export var App = () => {
  const smallCardView = false
  return (
    <div
      data-uid='app-root-div'
      style={{
        width: '100%',
        background: 'var(--orange)',
        overflowY: 'scroll',
        display: 'flex',
        flexDirection: 'column',
        gap: 0,
      }}
    >
      {
        // @utopia/uid=expr
        activities.map((activity) => (
          <ActivityCardSmall
            data-uid={'activity-card-small'}
            activity={activity}
          />
        ))
      }
    </div>
  )
}

`

const storyboardWithSimpleAppComponent = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 744,
        height: 1133,
        position: 'absolute',
        left: 1036,
        top: 128,
      }}
      data-label='My App'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
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
  return <div data-uid="button-root">{
    // @utopia/uid=button-children
    props.children
  }</div>;
};
export const Card = () => {
  return (
    <Button data-uid="button-instance">
      {
        // @utopia/uid=expr
        [0, 1, 2].map(i => (
          <HiElement data-uid="hi-element" />
        ))
      }
    </Button>
  );
};`,
  })
}

function createComponentWithConditionalProject() {
  return createAndRenderModifiedProject({
    [StoryboardFilePath]: storyboardWithSimpleAppComponent,
    ['/src/app.js']: appJSWithRepresentativeComponent,
  })
}

function createProjectWithEarlyReturn(returnsEarly: boolean): Promise<EditorRenderResult> {
  return createAndRenderModifiedProject({
    [StoryboardFilePath]: `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app-entity' />
    </Scene>
  </Storyboard>
)`,
    ['/src/app.js']: `import * as React from 'react'

export var App = () => {
  if (${returnsEarly}) {
    return 'Early Return Value.'
  }
  return (
    <div
      data-uid='app-root-div'
      style={{
        width: '100%',
        background: 'var(--orange)',
        overflowY: 'scroll',
        display: 'flex',
        flexDirection: 'column',
        gap: 0,
      }}
    >
      Something
    </div>
  )
}
`,
  })
}

describe('Spy Wrapper Template Path Tests', () => {
  it('captures an early return value when there is one', async () => {
    const { getEditorState } = await createProjectWithEarlyReturn(true)
    const metadata = getEditorState().editor.jsxMetadata
    expect(metadata['storyboard-entity/scene-1-entity/app-entity'].earlyReturn).toEqual(
      earlyReturnResult('Early Return Value.'),
    )
  })
  it('does not capture an early return value in the case where there is not one', async () => {
    const { getEditorState } = await createProjectWithEarlyReturn(false)
    const metadata = getEditorState().editor.jsxMetadata
    expect(metadata['storyboard-entity/scene-1-entity/app-entity'].earlyReturn).toBeNull()
  })
  it('a simple component in a regular scene', async () => {
    const { getEditorState } = await createExampleProject()

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~3": Object {
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

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance:other-button-root/209": Object {
          "name": "not-jsx-element",
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
            ['other-button-instance', 'other-expr', 'other-hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~2:c33": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~2:c33/hi-element-fragment-child-1": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~2:c33/hi-element-fragment-child-2": Object {
          "name": "div",
        },
        "storyboard/scene-1/app:other-app-root/other-inner-div/other-card-instance:other-button-instance/other-expr/other-hi-element~~~3": Object {
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

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~3": Object {
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

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~3": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance:button-root/button-children": Object {
          "name": "not-jsx-element",
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
            ['button-instance', 'expr', 'hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })
  it('component with generated conditionals with nothing focused', async () => {
    const { getEditorState } = await createComponentWithConditionalProject()

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene": Object {
          "name": "Scene",
        },
        "storyboard/scene/app": Object {
          "name": "App",
        },
        "storyboard/scene/app:app-root-div": Object {
          "name": "div",
        },
        "storyboard/scene/app:app-root-div/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~1": Object {
          "name": "ActivityCardSmall",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~2": Object {
          "name": "ActivityCardSmall",
        },
      }
    `,
    )
  })
  it('component with generated conditionals with a component focused', async () => {
    const { getEditorState, dispatch } = await createComponentWithConditionalProject()

    await dispatch(
      [
        setFocusedElement(
          EP.elementPath([
            ['storyboard', 'scene', 'app'],
            ['app-root-div', 'expr', 'activity-card-small~~~1'],
          ]),
        ),
      ],
      true,
    )

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

    matchInlineSnapshotBrowser(
      sanitizedFinalMetadata,
      `
      Object {
        "storyboard": Object {
          "name": "Storyboard",
        },
        "storyboard/scene": Object {
          "name": "Scene",
        },
        "storyboard/scene/app": Object {
          "name": "App",
        },
        "storyboard/scene/app:app-root-div": Object {
          "name": "div",
        },
        "storyboard/scene/app:app-root-div/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~1": Object {
          "name": "ActivityCardSmall",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~1:activity-div": Object {
          "name": "div",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~1:activity-div/conditional": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~1:activity-div/conditional/completed-false": Object {
          "name": "div",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~1:activity-div/conditional/completed-false/cond-completed-false": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene/app:app-root-div/expr/activity-card-small~~~2": Object {
          "name": "ActivityCardSmall",
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
              {
                // @utopia/uid=expr
                [0, 1, 2].map(i => (
                  <HiElement data-uid="hi-element" />
                ))
              }
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
            ['button-instance', 'expr', 'hi-element~~~2'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2:hi-element-root/465": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~3": Object {
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
              {
                // @utopia/uid=expr
                [0, 1, 2].map(i => (
                  <HiElement data-uid="hi-element" />
                ))
              }
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
            ['button-instance', 'expr', 'hi-element~~~2'],
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
            ['button-instance', 'expr', 'hi-element~~~2'],
            ['hi-element-root', 'hi-element-inner'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2:hi-element-root/hi-element-inner": Object {
          "name": "HiElementInner",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~2:hi-element-root/hi-element-inner:hi-element-inner-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:button-instance/expr/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })

  it('elements inside files imported multiple times can still be focused', async () => {
    const { dispatch, getEditorState } = await createAndRenderModifiedProject({
      [StoryboardFilePath]: exampleProject,

      '/src/hi-inner.js': `
        import * as React from "react";

        export const HiElementInner = (props) => {
          return (
            <div data-uid="hi-element-inner-root">
              <div data-uid="actual-text">hi!</div>
            </div>
          )
        }`,

      '/src/hi-outer.js': `
        import * as React from "react";
        import { HiElementInner } from "./hi-inner";
        export const HiElement = (props) => {
          return (
            <div data-uid="hi-element-root">
              <HiElementInner data-uid="hi-element-inner" />
            </div>
          )
        }`,

      'src/card-inner.js': `
        import * as React from "react";
        import { HiElement } from "./hi-outer";

        const Button = (props) => {
          return <div data-uid="button-root">{props.children}</div>;
        };
        export const CardInner = () => {
          return (
            <Button data-uid="button-instance">
              {
                // @utopia/uid=expr
                [0, 1, 2].map(i => (
                  <HiElement data-uid="hi-element" />
                ))
              }
            </Button>
          );
        };`,

      '/src/card.js': `
        import * as React from "react";
        import { HiElement } from "./hi-outer";
        import { CardInner } from "./card-inner";

        export const Card = () => {
          return (
            <CardInner data-uid="card-inner-instance" />
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
            ['card-inner-instance'],
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
            ['card-inner-instance'],
            ['button-instance', 'expr', 'hi-element~~~2'],
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
            ['card-inner-instance'],
            ['button-instance', 'expr', 'hi-element~~~2'],
            ['hi-element-root', 'hi-element-inner'],
          ]),
        ),
      ],
      true,
    )

    await dispatch([CanvasActions.scrollCanvas(canvasPoint(point(0, 1)))], true) // TODO fix the dom walker so it runs _after_ rendering the canvas so we can avoid this horrible hack here

    const finalMetadata = getEditorState().editor.jsxMetadata
    const sanitizedFinalMetadata = simplifiedMetadataMap(finalMetadata)

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
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance": Object {
          "name": "CardInner",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance": Object {
          "name": "Button",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr": Object {
          "name": "not-jsx-element",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~1": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~2": Object {
          "name": "HiElement",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~2:hi-element-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~2:hi-element-root/hi-element-inner": Object {
          "name": "HiElementInner",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~2:hi-element-root/hi-element-inner:hi-element-inner-root": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~2:hi-element-root/hi-element-inner:hi-element-inner-root/actual-text": Object {
          "name": "div",
        },
        "storyboard/scene-2/app2:app-outer-div/card-instance:card-inner-instance:button-instance/expr/hi-element~~~3": Object {
          "name": "HiElement",
        },
      }
    `,
    )
  })
})
