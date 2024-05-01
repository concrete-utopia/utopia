import { canvasPoint } from '../../core/shared/math-utils'
import { elementPath, fromString } from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseMoveToPoint } from '../canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../canvas/ui-jsx.test-utils'
import { clearSelection, selectComponents } from '../editor/actions/action-creators'
import { cmdModifier } from '../../utils/modifiers'
import { selectComponentsForTest, wait } from '../../utils/utils.test-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../editor/store/editor-state'

function exampleProjectForSelection(): string {
  return `import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";

export var App = (props) => {
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
      <div
        data-uid='div-red'
        data-testid='div-red'
        style={{
          position: 'absolute',
          left: 50,
          top: 50,
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-label='Red'
      />
      <div
        data-uid='div-green'
        data-testid='div-green'
        style={{
          position: 'absolute',
          left: 160,
          top: 160,
          width: 100,
          height: 100,
          backgroundColor: 'green',
        }}
        data-label='Green'
      />
      <div
        data-uid='div-blue'
        data-testid='div-blue'
        style={{
          position: 'absolute',
          left: 270,
          top: 270,
          width: 100,
          height: 100,
          backgroundColor: 'blue',
        }}
        data-label='Blue'
      />
    </div>
  );
};

export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene-1"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid="app" />
    </Scene>
  </Storyboard>
);
`
}

function exampleProjectToCheckPaddingControls(): string {
  return `import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";

export var App = (props) => {
  return (
    <div
      data-uid="app-root"
      style={{
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
        display: "flex"
      }}
    >
      <div
        data-uid='div-green'
        data-testid='div-green'
        style={{
          position: 'absolute',
          left: 160,
          top: 160,
          width: 100,
          height: 100,
          backgroundColor: 'green',
        }}
        data-label='Green'
      />
      <div
        data-uid='div-blue'
        data-testid='div-blue'
        style={{
          position: 'absolute',
          left: 270,
          top: 270,
          width: 100,
          height: 100,
          backgroundColor: 'blue',
        }}
        data-label='Blue'
      />
    </div>
  );
};

export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene-1"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid="app" />
    </Scene>
  </Storyboard>
);
`
}

function exampleProjectForSectionRemoval(): string {
  return `import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        fontWeight: 500,
        fontStyle: 'normal',
        fontSize: '19px',
        transform: 'rotate(0deg)',
        backgroundAttachment: 'fixed',
        backgroundColor: '#d3d3d3',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        textShadow: '0px 0px 0px #d3d3d3',
        border: '0px solid rgb(0, 0, 0, 1)',
      }}
    />
  </Storyboard>
);
`
}

describe('inspector', () => {
  // TODO aspect ratio lock is gone from inspector

  it('switching selection should not break the inspector', async () => {
    const editor = await renderTestEditorWithCode(
      exampleProjectForSelection(),
      'await-first-dom-report',
    )

    async function clickOnElementCheckTop(
      elementTestId: string,
      expectedTop: string,
    ): Promise<void> {
      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      // Click on the element.
      const targetDiv = editor.renderedDOM.getByTestId(elementTestId)
      const targetDivRect = targetDiv.getBoundingClientRect()
      const targetDivCenter = canvasPoint({
        x: targetDivRect.x + targetDivRect.width / 2,
        y: targetDivRect.y + targetDivRect.height / 2,
      })
      await mouseMoveToPoint(canvasControlsLayer, targetDivCenter, { modifiers: cmdModifier })
      await mouseClickAtPoint(canvasControlsLayer, targetDivCenter, { modifiers: cmdModifier })
      await editor.getDispatchFollowUpActionsFinished()

      // Check that the inspector has been populated appropriately.
      const topField = editor.renderedDOM.getByTestId('frame-top-number-input') as HTMLInputElement
      expect(topField.value).toEqual(expectedTop)
    }

    // Click on the red div.
    await clickOnElementCheckTop('div-red', '50')

    // Click on the green div.
    await clickOnElementCheckTop('div-green', '160')

    // Click on the blue div.
    await clickOnElementCheckTop('div-blue', '270')
  })
  it('check that only one pair of padding controls shows up at a time', async () => {
    const editor = await renderTestEditorWithCode(
      exampleProjectToCheckPaddingControls(),
      'await-first-dom-report',
    )

    function validatePaddingControlCount(length: 0 | 1): void {
      const paddingHControls = editor.renderedDOM.queryAllByTestId('padding-H')
      const paddingVControls = editor.renderedDOM.queryAllByTestId('padding-V')
      expect(paddingHControls).toHaveLength(length)
      expect(paddingVControls).toHaveLength(length)
    }

    await editor.dispatch([clearSelection()], true)
    await editor.getDispatchFollowUpActionsFinished()

    validatePaddingControlCount(0)

    const appRootPath = elementPath([['storyboard', 'scene-1', 'app'], ['app-root']])
    await editor.dispatch([selectComponents([appRootPath], false)], true)
    await editor.getDispatchFollowUpActionsFinished()
    validatePaddingControlCount(1)

    const divPath = elementPath([
      ['storyboard', 'scene-1', 'app'],
      ['app-root', 'div-green'],
    ])
    await editor.dispatch([selectComponents([divPath], false)], true)
    await editor.getDispatchFollowUpActionsFinished()
    validatePaddingControlCount(1)
  })
  it('removes the transform property when clicking the cross on that section', async () => {
    const resultCode = await setupRemovalTest('inspector-transform-remove-all')
    expect(resultCode).toEqual(`import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        fontWeight: 500,
        fontStyle: 'normal',
        fontSize: '19px',
        backgroundAttachment: 'fixed',
        backgroundColor: '#d3d3d3',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        textShadow: '0px 0px 0px #d3d3d3',
        border: '0px solid rgb(0, 0, 0, 1)',
      }}
    />
  </Storyboard>
)
`)
  })
  it('removes the background* properties when clicking the cross on that section', async () => {
    const resultCode = await setupRemovalTest('inspector-background-remove-all')
    expect(resultCode).toEqual(`import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        fontWeight: 500,
        fontStyle: 'normal',
        fontSize: '19px',
        transform: 'rotate(0deg)',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        textShadow: '0px 0px 0px #d3d3d3',
        border: '0px solid rgb(0, 0, 0, 1)',
      }}
    />
  </Storyboard>
)
`)
  })
  it('removes the border* properties when clicking the cross on that section', async () => {
    const resultCode = await setupRemovalTest('inspector-border-remove-all')
    expect(resultCode).toEqual(`import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        fontWeight: 500,
        fontStyle: 'normal',
        fontSize: '19px',
        transform: 'rotate(0deg)',
        backgroundAttachment: 'fixed',
        backgroundColor: '#d3d3d3',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        textShadow: '0px 0px 0px #d3d3d3',
      }}
    />
  </Storyboard>
)
`)
  })
  it('removes the boxShadow property when clicking the cross on that section', async () => {
    const resultCode = await setupRemovalTest('inspector-shadow-remove-all')
    expect(resultCode).toEqual(`import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        fontWeight: 500,
        fontStyle: 'normal',
        fontSize: '19px',
        transform: 'rotate(0deg)',
        backgroundAttachment: 'fixed',
        backgroundColor: '#d3d3d3',
        textShadow: '0px 0px 0px #d3d3d3',
        border: '0px solid rgb(0, 0, 0, 1)',
      }}
    />
  </Storyboard>
)
`)
  })
  it('removes the textShadow property when clicking the cross on that section', async () => {
    const resultCode = await setupRemovalTest('inspector-text-shadow-remove-all')
    expect(resultCode).toEqual(`import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        fontWeight: 500,
        fontStyle: 'normal',
        fontSize: '19px',
        transform: 'rotate(0deg)',
        backgroundAttachment: 'fixed',
        backgroundColor: '#d3d3d3',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        border: '0px solid rgb(0, 0, 0, 1)',
      }}
    />
  </Storyboard>
)
`)
  })
  it('removes the text related properties when clicking the cross on that section', async () => {
    const resultCode = await setupRemovalTest('inspector-text-remove-all')
    expect(resultCode).toEqual(`import * as React from 'react'
import { Storyboard, jsx } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='target'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        transform: 'rotate(0deg)',
        backgroundAttachment: 'fixed',
        backgroundColor: '#d3d3d3',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        border: '0px solid rgb(0, 0, 0, 1)',
      }}
    />
  </Storyboard>
)
`)
  })

  describe('enabling inspector sections through the component API', () => {
    const project = (inspectorConfig: string) =>
      createModifiedProject({
        [StoryboardFilePath]: `import * as React from 'react'
      import * as Utopia from 'utopia-api'
      import { Storyboard, Scene } from 'utopia-api'
      
      export function Card({ header, children }) {
        return (
          <div data-uid='dbc'>
            <h2 data-uid='4b3'>{header}</h2>
            {children}
          </div>
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            style={{
              width: 521,
              height: 266,
              position: 'absolute',
              left: 717,
              top: 597,
              backgroundColor: 'white',
            }}
            data-uid='scene'
            data-testid='scene'
            commentId='sce'
          >
            <Card
              className='card'
              data-uid='card'
              header={<h3 data-uid='render-prop'>Header</h3>}
            >
              <p data-uid='child'>Card contents</p>
            </Card>
          </Scene>
        </Storyboard>
      )
      `,
        ['components.utopia.js']: `import { Card } from './storyboard'

      const Components = {
        '/utopia/storyboard': {
          Card: {
            component: Card,
            properties: {
              header: {
                control: 'jsx',
              },
            },
            inspector: ${inspectorConfig},
            variants: [],
          },
        },
      }
      
      export default Components
      `,
      })

    it('shows style, transforms, background, border, shadow, text shadow when `visual` is set', async () => {
      const editor = await renderTestEditorWithModel(
        project(JSON.stringify(['visual'])),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [fromString('sb/scene/card')])

      for (const section of ['Transforms', 'Background', 'Border', 'Shadow', 'Text Shadow']) {
        expect(editor.renderedDOM.queryByText(section)?.innerText).toEqual(section)
      }
    })

    it('shows container when `layout` is set', async () => {
      const editor = await renderTestEditorWithModel(
        project(JSON.stringify(['layout'])),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [fromString('sb/scene/card')])

      for (const section of ['Container']) {
        expect(editor.renderedDOM.queryByText(section)?.innerText).toEqual(section)
      }
    })

    it('shows the flex section when `layout-system` is set', async () => {
      const editor = await renderTestEditorWithModel(
        project(JSON.stringify(['layout'])),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [fromString('sb/scene/card')])

      for (const section of ['Layout System']) {
        expect(editor.renderedDOM.queryByText(section)?.innerText).toEqual(section)
      }
    })

    it('shows the type section when `layout-system` is set', async () => {
      const editor = await renderTestEditorWithModel(
        project(JSON.stringify(['typography'])),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [fromString('sb/scene/card')])

      for (const section of ['Type']) {
        expect(editor.renderedDOM.queryByText(section)?.innerText).toEqual(section)
      }
    })

    it('shows all sections when `inspector: "all"` is set', async () => {
      const editor = await renderTestEditorWithModel(project('all'), 'await-first-dom-report')

      await selectComponentsForTest(editor, [fromString('sb/scene/card')])

      for (const section of [
        'Frame',
        'Layout System',
        'Container',
        'Transforms',
        'Background',
        'Border',
        'Shadow',
        'Text Shadow',
        'Type',
      ]) {
        expect(editor.renderedDOM.queryByText(section)?.innerText).toEqual(section)
      }
    })

    it('shows all sections when `inspector: undefined` is set', async () => {
      const editor = await renderTestEditorWithModel(project('undefined'), 'await-first-dom-report')

      await selectComponentsForTest(editor, [fromString('sb/scene/card')])

      for (const section of [
        'Frame',
        'Layout System',
        'Container',
        'Transforms',
        'Background',
        'Border',
        'Shadow',
        'Text Shadow',
        'Type',
      ]) {
        expect(editor.renderedDOM.queryByText(section)?.innerText).toEqual(section)
      }
    })
  })
})

async function setupRemovalTest(buttonToPress: string): Promise<string> {
  const editor = await renderTestEditorWithCode(
    exampleProjectForSectionRemoval(),
    'await-first-dom-report',
  )
  const target = elementPath([['storyboard', 'target']])

  await editor.dispatch([selectComponents([target], false)], true)
  await editor.getDispatchFollowUpActionsFinished()

  const inspectorButton = editor.renderedDOM.getByTestId(buttonToPress)
  const inspectorButtonBounds = inspectorButton.getBoundingClientRect()
  await mouseClickAtPoint(inspectorButton, {
    x: inspectorButtonBounds.x + 1,
    y: inspectorButtonBounds.y + 1,
  })

  await editor.getDispatchFollowUpActionsFinished()
  return getPrintedUiJsCode(editor.getEditorState())
}
