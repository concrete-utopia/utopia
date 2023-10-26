import { canvasPoint } from '../../core/shared/math-utils'
import { elementPath } from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseMoveToPoint } from '../canvas/event-helpers.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { clearSelection, selectComponents } from '../editor/actions/action-creators'
import { AspectRatioLockButtonTestId } from './sections/layout-section/self-layout-subsection/gigantic-size-pins-subsection'
import { cmdModifier } from '../../utils/modifiers'
import { wait } from '../../utils/utils.test-utils'

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

  xit('toggle aspect ratio lock off', async () => {
    const codeAfterToggle = await runToggleAspectRatioLockTest('locked')
    expect(codeAfterToggle).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
  </Storyboard>
)
`)
  })

  xit('toggle aspect ratio lock on', async () => {
    const codeAfterToggle = await runToggleAspectRatioLockTest('not-locked')
    expect(codeAfterToggle).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
      data-aspect-ratio-locked
    />
  </Storyboard>
)
`)
  })

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

    function validatePaddingControlCount(): void {
      const paddingHControls = editor.renderedDOM.queryAllByTestId('padding-H')
      const paddingVControls = editor.renderedDOM.queryAllByTestId('padding-V')
      expect(paddingHControls).toHaveLength(1)
      expect(paddingVControls).toHaveLength(1)
    }

    await editor.dispatch([clearSelection()], true)
    await editor.getDispatchFollowUpActionsFinished()
    validatePaddingControlCount()

    const appRootPath = elementPath([['storyboard', 'scene-1', 'app'], ['app-root']])
    await editor.dispatch([selectComponents([appRootPath], false)], true)
    await editor.getDispatchFollowUpActionsFinished()
    validatePaddingControlCount()

    const divPath = elementPath([
      ['storyboard', 'scene-1', 'app'],
      ['app-root', 'div-green'],
    ])
    await editor.dispatch([selectComponents([divPath], false)], true)
    await editor.getDispatchFollowUpActionsFinished()
    validatePaddingControlCount()
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

async function runToggleAspectRatioLockTest(
  aspectRatioLocked: AspectRatioLockedState,
): Promise<string> {
  const editor = await renderTestEditorWithCode(
    projectSource(aspectRatioLocked),
    'await-first-dom-report',
  )
  const target = elementPath([['0cd', '7a0']])

  await editor.dispatch([selectComponents([target], false)], true)
  await editor.getDispatchFollowUpActionsFinished()

  const aspectRatioLockButton = editor.renderedDOM.getByTestId(AspectRatioLockButtonTestId)
  const aspectRatioLockButtonBounds = aspectRatioLockButton.getBoundingClientRect()
  await mouseClickAtPoint(aspectRatioLockButton, {
    x: aspectRatioLockButtonBounds.x + 1,
    y: aspectRatioLockButtonBounds.y + 1,
  })

  await editor.getDispatchFollowUpActionsFinished()
  return getPrintedUiJsCode(editor.getEditorState())
}

type AspectRatioLockedState = 'locked' | 'not-locked'

const isAspectRatioLocked = (state: AspectRatioLockedState): boolean => {
  switch (state) {
    case 'locked':
      return true
    case 'not-locked':
      return false
    default:
      assertNever(state)
  }
}

function projectSource(aspectRatioLockedState: AspectRatioLockedState) {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
      data-aspect-ratio-locked${isAspectRatioLocked(aspectRatioLockedState) ? `` : `={false}`}
    />
  </Storyboard>
)
`
}
