import { canvasPoint } from '../../core/shared/math-utils'
import { elementPath } from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseMoveToPoint } from '../canvas/event-helpers.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { selectComponents } from '../editor/actions/action-creators'
import { AspectRatioLockButtonTestId } from './sections/layout-section/self-layout-subsection/gigantic-size-pins-subsection'
import { cmdModifier } from '../../utils/modifiers'

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

describe('inspector', () => {
  it('toggle aspect ratio lock off', async () => {
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

  it('toggle aspect ratio lock on', async () => {
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
      const topField = editor.renderedDOM.getByTestId(
        'position-top-number-input',
      ) as HTMLInputElement
      expect(topField.value).toEqual(expectedTop)
    }

    // Click on the red div.
    await clickOnElementCheckTop('div-red', '50')

    // Click on the green div.
    await clickOnElementCheckTop('div-green', '160')

    // Click on the blue div.
    await clickOnElementCheckTop('div-blue', '270')
  })
})

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
