import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { offsetPoint, windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../utils/modifiers'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import { wait } from '../../../core/model/performance-scripts'

// TODO share this code with the other tests

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControl = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  })
  const endPoint = offsetPoint(startPoint, dragDelta)
  fireEvent(
    canvasControl,
    new MouseEvent('mousedown', {
      bubbles: true,
      cancelable: true,
      metaKey: true,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
      clientX: startPoint.x,
      clientY: startPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    canvasControl,
    new MouseEvent('mousemove', {
      bubbles: true,
      cancelable: true,
      metaKey: modifiers.cmd,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
      clientX: endPoint.x,
      clientY: endPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    window,
    new MouseEvent('mouseup', {
      bubbles: true,
      cancelable: true,
      metaKey: modifiers.cmd,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
      clientX: endPoint.x,
      clientY: endPoint.y,
    }),
  )
}

const defaultTestCode = `
  <div
    style={{
      position: 'absolute',
      width: 700,
      height: 600,
    }}
    data-uid='container'
    data-testid='container'
  >
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'blue',
      }}
      data-uid='flexparent1'
      data-testid='flexparent1'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'purple',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'pink',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
    </div>
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent2'
      data-testid='flexparent2'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'teal',
        }}
        data-uid='flexchild3'
        data-testid='flexchild3'
      />
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-uid='flexchild4'
        data-testid='flexchild4'
      />
    </div>
  </div>
`

function makeTestProjectCodeWithComponentInnards(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
${componentInnards}
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 2000, height: 2000 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

function makeTestProjectCodeWithSnippet(snippet: string): string {
  return makeTestProjectCodeWithComponentInnards(`
  return (
${snippet}
  )
`)
}

describe('Flex Reparent To Flex Strategy', () => {
  beforeEach(() => {
    viewport.set(2200, 1000)
  })
  it('reparents flex element to other flex parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const targetFlexParent = await renderResult.renderedDOM.findByTestId('flexparent1')
    const targetFlexParentRect = targetFlexParent.getBoundingClientRect()
    const targetFlexParentEnd = {
      x: targetFlexParentRect.x + targetFlexParentRect.width - 15,
      y: targetFlexParentRect.y + targetFlexParentRect.height / 2,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexParentEnd.x - flexChildToReparentCenter.x,
      y: targetFlexParentEnd.y - flexChildToReparentCenter.y,
    })
    act(() => dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier))

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
    <div
      style={{
        position: 'absolute',
        width: 700,
        height: 600,
      }}
      data-uid='container'
      data-testid='container'
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 0,
          top: 0,
          backgroundColor: 'blue',
        }}
        data-uid='flexparent1'
        data-testid='flexparent1'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'purple',
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'pink',
          }}
          data-uid='flexchild2'
          data-testid='flexchild2'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            position: 'relative',
          }}
          data-uid='flexchild3'
          data-testid='flexchild3'
        />
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })

  it('reparents flex as first child when moving the mouse to the left edge of the first child', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const targetFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const targetFlexChildRect = targetFlexChild.getBoundingClientRect()
    const targetFlexChildCenter = {
      x: targetFlexChildRect.x + 10,
      y: targetFlexChildRect.y + targetFlexChildRect.height / 2,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexChildCenter.x - flexChildToReparentCenter.x,
      y: targetFlexChildCenter.y - flexChildToReparentCenter.y,
    })
    act(() => dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier))

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
    <div
      style={{
        position: 'absolute',
        width: 700,
        height: 600,
      }}
      data-uid='container'
      data-testid='container'
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 0,
          top: 0,
          backgroundColor: 'blue',
        }}
        data-uid='flexparent1'
        data-testid='flexparent1'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            position: 'relative',
          }}
          data-uid='flexchild3'
          data-testid='flexchild3'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'purple',
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'pink',
          }}
          data-uid='flexchild2'
          data-testid='flexchild2'
        />
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })
})
