import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { act } from '@testing-library/react'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { offsetPoint, windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../event-helpers.test-utils'

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = {
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  }

  mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
  })
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
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      <div
        style={{
          position: 'absolute',
          left: 93.5,
          top: 58,
          width: 100,
          height: 100,
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
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
      data-uid='flexparent'
      data-testid='flexparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
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

describe('Flex Reparent To Absolute Strategy', () => {
  it('reparents flex element to absolute parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
    const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
    const targetAbsoluteParentCenter = {
      x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
      y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
    }
    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildCenter = {
      x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetAbsoluteParentCenter.x - firstFlexChildCenter.x,
      y: targetAbsoluteParentCenter.y - firstFlexChildCenter.y,
    })
    dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier)

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
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      <div
        style={{
          position: 'absolute',
          left: 93.5,
          top: 58,
          width: 100,
          height: 100,
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            position: 'absolute',
            left: -0.5,
            top: 0,
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
      </div>
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
      data-uid='flexparent'
      data-testid='flexparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
    </div>
  </div>`),
    )
  })
})
