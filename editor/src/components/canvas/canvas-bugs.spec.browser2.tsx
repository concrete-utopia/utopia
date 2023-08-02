/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkOverflowingDragBehaviour"] }] */
import type { EditorRenderResult } from './ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from './ui-jsx.test-utils'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
} from './event-helpers.test-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'

const OverflowingElementsProject = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = (props) => {
  return (
    <div style={props.style} data-uid='c2b' data-testid='to-click-on'>
      <div data-uid='33d'>
        <span data-uid='a6e'>App</span>
      </div>
    </div>
  )
}

const SBChild = (props) => {
  return (
    <div
      style={{
        backgroundColor: '#d3d3d3',
        width: 150,
        height: 150,
        ...props.style,
      }}
      data-uid='c76'
    >
      <div data-uid='a59'>
        <span data-uid='a76'>SBChild</span>
        <div data-uid='46a'>
          <App
            style={{
              position: 'absolute',
              left: 0,
              top: 200,
              width: 150,
              height: 150,
              backgroundColor: '#F32525',
            }}
            data-uid='e78'
          />
        </div>
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <SBChild
      style={{ position: 'absolute', left: 15, top: -6 }}
      data-uid='b58'
    />
  </Storyboard>
)
`

describe('Overflowing elements', () => {
  async function checkOverflowingDragBehaviour(
    mouseOperations: (renderResult: EditorRenderResult) => Promise<void>,
  ): Promise<void> {
    const renderResult = await renderTestEditorWithCode(
      OverflowingElementsProject,
      'await-first-dom-report',
    )

    await mouseOperations(renderResult)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = (props) => {
  return (
    <div
      style={props.style}
      data-uid='c2b'
      data-testid='to-click-on'
    >
      <div data-uid='33d'>
        <span data-uid='a6e'>App</span>
      </div>
    </div>
  )
}

const SBChild = (props) => {
  return (
    <div
      style={{
        backgroundColor: '#d3d3d3',
        width: 150,
        height: 150,
        ...props.style,
      }}
      data-uid='c76'
    >
      <div data-uid='a59'>
        <span data-uid='a76'>SBChild</span>
        <div data-uid='46a'>
          <App
            style={{
              position: 'absolute',
              left: 0,
              top: 200,
              width: 150,
              height: 150,
              backgroundColor: '#F32525',
            }}
            data-uid='e78'
          />
        </div>
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <SBChild
      style={{ position: 'absolute', left: 215, top: -6 }}
      data-uid='b58'
    />
  </Storyboard>
)
`),
    )
  }

  it('should be draggable immediately after the mouse down.', async () => {
    async function mouseOperations(renderResult: EditorRenderResult): Promise<void> {
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const toClickOn = renderResult.renderedDOM.getByTestId('to-click-on')
      const toClickOnCenter = getDomRectCenter(toClickOn.getBoundingClientRect())
      const dragEndPoint = {
        ...toClickOnCenter,
        x: toClickOnCenter.x + 200,
      }
      // Move to the element that we plan on clicking on.
      await mouseMoveToPoint(canvasControlsLayer, toClickOnCenter)
      // Do an immediate click and drag.
      await mouseDragFromPointToPoint(canvasControlsLayer, toClickOnCenter, dragEndPoint)
    }
    await checkOverflowingDragBehaviour(mouseOperations)
  })
  it('should be draggable after a full click selection.', async () => {
    async function mouseOperations(renderResult: EditorRenderResult): Promise<void> {
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const toClickOn = renderResult.renderedDOM.getByTestId('to-click-on')
      const toClickOnCenter = getDomRectCenter(toClickOn.getBoundingClientRect())
      const dragEndPoint = {
        ...toClickOnCenter,
        x: toClickOnCenter.x + 200,
      }
      // Move to the element that we plan on clicking on.
      await mouseMoveToPoint(canvasControlsLayer, toClickOnCenter)
      // Click on the element before the click and drag.
      await mouseClickAtPoint(canvasControlsLayer, toClickOnCenter)
      // Click and drag.
      await mouseDragFromPointToPoint(canvasControlsLayer, toClickOnCenter, dragEndPoint)
    }
    await checkOverflowingDragBehaviour(mouseOperations)
  })
  it('should be draggable after a click and a mouse movement.', async () => {
    async function mouseOperations(renderResult: EditorRenderResult): Promise<void> {
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const toClickOn = renderResult.renderedDOM.getByTestId('to-click-on')
      const toClickOnCenter = getDomRectCenter(toClickOn.getBoundingClientRect())
      const dragEndPoint = {
        ...toClickOnCenter,
        x: toClickOnCenter.x + 200,
      }
      // Move to the element that we plan on clicking on.
      await mouseMoveToPoint(canvasControlsLayer, toClickOnCenter)
      // Click on the element before the click and drag.
      await mouseClickAtPoint(canvasControlsLayer, toClickOnCenter)
      // Move a little bit.
      await mouseMoveToPoint(canvasControlsLayer, { ...toClickOnCenter, x: toClickOnCenter.x + 10 })
      // Click and drag.
      await mouseDragFromPointToPoint(canvasControlsLayer, toClickOnCenter, dragEndPoint)
    }
    await checkOverflowingDragBehaviour(mouseOperations)
  })
  it('should be draggable after two clicks.', async () => {
    async function mouseOperations(renderResult: EditorRenderResult): Promise<void> {
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const toClickOn = renderResult.renderedDOM.getByTestId('to-click-on')
      const toClickOnCenter = getDomRectCenter(toClickOn.getBoundingClientRect())
      const dragEndPoint = {
        ...toClickOnCenter,
        x: toClickOnCenter.x + 200,
      }
      // Move to the element that we plan on clicking on.
      await mouseMoveToPoint(canvasControlsLayer, toClickOnCenter)
      // Click on the element twice before the click and drag.
      await mouseClickAtPoint(canvasControlsLayer, toClickOnCenter)
      await mouseClickAtPoint(canvasControlsLayer, toClickOnCenter)
      // Click and drag.
      await mouseDragFromPointToPoint(canvasControlsLayer, toClickOnCenter, dragEndPoint)
    }
    await checkOverflowingDragBehaviour(mouseOperations)
  })
})
