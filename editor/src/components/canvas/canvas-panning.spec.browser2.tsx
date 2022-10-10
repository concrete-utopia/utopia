import { createComplexDefaultProjectContents } from '../../sample-projects/sample-project-utils'
import { contentsToTree } from '../assets'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import {
  keyDown,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
} from './event-helpers.test-utils'
import {
  EditorRenderResult,
  renderTestEditorWithProjectContent,
  testEditorContext,
} from './ui-jsx.test-utils'

function createExampleProject(): Promise<EditorRenderResult> {
  return renderTestEditorWithProjectContent(
    contentsToTree(createComplexDefaultProjectContents()),
    'await-first-dom-report',
    testEditorContext({}),
  )
}

describe(`pan while 'space' is held down`, () => {
  it(`press 'space' first`, async () => {
    const renderResult = await createExampleProject()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const controlsBounds = canvasControlsLayer.getBoundingClientRect()
    const startingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset

    keyDown('Space')
    mouseDragFromPointToPoint(
      canvasControlsLayer,
      {
        x: controlsBounds.x + controlsBounds.width / 2,
        y: controlsBounds.y + controlsBounds.height / 2,
      },
      {
        x: controlsBounds.x + controlsBounds.width / 2 + 100,
        y: controlsBounds.y + controlsBounds.height / 2 + 100,
      },
    )

    const endingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset
    expect(endingCanvasPosition.x - startingCanvasPosition.x).toEqual(100)
    expect(endingCanvasPosition.y - startingCanvasPosition.y).toEqual(100)
  })
  it(`start drag first`, async () => {
    const renderResult = await createExampleProject()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const controlsBounds = canvasControlsLayer.getBoundingClientRect()
    const startingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset

    mouseDownAtPoint(canvasControlsLayer, {
      x: controlsBounds.x + controlsBounds.width / 2,
      y: controlsBounds.y + controlsBounds.height / 2,
    })
    keyDown('Space')
    mouseMoveToPoint(
      canvasControlsLayer,
      {
        x: controlsBounds.x + controlsBounds.width / 2 + 100,
        y: controlsBounds.y + controlsBounds.height / 2 + 100,
      },
      {
        eventOptions: {
          buttons: 1,
          movementX: 100,
          movementY: 100,
        },
      },
    )

    const endingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset
    expect(endingCanvasPosition.x - startingCanvasPosition.x).toEqual(100)
    expect(endingCanvasPosition.y - startingCanvasPosition.y).toEqual(100)
  })
})
