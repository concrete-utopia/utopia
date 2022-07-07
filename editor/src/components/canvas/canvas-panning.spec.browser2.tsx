import { act, fireEvent } from '@testing-library/react'
import { createComplexDefaultProjectContents } from '../../sample-projects/sample-project-utils'
import { contentsToTree } from '../assets'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { EditorRenderResult, renderTestEditorWithProjectContent } from './ui-jsx.test-utils'

function createExampleProject(): Promise<EditorRenderResult> {
  return renderTestEditorWithProjectContent(
    contentsToTree(createComplexDefaultProjectContents()),
    'await-first-dom-report',
  )
}

describe(`pan while 'space' is held down`, () => {
  it(`press 'space' first`, async () => {
    const renderResult = await createExampleProject()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const controlsBounds = canvasControlsLayer.getBoundingClientRect()
    const startingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset

    act(() => {
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Space', keyCode: 32 }))
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          clientX: controlsBounds.x + controlsBounds.width / 2,
          clientY: controlsBounds.y + controlsBounds.height / 2,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          clientX: controlsBounds.x + controlsBounds.width / 2 + 100,
          clientY: controlsBounds.y + controlsBounds.height / 2 + 100,
          movementX: 100,
          movementY: 100,
          buttons: 1,
        }),
      )
    })

    const endingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset
    expect(endingCanvasPosition.x - startingCanvasPosition.x).toEqual(100)
    expect(endingCanvasPosition.y - startingCanvasPosition.y).toEqual(100)
  })
  it(`start drag first`, async () => {
    const renderResult = await createExampleProject()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const controlsBounds = canvasControlsLayer.getBoundingClientRect()
    const startingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset

    act(() => {
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          clientX: controlsBounds.x + controlsBounds.width / 2,
          clientY: controlsBounds.y + controlsBounds.height / 2,
          buttons: 1,
        }),
      )
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Space', keyCode: 32 }))
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          clientX: controlsBounds.x + controlsBounds.width / 2 + 100,
          clientY: controlsBounds.y + controlsBounds.height / 2 + 100,
          movementX: 100,
          movementY: 100,
          buttons: 1,
        }),
      )
    })

    const endingCanvasPosition = renderResult.getEditorState().editor.canvas.roundedCanvasOffset
    expect(endingCanvasPosition.x - startingCanvasPosition.x).toEqual(100)
    expect(endingCanvasPosition.y - startingCanvasPosition.y).toEqual(100)
  })
})
