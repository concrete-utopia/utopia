import { CanvasControlsContainerID } from '../components/canvas/controls/new-canvas-controls'
import {
  firePasteImageEvent,
  mouseClickAtPoint,
} from '../components/canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithCode,
  formatTestProjectCode,
} from '../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardVariableName } from '../core/model/scene-utils'
import { imgBase641x1, makeImageFile } from '../components/canvas/image-insert.test-utils'
import { defer } from './utils'
import Sinon from 'sinon'
import { Clipboard } from './clipboard'

describe('Pasting an image onto the canvas', () => {
  var pasteDone: ReturnType<typeof defer> = defer()
  var sandbox = Sinon.createSandbox()
  const originalParseClipboardData = Clipboard.parseClipboardData

  beforeEach(() => {
    pasteDone = defer()

    const parseClipboardDataStub = sandbox.stub(Clipboard, 'parseClipboardData')
    parseClipboardDataStub.callsFake(async (c) => {
      const result = await originalParseClipboardData(c)
      pasteDone.resolve()
      return result
    })
  })

  afterEach(() => {
    sandbox.restore()
  })

  it('Pastes successfully onto the storyboard', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboardChildren(``),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const imagesToPaste = [await makeImageFile(imgBase641x1, 'chucknorris.png')]

    firePasteImageEvent(canvasControlsLayer, imagesToPaste)

    // Wait for the next frame
    await pasteDone
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithStoryboardChildren(`
        <img
          alt=''
          src='./assets/clipboard/chucknorris.png'
          style={{
            position: 'absolute',
            left: 719,
            top: 419.5,
            width: 1,
            height: 1,
          }}
          data-aspect-ratio-locked
        />
      `),
    )
  })

  it('pastes image into flex container as flex child', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboardChildren(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 335,
      top: 147,
      width: 513,
      height: 364,
      display: 'flex',
    }}
    data-testid='container'
  />`),
      'await-first-dom-report',
    )

    const imagesToPaste = [await makeImageFile(imgBase641x1, 'chucknorris.png')]

    const container = editor.renderedDOM.getByTestId('container')
    const containerBounds = container.getBoundingClientRect()
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    await mouseClickAtPoint(canvasControlsLayer, {
      x: containerBounds.left + 1,
      y: containerBounds.top + 1,
    })

    firePasteImageEvent(canvasControlsLayer, imagesToPaste)

    // Wait for the next frame
    await pasteDone
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithStoryboardChildren(`
      <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 335,
        top: 147,
        width: 513,
        height: 364,
        display: 'flex',
      }}
      data-testid='container'
    >
    <img
    alt=''
    src='./assets/clipboard/chucknorris.png'
    style={{ width: 1, height: 1 }}
    data-aspect-ratio-locked
  />
    </div>
      `),
    )
  })
})

function makeTestProjectCodeWithStoryboardChildren(storyboardChildren: string): string {
  const code = `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard>
          ${storyboardChildren}
        </Storyboard>
      )
    }
  `

  return formatTestProjectCode(code)
}
