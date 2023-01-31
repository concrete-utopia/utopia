import { CanvasControlsContainerID } from '../components/canvas/controls/new-canvas-controls'
import { firePasteImageEvent } from '../components/canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithCode,
  formatTestProjectCode,
} from '../components/canvas/ui-jsx.test-utils'
import { BakedInStoryboardVariableName } from '../core/model/scene-utils'
import { imgBase641x1, makeImageFile } from '../components/canvas/image-insert.test-utils'
import { wait } from './utils.test-utils'

describe('Pasting an image onto the canvas', () => {
  it('Pastes successfully onto the storyboard', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboardChildren(``),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const imagesToPaste = [await makeImageFile(imgBase641x1, 'chucknorris.png')]

    firePasteImageEvent(canvasControlsLayer, imagesToPaste)

    // Wait for the next frame
    await wait(1)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithStoryboardChildren(`
        <img
          alt=''
          src='./assets/clipboard/chucknorris.png'
          style={{
            position: 'absolute',
            left: 99.5,
            top: 99.5,
            width: 1,
            height: 1,
          }}
          data-aspect-ratio-locked
        />
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
