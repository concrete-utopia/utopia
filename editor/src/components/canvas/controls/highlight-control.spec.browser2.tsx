import { getDomRectCenter } from '../../../core/shared/dom-utils'
import { mouseClickAtPoint, mouseMoveToPoint } from '../event-helpers.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from './new-canvas-controls'

describe('HighlightControl', () => {
  it('is shown when an element is hovered', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div>
          <div
            style={{ backgroundColor: 'red', width: 250, height: 300 }}
            data-uid='target-to-highlight'
            data-testid='target-to-highlight'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const toHighlight = renderResult.renderedDOM.getByTestId('target-to-highlight')
    const toHighlightCenter = getDomRectCenter(toHighlight.getBoundingClientRect())
    await mouseMoveToPoint(canvasControlsLayer, toHighlightCenter)
    const highlightControls = renderResult.renderedDOM.queryAllByTestId('highlight-control')
    expect(highlightControls).toHaveLength(1)
  })
  it('is not shown when an element is hovered and selected', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div>
          <div
            style={{ backgroundColor: 'red', width: 250, height: 300 }}
            data-uid='target-to-highlight'
            data-testid='target-to-highlight'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const toHighlight = renderResult.renderedDOM.getByTestId('target-to-highlight')
    const toHighlightCenter = getDomRectCenter(toHighlight.getBoundingClientRect())
    await mouseMoveToPoint(canvasControlsLayer, toHighlightCenter)
    await mouseClickAtPoint(canvasControlsLayer, toHighlightCenter)
    const highlightControls = renderResult.renderedDOM.queryAllByTestId('highlight-control')
    expect(highlightControls).toHaveLength(0)
  })
})
