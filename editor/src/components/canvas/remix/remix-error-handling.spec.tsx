import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { runTestReturningErrorBoundaries } from './remix-error-handling.test-utils'

describe('Remix error handling', () => {
  it('Supports a user-defined error boundary', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div>Helo</div>`),
      'await-first-dom-report',
    )

    expect(renderResult.renderedDOM.getByText('Helo')).toBeDefined()
  })

  it('Bubbles errors to the canvas if no error boundary exists', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'without-custom-boundary',
    )

    expect(customBoundary).toBeNull()
    expect(canvasOverlay).not.toBeNull()
  })
})
