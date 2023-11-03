import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { runTestReturningErrorBoundaries } from './remix-error-handling.test-utils'

describe('Remix error handling', () => {
  it('Supports a user-defined error boundary', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'with-custom-boundary',
    )
    expect(customBoundary).not.toBeNull()
    expect(canvasOverlay).toBeNull()
  })

  it('Bubbles errors to the canvas if no error boundary exists', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'without-custom-boundary',
    )

    expect(customBoundary).toBeNull()
    expect(canvasOverlay).not.toBeNull()
  })
})
