import { setFeatureForUnitTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { runTestReturningErrorBoundaries } from './remix-error-handling.test-utils'

// This test and its sibling have to be kept in separate files,
// as otherwise it causes issues with Jest for some reason

describe('Remix error handling', () => {
  setFeatureForUnitTestsUseInDescribeBlockOnly('Remix support', true)

  it('Bubbles errors to the canvas if no error boundary exists', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'without-custom-boundary',
    )
    expect(customBoundary).toBeUndefined()
    expect(canvasOverlay).toBeDefined()
  })
})
