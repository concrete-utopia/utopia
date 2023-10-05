import { setFeatureForUnitTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { runTestReturningErrorBoundaries } from './remix-error-handling.test-utils'

// This test and its sibling have to be kept in separate files,
// as otherwise it causes issues with Jest for some reason

describe('Remix error handling', () => {
  it('Supports a user-defined error boundary', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'with-custom-boundary',
    )
    expect(customBoundary).not.toBeNull()
    expect(canvasOverlay).toBeNull()
  })
})
