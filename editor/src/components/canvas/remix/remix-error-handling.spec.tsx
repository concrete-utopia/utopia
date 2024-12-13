import { fromField, fromTypeGuard } from '../../../core/shared/optics/optic-creators'
import { modify } from '../../../core/shared/optics/optic-utilities'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { isTextFile } from '../../../core/shared/project-file-types'
import { getProjectFileByFilePath } from '../../assets'
import { updateFile } from '../../editor/actions/action-creators'
import { CanvasContainerID } from '../canvas-types'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import {
  createMaybeFailingProject,
  renderRemixProject,
  runTestReturningErrorBoundaries,
} from './remix-error-handling.test-utils'

describe('Remix error handling', () => {
  it('Supports a user-defined error boundary', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'with-custom-boundary',
      'use-error-boundaries',
    )

    expect(canvasOverlay).toBeNull()
  })

  it('Bubbles errors to the canvas if no error boundary exists', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'without-custom-boundary',
      'use-error-boundaries',
    )

    expect(customBoundary).toBeNull()
    expect(canvasOverlay).not.toBeNull()
  })
  it('Supports a user-defined error boundary, ignoring it', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'with-custom-boundary',
      'ignore-error-boundaries',
    )

    expect(customBoundary).toBeNull()
    expect(canvasOverlay).not.toBeNull()
  })

  it('Bubbles errors to the canvas if no error boundary exists, ignoring any error boundaries', async () => {
    const { customBoundary, canvasOverlay } = await runTestReturningErrorBoundaries(
      'without-custom-boundary',
      'ignore-error-boundaries',
    )

    expect(customBoundary).toBeNull()
    expect(canvasOverlay).not.toBeNull()
  })

  it('Removing a failing bit of logic, results in the shown error going away', async () => {
    const notFailingProject = createMaybeFailingProject(false)
    const failingProject = createMaybeFailingProject(true)
    const renderResult = await renderRemixProject(notFailingProject)

    // Check upfront that the project renders.
    expect(renderResult.renderedDOM.queryByText('Index Content')).not.toBeNull()
    expect(renderResult.renderedDOM.queryByText('Error: Failure')).toBeNull()

    // Change the index file to make it thrown an exception.
    const indexPath = '/app/routes/_index.js'
    let failingIndexFile = forceNotNull(
      'Should be able to get failing index file.',
      getProjectFileByFilePath(failingProject.projectContents, indexPath),
    )
    failingIndexFile = modify(
      fromTypeGuard(isTextFile).compose(fromField('versionNumber')),
      (versionNumber) => versionNumber + 10,
      failingIndexFile,
    )
    await renderResult.dispatch([updateFile(indexPath, failingIndexFile, false)], true)

    // Check that the error is now shown.
    expect(renderResult.renderedDOM.queryByText('Index Content')).toBeNull()
    expect(renderResult.renderedDOM.queryByText('Error: Failure')).not.toBeNull()

    // Reset the index file to make it not throw an exception.
    let notFailingIndexFile = forceNotNull(
      'Should be able to get not failing index file.',
      getProjectFileByFilePath(notFailingProject.projectContents, indexPath),
    )
    notFailingIndexFile = modify(
      fromTypeGuard(isTextFile).compose(fromField('versionNumber')),
      (versionNumber) => versionNumber + 20,
      notFailingIndexFile,
    )
    await renderResult.dispatch([updateFile(indexPath, notFailingIndexFile, false)], true)

    // Check that the error is now gone.
    expect(renderResult.renderedDOM.queryByText('Index Content')).not.toBeNull()
    expect(renderResult.renderedDOM.queryByText('Error: Failure')).toBeNull()
  })
})
