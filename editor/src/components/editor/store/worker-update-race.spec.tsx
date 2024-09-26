import { act } from '@testing-library/react'
import { getDefaultUIJsFile } from '../../../core/model/new-project-files'
import { type TextFile, textFile } from '../../../core/shared/project-file-types'
import type { ParseOrPrint, ParseOrPrintResult } from '../../../core/workers/common/worker-types'
import type { SteganographyMode } from '../../../core/workers/parser-printer/parser-printer'
import { simpleDefaultProject } from '../../../sample-projects/sample-project-utils'
import { defer } from '../../../utils/utils'
import { getProjectFileByFilePath } from '../../assets'
import { renderTestEditorWithModel } from '../../canvas/ui-jsx.test-utils'
import { updateFile } from '../actions/action-creators'
import { StoryboardFilePath } from './editor-state'
import type { FilePathMappings } from '../../../core/model/project-file-utils'

// We have to prefix all of these with "mock" otherwise Jest won't allow us to use them below
const mockDefer = defer
let mockLock1 = defer()
let mockLock2 = defer()
let mockParseStartedCount = 0
let mockParseCompletedCount = 0

jest.mock('../../../core/workers/common/worker-types', () => ({
  ...jest.requireActual('../../../core/workers/common/worker-types'),
  async getParseResult(
    workers: any,
    files: Array<ParseOrPrint>,
    filePathMappings: FilePathMappings,
    alreadyExistingUIDs: Set<string>,
    applySteganography: SteganographyMode,
    parserChunkCount: number,
  ): Promise<Array<ParseOrPrintResult>> {
    mockParseStartedCount++
    const result = await jest
      .requireActual('../../../core/workers/common/worker-types')
      .getParseResult(
        workers,
        files,
        filePathMappings,
        alreadyExistingUIDs,
        applySteganography,
        parserChunkCount,
      )
    mockLock2.resolve()
    await mockLock1
    mockLock1 = mockDefer()
    mockParseCompletedCount++
    return result
  },
}))

describe('Updates from the worker that are skipped', () => {
  it('Trigger a re-parse immediately after', async () => {
    const renderResult = await renderTestEditorWithModel(
      simpleDefaultProject(),
      'dont-await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check that the parsing has started but not yet completed
    expect(mockParseStartedCount).toBe(1)
    expect(mockParseCompletedCount).toBe(0)

    const startingStoryboard = getProjectFileByFilePath(
      renderResult.getEditorState().editor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(startingStoryboard.versionNumber).toBe(1)
    expect(startingStoryboard.fileContents.parsed.type).toBe('UNPARSED')

    // Update any existing file so that the worker update is skipped
    const modifiedStoryboard = textFile(startingStoryboard.fileContents, null, null, 2)
    await renderResult.dispatch([updateFile(StoryboardFilePath, modifiedStoryboard, false)], false)

    await renderResult.getDispatchFollowUpActionsFinished()

    // Ensure the file was updated and is still unparsed
    const updatedStoryboardFile = getProjectFileByFilePath(
      renderResult.getEditorState().editor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(updatedStoryboardFile.versionNumber).toBe(2)
    expect(updatedStoryboardFile.fileContents.parsed.type).toBe('UNPARSED')

    // Unblock and run the parser printer once
    await mockLock2
    mockLock2 = defer()
    mockLock1.resolve()
    await mockLock2

    // The parse result should have been ignored
    expect(mockParseStartedCount).toBe(2)
    expect(mockParseCompletedCount).toBe(1)
    const storyboardFileAfterIgnoredParse = getProjectFileByFilePath(
      renderResult.getEditorState().editor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(storyboardFileAfterIgnoredParse.versionNumber).toBe(2)
    expect(storyboardFileAfterIgnoredParse.fileContents.parsed.type).toBe('UNPARSED')

    await act(async () => renderResult.dispatch([], false))

    // Unblock and run the parser printer again
    mockLock1.resolve()
    await renderResult.getDispatchFollowUpActionsFinished()

    // The parse result should have been applied
    expect(mockParseStartedCount).toBe(2)
    expect(mockParseCompletedCount).toBe(2)
    const storyboardFileAfterSuccessfulParse = getProjectFileByFilePath(
      renderResult.getEditorState().editor.projectContents,
      StoryboardFilePath,
    ) as TextFile
    expect(storyboardFileAfterSuccessfulParse.versionNumber).toBe(2)
    expect(storyboardFileAfterSuccessfulParse.fileContents.parsed.type).toBe('PARSE_SUCCESS')
  })
})
