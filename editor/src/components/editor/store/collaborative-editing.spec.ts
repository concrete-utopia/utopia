import { Y } from '../../../core/shared/yjs'
import {
  RevisionsState,
  assetFile,
  imageFile,
  isParseSuccess,
  textFile,
  textFileContents,
} from '../../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../../assets'
import { addFileToProjectContents, removeFromProjectContents } from '../../assets'
import {
  addHookForProjectChanges,
  collateCollaborativeProjectChanges,
  removeSourceMaps,
  updateCollaborativeProjectContents,
} from './collaborative-editing'
import type { EditorAction } from '../action-types'
import type { CollaborativeEditingSupportSession } from './editor-state'
import { emptyCollaborativeEditingSupportSession } from './editor-state'
import { testParseCode } from '../../../core/workers/parser-printer/parser-printer.test-utils'
import {
  deleteFileFromCollaboration,
  updateExportsDetailFromCollaborationUpdate,
  updateFileFromCollaboration,
  updateImportsFromCollaborationUpdate,
  updateTopLevelElementsFromCollaborationUpdate,
} from '../actions/action-creators'
import { fromField, fromTypeGuard, traverseArray } from '../../../core/shared/optics/optic-creators'
import type { TopLevelElement } from '../../../core/shared/element-template'
import { isArbitraryJSBlock } from '../../../core/shared/element-template'
import { set } from '../../../core/shared/optics/optic-utilities'
import type { Optic } from '../../../core/shared/optics/optics'
import type { RawSourceMap } from '../../../core/workers/ts/ts-typings/RawSourceMap'

interface AddHookForProjectChangesTest {
  initialState: ProjectContentTreeRoot
  changesToApply: (
    firstSession: CollaborativeEditingSupportSession,
    secondSession: CollaborativeEditingSupportSession,
  ) => void
  expectations: (
    firstSessionPromises: Array<EditorAction>,
    secondSessionPromises: Array<EditorAction>,
  ) => void
}

const elementArrayToSourceMapOptic: Optic<
  Array<TopLevelElement>,
  RawSourceMap | null
> = traverseArray<TopLevelElement>()
  .compose(fromTypeGuard(isArbitraryJSBlock))
  .compose(fromField('sourceMap'))

async function runAddHookForProjectChangesTest(test: AddHookForProjectChangesTest): Promise<void> {
  // Create the first collaboration session.
  const firstCollaborationSession = emptyCollaborativeEditingSupportSession()

  // A fake dispatch for the first session to be used with `addHookForProjectChanges`.
  let firstDispatchPromises: Array<EditorAction> = []
  function firstDispatch(actions: ReadonlyArray<EditorAction>): void {
    firstDispatchPromises.push(...actions)
  }

  // Create the second collaboration session.
  const secondCollaborationSession = emptyCollaborativeEditingSupportSession()

  // A fake dispatch for the second session to be used with `addHookForProjectChanges`.
  let secondDispatchPromises: Array<EditorAction> = []
  function secondDispatch(actions: ReadonlyArray<EditorAction>): void {
    secondDispatchPromises.push(...actions)
  }

  try {
    // Updates to the first doc should be applied to the second doc.
    // Currently this includes updates from everywhere, including itsef.
    firstCollaborationSession.mergeDoc.on('update', (update) => {
      Y.applyUpdate(secondCollaborationSession.mergeDoc, update)
    })
    // Updates to the second doc should be applied to the first doc.
    // Currently this includes updates from everywhere, including itself.
    secondCollaborationSession.mergeDoc.on('update', (update) => {
      Y.applyUpdate(firstCollaborationSession.mergeDoc, update)
    })

    // Hook into changes to the `projectContents` to have them dispatch actions into
    // the fake dispatch calls which record those actions.
    addHookForProjectChanges(firstCollaborationSession, firstDispatch)
    addHookForProjectChanges(secondCollaborationSession, secondDispatch)

    // Apply the initial state.
    const initialStateChanges = collateCollaborativeProjectChanges({}, test.initialState)
    updateCollaborativeProjectContents(firstCollaborationSession, initialStateChanges, [])

    // Blank out the recorded promises which will have likely been modified by the change
    // to the initial state.
    firstDispatchPromises = []
    secondDispatchPromises = []

    // Apply the changes for this test case.
    test.changesToApply(firstCollaborationSession, secondCollaborationSession)

    // Test the result of the above changes.
    test.expectations(firstDispatchPromises, secondDispatchPromises)
  } finally {
    // Cleanup the `Doc` instances.
    secondCollaborationSession.mergeDoc.destroy()
    firstCollaborationSession.mergeDoc.destroy()
  }
}

describe('addHookForProjectChanges', () => {
  it('adding file causes it to be added to the project contents', async () => {
    const code = 'export const A = <div />'
    const newParsedCode = testParseCode(code)
    if (!isParseSuccess(newParsedCode)) {
      throw new Error('Code should parse.')
    }

    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const newFile = textFile(
          textFileContents(code, newParsedCode, RevisionsState.BothMatch),
          null,
          null,
          1,
        )
        const updatedProjectContents = addFileToProjectContents({}, '/assets/test1.js', newFile)
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        const fixedElements = set(
          elementArrayToSourceMapOptic,
          null,
          removeSourceMaps(newParsedCode.topLevelElements),
        )
        expect(secondSessionPromises).toEqual([
          updateTopLevelElementsFromCollaborationUpdate('/assets/test1.js', fixedElements),
          updateExportsDetailFromCollaborationUpdate(
            '/assets/test1.js',
            newParsedCode.exportsDetail,
          ),
          updateImportsFromCollaborationUpdate('/assets/test1.js', newParsedCode.imports),
        ])
      },
    })
  })

  it('adding image causes it to be added to the project contents', async () => {
    const newFile = imageFile('jpg', undefined, 640, 480, 58345987345, undefined)
    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const updatedProjectContents = addFileToProjectContents({}, '/assets/test1.jpg', newFile)
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        expect(secondSessionPromises).toEqual([
          updateFileFromCollaboration('/assets/test1.jpg', newFile, true),
        ])
      },
    })
  })

  it('adding image does not get added to the project contents if it includes the binary contents', async () => {
    const newFile = imageFile(
      'jpg',
      '84395834543hj5h4kjh5j43h5dfgdfgd',
      640,
      480,
      58345987345,
      undefined,
    )
    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const updatedProjectContents = addFileToProjectContents({}, '/assets/test1.jpg', newFile)
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        expect(secondSessionPromises).toEqual([])
      },
    })
  })

  it('adding asset causes it to be added to the project contents', async () => {
    const newFile = assetFile(undefined, '01189998819991197253')
    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const updatedProjectContents = addFileToProjectContents({}, '/assets/test1.ttf', newFile)
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        expect(secondSessionPromises).toEqual([
          updateFileFromCollaboration('/assets/test1.ttf', newFile, true),
        ])
      },
    })
  })

  it('adding asset does not get added to the project contents if it includes the binary contents', async () => {
    const newFile = assetFile('aaaaaaaabbbbbbbcccccccccccc', '01189998819991197253')
    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const updatedProjectContents = addFileToProjectContents({}, '/assets/test1.ttf', newFile)
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        expect(secondSessionPromises).toEqual([])
      },
    })
  })

  it('adding file causes it to be added to the project contents, unless the file was modified by another user', async () => {
    const code = 'export const A = <div />'
    const newParsedCode = testParseCode(code)
    if (!isParseSuccess(newParsedCode)) {
      throw new Error('Code should parse.')
    }

    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const newFile = textFile(
          textFileContents(code, newParsedCode, RevisionsState.BothMatch),
          null,
          null,
          1,
        )
        const updatedProjectContents = addFileToProjectContents({}, '/assets/test1.js', newFile)
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, ['/assets/test1.js'])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        expect(secondSessionPromises).toEqual([])
      },
    })
  })

  it('adding two files cause them to be added to the project contents', async () => {
    const firstFileCode = 'export const A = <div />'
    const firstFileParsedCode = testParseCode(firstFileCode)
    if (!isParseSuccess(firstFileParsedCode)) {
      throw new Error('Code should parse.')
    }

    const secondFileCode = 'export const A = <div />'
    const secondFileParsedCode = testParseCode(secondFileCode)
    if (!isParseSuccess(secondFileParsedCode)) {
      throw new Error('Code should parse.')
    }

    await runAddHookForProjectChangesTest({
      initialState: {},
      changesToApply: (firstSession, secondSession) => {
        const firstFile = textFile(
          textFileContents(firstFileCode, firstFileParsedCode, RevisionsState.BothMatch),
          null,
          null,
          1,
        )
        const secondFile = textFile(
          textFileContents(secondFileCode, secondFileParsedCode, RevisionsState.BothMatch),
          null,
          null,
          1,
        )
        const updatedProjectContents = addFileToProjectContents(
          addFileToProjectContents({}, '/assets/test1.js', firstFile),
          '/assets/test2.js',
          secondFile,
        )
        const collaborationChanges = collateCollaborativeProjectChanges({}, updatedProjectContents)
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        const firstFileFixedElements = set(
          elementArrayToSourceMapOptic,
          null,
          firstFileParsedCode.topLevelElements,
        )
        const secondFileFixedElements = set(
          elementArrayToSourceMapOptic,
          null,
          secondFileParsedCode.topLevelElements,
        )
        expect(secondSessionPromises).toEqual([
          updateTopLevelElementsFromCollaborationUpdate(
            '/assets/test1.js',
            removeSourceMaps(firstFileFixedElements),
          ),
          updateExportsDetailFromCollaborationUpdate(
            '/assets/test1.js',
            firstFileParsedCode.exportsDetail,
          ),
          updateImportsFromCollaborationUpdate('/assets/test1.js', firstFileParsedCode.imports),
          updateTopLevelElementsFromCollaborationUpdate(
            '/assets/test2.js',
            removeSourceMaps(secondFileFixedElements),
          ),
          updateExportsDetailFromCollaborationUpdate(
            '/assets/test2.js',
            secondFileParsedCode.exportsDetail,
          ),
          updateImportsFromCollaborationUpdate('/assets/test2.js', secondFileParsedCode.imports),
        ])
      },
    })
  })

  it('adding a new file to the project contents is resolved correctly', async () => {
    const firstFileCode = 'export const A = <div />'
    const firstFileParsedCode = testParseCode(firstFileCode)
    if (!isParseSuccess(firstFileParsedCode)) {
      throw new Error('Code should parse.')
    }

    const secondFileCode = 'export const A = <div />'
    const secondFileParsedCode = testParseCode(secondFileCode)
    if (!isParseSuccess(secondFileParsedCode)) {
      throw new Error('Code should parse.')
    }

    const firstFile = textFile(
      textFileContents(firstFileCode, firstFileParsedCode, RevisionsState.BothMatch),
      null,
      null,
      1,
    )
    const startingProjectContents = addFileToProjectContents({}, '/assets/test1.js', firstFile)

    await runAddHookForProjectChangesTest({
      initialState: startingProjectContents,
      changesToApply: (firstSession, secondSession) => {
        const secondFile = textFile(
          textFileContents(secondFileCode, secondFileParsedCode, RevisionsState.BothMatch),
          null,
          null,
          1,
        )
        const updatedProjectContents = addFileToProjectContents(
          startingProjectContents,
          '/assets/test2.js',
          secondFile,
        )
        const collaborationChanges = collateCollaborativeProjectChanges(
          startingProjectContents,
          updatedProjectContents,
        )
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        const secondFileFixedElements = set(
          elementArrayToSourceMapOptic,
          null,
          removeSourceMaps(secondFileParsedCode.topLevelElements),
        )
        expect(secondSessionPromises).toEqual([
          updateTopLevelElementsFromCollaborationUpdate(
            '/assets/test2.js',
            removeSourceMaps(secondFileFixedElements),
          ),
          updateExportsDetailFromCollaborationUpdate(
            '/assets/test2.js',
            secondFileParsedCode.exportsDetail,
          ),
          updateImportsFromCollaborationUpdate('/assets/test2.js', secondFileParsedCode.imports),
        ])
      },
    })
  })

  it('deleting a file causes it to be removed from the project contents', async () => {
    const firstFileCode = 'export const A = <div />'
    const firstFileParsedCode = testParseCode(firstFileCode)
    if (!isParseSuccess(firstFileParsedCode)) {
      throw new Error('Code should parse.')
    }

    const secondFileCode = 'export const A = <div />'
    const secondFileParsedCode = testParseCode(secondFileCode)
    if (!isParseSuccess(secondFileParsedCode)) {
      throw new Error('Code should parse.')
    }
    const firstFile = textFile(
      textFileContents(firstFileCode, firstFileParsedCode, RevisionsState.BothMatch),
      null,
      null,
      1,
    )
    const secondFile = textFile(
      textFileContents(secondFileCode, secondFileParsedCode, RevisionsState.BothMatch),
      null,
      null,
      1,
    )
    const startingProjectContents = addFileToProjectContents(
      addFileToProjectContents({}, '/assets/test1.js', firstFile),
      '/assets/test2.js',
      secondFile,
    )

    await runAddHookForProjectChangesTest({
      initialState: startingProjectContents,
      changesToApply: (firstSession, secondSession) => {
        const updatedProjectContents = removeFromProjectContents(
          startingProjectContents,
          '/assets/test1.js',
        )
        const collaborationChanges = collateCollaborativeProjectChanges(
          startingProjectContents,
          updatedProjectContents,
        )
        updateCollaborativeProjectContents(firstSession, collaborationChanges, [])
      },
      expectations: (firstSessionPromises, secondSessionPromises) => {
        expect(secondSessionPromises).toEqual([deleteFileFromCollaboration('/assets/test1.js')])
      },
    })
  })
})
