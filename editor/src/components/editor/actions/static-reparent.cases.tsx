/* eslint-disable jest/no-export */
import * as EP from '../../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { selectComponentsForTest } from '../../../utils/utils.test-utils'
import { SelectionLocked } from '../../canvas/canvas-types'
import {
  EditorRenderResult,
  TestAppUID,
  TestSceneUID,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../canvas/ui-jsx.test-utils'
import { navigatorEntryToKey } from '../store/editor-state'
import { toggleHidden, toggleSelectionLock } from './action-creators'

interface StaticReparentTestCase {
  test: (trigger: (_: EditorRenderResult) => Promise<void>) => void
}

function makeTargetPath(suffix: string): ElementPath {
  return EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:${suffix}`)
}

async function setupHiddenTestCase(): Promise<EditorRenderResult> {
  const testCode = `
  <div data-uid='root' style={{contain: 'layout', width: 300, height: 300}}>
    <div data-uid='container'>
      <div data-uid='first' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
      <div data-uid='second' style={{width: 60, height: 60}} />
    </div>
  </div>
`
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(testCode),
    'await-first-dom-report',
  )
  await selectComponentsForTest(renderResult, [makeTargetPath('root/container/first')])

  await renderResult.dispatch([toggleHidden()], false)
  await renderResult.getDispatchFollowUpActionsFinished()
  expect(renderResult.getEditorState().editor.hiddenInstances.map(EP.toString)).toEqual([
    'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first',
  ])

  return renderResult
}

async function setupLockedTest(selectionLock: SelectionLocked): Promise<EditorRenderResult> {
  const testCode = `
  <div data-uid='root' style={{contain: 'layout', width: 300, height: 300}}>
    <div data-uid='container'>
      <div data-uid='first' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
      <div data-uid='second' style={{width: 60, height: 60}} />
    </div>
  </div>
`
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(testCode),
    'await-first-dom-report',
  )

  const targets = [makeTargetPath('root/container/first')]

  await selectComponentsForTest(renderResult, targets)

  await renderResult.dispatch([toggleSelectionLock(targets, selectionLock)], false)
  await renderResult.getDispatchFollowUpActionsFinished()
  return renderResult
}

export const TestCasesToCopyHiddenElements: StaticReparentTestCase[] = [
  {
    test: async (trigger) => {
      // the `it` is here, otherwise jest complains that the `expect`s are not in test blocks
      it('copy/pasting a hidden element preserves hidden status', async () => {
        const renderResult = await setupHiddenTestCase()

        await trigger(renderResult)

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/first',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/second',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/fir',
        ])

        expect(renderResult.getEditorState().editor.hiddenInstances.map(EP.toString)).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first', // <- the copied element stays hidden
          'utopia-storyboard-uid/scene-aaa/app-entity:root/fir', // <- the new element is also hidden
        ])
      })
    },
  },
]

export const TestCasesToCopyLockedElements: StaticReparentTestCase[] = [
  {
    test: async (trigger) => {
      it(`copy-paste preserves simple lock`, async () => {
        const renderResult = await setupLockedTest('locked')
        expect(
          renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
        ).toEqual(['utopia-storyboard-uid/scene-aaa/app-entity:root/container/first'])

        await trigger(renderResult)

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/first',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/second',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/fir',
        ])

        expect(
          renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first', // <- original element stays locked
          'utopia-storyboard-uid/scene-aaa/app-entity:root/fir', // <- new element is also locked
        ])

        expect(
          renderResult.getEditorState().editor.lockedElements.hierarchyLock.map(EP.toString),
        ).toEqual([]) // hierarchy lock doesn't change
      })
    },
  },
  {
    test: async (trigger) => {
      it(`copy-paste preserves hierarchy lock`, async () => {
        const renderResult = await setupLockedTest('locked-hierarchy')
        expect(
          renderResult.getEditorState().editor.lockedElements.hierarchyLock.map(EP.toString),
        ).toEqual(['utopia-storyboard-uid/scene-aaa/app-entity:root/container/first'])

        await trigger(renderResult)

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/first',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/second',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/fir',
        ])

        expect(
          renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
        ).toEqual([]) // simple lock doesn't change

        expect(
          renderResult.getEditorState().editor.lockedElements.hierarchyLock.map(EP.toString),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first', // <- original element stays locked
          'utopia-storyboard-uid/scene-aaa/app-entity:root/fir', // <- new element is also locked
        ])
      })
    },
  },
]

export const TestCasesToMoveHiddenElements: StaticReparentTestCase[] = [
  {
    test: async (trigger) => {
      it('cut/pasting a hidden element preserves hidden status', async () => {
        const renderResult = await setupHiddenTestCase()

        await trigger(renderResult)

        expect(renderResult.getEditorState().editor.hiddenInstances.map(EP.toString)).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first', // <- the cut element stays hidden in `hiddenInstances`, technically not a bug but still
          'utopia-storyboard-uid/scene-aaa/app-entity:root/first', // <- the new element is also hidden
        ])

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/second',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/first',
        ])
      })
    },
  },
]

export const TestCasesToMoveLockedElements: StaticReparentTestCase[] = [
  {
    test: async (trigger) => {
      it(`cut-paste preserves simple lock`, async () => {
        const renderResult = await setupLockedTest('locked')
        expect(
          renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
        ).toEqual(['utopia-storyboard-uid/scene-aaa/app-entity:root/container/first'])

        await trigger(renderResult)

        expect(
          renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first', // <- original element stays locked, even though it's cut
          'utopia-storyboard-uid/scene-aaa/app-entity:root/first', // <- new element is also locked
        ])

        expect(
          renderResult.getEditorState().editor.lockedElements.hierarchyLock.map(EP.toString),
        ).toEqual([]) // hierarchy lock doesn't change

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/second',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/first',
        ])
      })
    },
  },
  {
    test: async (trigger) => {
      it(`cut-paste preserves hierarchy lock`, async () => {
        const renderResult = await setupLockedTest('locked-hierarchy')
        expect(
          renderResult.getEditorState().editor.lockedElements.hierarchyLock.map(EP.toString),
        ).toEqual(['utopia-storyboard-uid/scene-aaa/app-entity:root/container/first'])

        await trigger(renderResult)

        expect(
          renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
        ).toEqual([]) // simple lock doesn't change

        expect(
          renderResult.getEditorState().editor.lockedElements.hierarchyLock.map(EP.toString),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:root/container/first', // <- original element stays locked, even though it's cut
          'utopia-storyboard-uid/scene-aaa/app-entity:root/first', // <- new element is locked
        ])

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/second',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/first',
        ])
      })
    },
  },
]
