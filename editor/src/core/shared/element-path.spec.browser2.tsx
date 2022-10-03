import * as EP from './element-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import { setLastProjectContentsForTesting } from '../../components/editor/store/dispatch'

describe('ElementPath Caching', () => {
  let originalRequestIdleCallback: (
    callback: IdleRequestCallback,
    options?: IdleRequestOptions,
  ) => number
  let fakeIdle: () => void = () => {
    throw new Error(`fakeIdle called too early`)
  }

  before(() => {
    originalRequestIdleCallback = window.requestIdleCallback

    window.requestIdleCallback = (
      callback: IdleRequestCallback,
      _options?: IdleRequestOptions,
    ): number => {
      fakeIdle = () => callback({} as IdleDeadline)
      return 1
    }
  })

  after(() => {
    window.requestIdleCallback = originalRequestIdleCallback
  })

  it('Culls the cached element paths when idle', async () => {
    // Add 2 paths to the cache
    const realPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const fakePath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])

    // Rendering alone will be enough to trigger the timer for culling the cache
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div data-uid='aaa'>
        <div data-uid='bbb' />
      </div>
    `),
      'await-first-dom-report',
    )

    // let's make sure the paths are currently still in the cache
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])).toBe(realPath)
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])).toBe(fakePath)

    setLastProjectContentsForTesting(renderResult.getEditorState().editor.projectContents)
    fakeIdle()

    // Now ensure only the fake path has been culled
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])).toBe(realPath)
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])).not.toBe(fakePath)
  })
})
