import * as EP from './element-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import {
  cullElementPathCache,
  killElementPathCacheCallback,
  setLastProjectContentsForTesting,
} from '../../components/editor/store/dispatch'

describe('ElementPath Caching', () => {
  it('Culls the cached element paths when idle', async () => {
    killElementPathCacheCallback()
    // Add 2 paths to the cache
    const realPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const fakePath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])

    // let's make sure the paths are currently still in the cache
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])).toBe(realPath)
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])).toBe(fakePath)

    // Rendering alone will usually be enough to trigger the timer for culling the cache.
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div data-uid='aaa'>
        <div data-uid='bbb' />
      </div>
    `),
      'await-first-dom-report',
    )
    setLastProjectContentsForTesting(renderResult.getEditorState().editor.projectContents)
    cullElementPathCache()

    // Now ensure only the fake path has been culled
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])).toBe(realPath)
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])).not.toBe(fakePath)
  })
})
