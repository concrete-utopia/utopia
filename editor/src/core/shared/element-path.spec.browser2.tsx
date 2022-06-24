import sinon, { SinonFakeTimers } from 'sinon'
import * as EP from './element-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import { CullElementPathCacheTimeout } from '../../components/editor/actions/actions'

describe('ElementPath Caching', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  let clock: SinonFakeTimers
  beforeEach(function () {
    clock = sinon.useFakeTimers({
      // the timers will tick so the editor is not totally broken, but we can fast-forward time at will
      // WARNING: the Sinon fake timers will advance in 20ms increments
      shouldAdvanceTime: true,
    })
  })
  afterEach(function () {
    clock.restore()
  })

  it('Culls the cached element paths when idle', async () => {
    // Add 2 paths to the cache
    const realPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const fakePath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])

    // Rendering alone will be enough to trigger the timer for culling the cache
    await renderTestEditorWithCode(
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

    // Let's do the time warp
    clock.tick(CullElementPathCacheTimeout)

    // Now ensure only the fake path has been culled
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])).toBe(realPath)
    expect(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])).not.toBe(fakePath)
  })
})
